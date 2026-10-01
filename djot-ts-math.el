;;; djot-ts-math.el --- Math previews for Djot -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: text, tex

;;; Commentary:

;; `djot-ts-math-mode' shows Djot math ($`...` and $$`...`) as images
;; and shows the source while point is on it.  Edited fragments are
;; re-rendered when point leaves them.
;;
;; Rendering is done by a backend, see `djot-ts-math-define-backend'.
;; The `org' backend (djot-ts-math-org.el) uses `org-latex-preview'.

;;; Code:

(require 'treesit)
(require 'seq)

(defgroup djot-ts-math nil
  "Math previews for Djot."
  :group 'djot-ts)

(defcustom djot-ts-math-backend 'org
  "Backend used by `djot-ts-math-mode'.
A backend NAME is registered with `djot-ts-math-define-backend', or
by the library djot-ts-math-NAME, loaded on demand."
  :type 'symbol)

(defcustom djot-ts-math-center-display t
  "Non-nil means center the images of display math."
  :type 'boolean)

(defcustom djot-ts-math-display-own-line t
  "Non-nil means show the images of display math on their own line.
Applies to display math inside a paragraph: text before or after it on
the same line is shown on the previous, resp. next line.  The buffer
text is not changed."
  :type 'boolean)

(defcustom djot-ts-math-live-preview t
  "Non-nil means preview the math being edited next to its source.
Inline math is previewed after its closing delimiter, display math on
the next line."
  :type 'boolean)

(defcustom djot-ts-math-live-delay 0.3
  "Seconds without edits before the math being edited is re-rendered."
  :type 'number)

(defvar djot-ts-math--backends nil
  "Alist of (NAME . PLIST), see `djot-ts-math-define-backend'.")

(defun djot-ts-math-define-backend (name &rest plist)
  "Register backend NAME with the functions in PLIST.

:place ENTRIES   Required.  Preview each entry (BEG END TEX DISPLAY) of
                 ENTRIES with an overlay over BEG..END, reusing an
                 existing preview with the same bounds.  TEX is the
                 source without delimiters, DISPLAY is non-nil for
                 display math.  The overlays must exist when :place
                 returns; the images may arrive later.
:overlay-p OV    Required.  Non-nil if OV is a preview overlay.
:reveal OV       Required.  Show the source under OV.  It must stay
                 shown if OV's image arrives later, also when OV is
                 placed again (live preview of the math being edited).
:conceal OV      Required.  Show OV's image.
:image OV        Optional.  Return OV's image spec, or nil if it has
                 none yet.  Needed to center and break display math.
:setup           Optional.  Called in the buffer when the mode is
:teardown        enabled, resp. disabled.

Previews are removed with `delete-overlay'.  A backend whose images
arrive asynchronously should call `djot-ts-math-image-updated' on
the overlay when they do."
  (dolist (k '(:place :overlay-p :reveal :conceal))
    (unless (functionp (plist-get plist k))
      (error "Backend %s: %s must be a function" name k)))
  (setf (alist-get name djot-ts-math--backends) plist))

(defvar djot-ts-math-mode)

(defvar-local djot-ts-math--backend nil
  "Plist of the backend used in this buffer.")

(defvar-local djot-ts-math--parser nil
  "The Djot parser whose notifications are tracked.")

(defvar-local djot-ts-math--dirty nil
  "(BEG . END) markers of the region to re-render, or nil.")

(defvar-local djot-ts-math--revealed nil
  "Preview overlays whose source is shown.")

(defvar-local djot-ts-math--layouts nil
  "Layout overlays, see `djot-ts-math--layout'.")

(defvar-local djot-ts-math--live-watch nil
  "Overlay over the math being edited; its changes trigger re-rendering.")

(defvar-local djot-ts-math--live-show nil
  "Empty overlay showing the live preview.")

(defvar-local djot-ts-math--live-timer nil
  "Debounce timer for re-rendering the math being edited.")

(defvar-local djot-ts-math--live-ov nil
  "Preview overlay of the math being edited, or nil.")

(defvar djot-ts-math--query nil
  "Compiled query for math nodes.")

(defun djot-ts-math--get-backend (name)
  "Return the plist of backend NAME, loading it if needed."
  (or (alist-get name djot-ts-math--backends)
      (and (require (intern (format "djot-ts-math-%s" name)) nil t)
           (alist-get name djot-ts-math--backends))
      (user-error "Unknown djot-ts-math backend: %s" name)))

(defun djot-ts-math--call (key &rest args)
  "Call the function KEY of the current backend with ARGS."
  (when-let* ((f (plist-get djot-ts-math--backend key)))
    (apply f args)))

(defun djot-ts-math--fragments (&optional beg end)
  "Return entries (BEG END TEX DISPLAY) for the math between BEG and END."
  (unless djot-ts-math--query
    (setq djot-ts-math--query (treesit-query-compile 'djot '((math) @m))))
  (let (res)
    (dolist (c (treesit-query-capture
                (treesit-parser-root-node djot-ts-math--parser)
                djot-ts-math--query beg end))
      (let* ((node (cdr c))
             (content (treesit-node-child-by-field-name node "content"))
             (marker (treesit-node-child-by-field-name node "math_marker"))
             (tex (and content (treesit-node-text content t))))
        (unless (or (null tex) (string-blank-p tex))
          (push (list (treesit-node-start node) (treesit-node-end node) tex
                      (equal (treesit-node-text marker t) "$$"))
                res))))
    (nreverse res)))

(defun djot-ts-math--overlays (&optional beg end)
  "Return the preview overlays between BEG and END."
  (seq-filter (lambda (o) (djot-ts-math--call :overlay-p o))
              (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun djot-ts-math--overlay (beg end)
  "Return the preview overlay spanning exactly BEG..END."
  (seq-find (lambda (o) (and (= beg (overlay-start o)) (= end (overlay-end o))))
            (djot-ts-math--overlays beg end)))

(defun djot-ts-math--in-paragraph-p (pos)
  "Non-nil if the math starting at POS is inside a paragraph."
  (when-let* ((leaf (treesit-node-at pos djot-ts-math--parser))
              (math (treesit-parent-until
                     leaf (lambda (n) (equal (treesit-node-type n) "math")) t)))
    (treesit-parent-until
     math (lambda (n) (equal (treesit-node-type n) "paragraph")))))

(defun djot-ts-math--center-space (img)
  "Return a space that centers IMG following it."
  (propertize " " 'face 'default
              'display `(space :align-to (- center (0.55 . ,img)))))

(defun djot-ts-math--layout-strings (ov)
  "Return (BEFORE . AFTER) strings laying out the preview OV, or nil.
Only display math with an image is laid out."
  (when-let* (((nth 1 (overlay-get ov 'djot-ts-math-source)))
              (img (djot-ts-math--call :image ov)))
    (let* ((beg (overlay-start ov))
           (end (overlay-end ov))
           (inline (djot-ts-math--in-paragraph-p beg))
           (alone-before (or (not inline)
                             (save-excursion (goto-char beg)
                                             (skip-chars-backward " \t")
                                             (bolp))))
           (alone-after (or (not inline)
                            (save-excursion (goto-char end)
                                            (skip-chars-forward " \t")
                                            (eolp))))
           (break djot-ts-math-display-own-line)
           (before (concat
                    (and break (not alone-before) "\n")
                    (and djot-ts-math-center-display (or break alone-before)
                         (djot-ts-math--center-space img))))
           (after (and break (not alone-after) "\n")))
      (unless (and (string-empty-p before) (null after))
        (cons (and (not (string-empty-p before)) before) after)))))

(defun djot-ts-math--unlayout (ov)
  "Remove the layout of the preview OV."
  (when-let* ((l (overlay-get ov 'djot-ts-math-layout)))
    (delete-overlay l)
    (setq djot-ts-math--layouts (delq l djot-ts-math--layouts))
    (overlay-put ov 'djot-ts-math-layout nil)))

(defun djot-ts-math--layout (ov)
  "Center display math OV and put it on its own line, per the options.
This uses a separate overlay, so that the backend's overlay
properties are left alone."
  (djot-ts-math--unlayout ov)
  (when-let* ((s (djot-ts-math--layout-strings ov))
              (l (make-overlay (overlay-start ov) (overlay-end ov))))
    (overlay-put l 'evaporate t)
    (overlay-put l 'djot-ts-math-preview ov)
    (overlay-put l 'before-string (car s))
    (overlay-put l 'after-string (cdr s))
    (overlay-put ov 'djot-ts-math-layout l)
    (push l djot-ts-math--layouts)))

(defun djot-ts-math--gc-layouts ()
  "Delete the layouts of deleted previews."
  (dolist (l djot-ts-math--layouts)
    (unless (overlay-buffer (overlay-get l 'djot-ts-math-preview))
      (delete-overlay l)
      (setq djot-ts-math--layouts (delq l djot-ts-math--layouts)))))

(defun djot-ts-math-image-updated (ov)
  "Update the layout of the preview OV after its image changed.
Backends call this when an image arrives, see
`djot-ts-math-define-backend'."
  (when-let* ((buf (overlay-buffer ov)))
    (with-current-buffer buf
      (when djot-ts-math-mode
        (cond ((eq ov djot-ts-math--live-ov) (djot-ts-math--live-display))
              ((not (memq ov djot-ts-math--revealed))
               (djot-ts-math--layout ov)))))))

(defun djot-ts-math--place (entries)
  "Preview ENTRIES, skipping those whose preview is up to date."
  (when-let* ((todo (seq-remove
                     (lambda (e)
                       (when-let* ((ov (djot-ts-math--overlay (nth 0 e) (nth 1 e))))
                         (equal (overlay-get ov 'djot-ts-math-source) (cddr e))))
                     entries)))
    (djot-ts-math--call :place todo)
    (dolist (e todo)
      (when-let* ((ov (djot-ts-math--overlay (nth 0 e) (nth 1 e))))
        (overlay-put ov 'djot-ts-math-source (cddr e))
        (unless (memq ov djot-ts-math--revealed)
          (djot-ts-math--layout ov))))))

(defun djot-ts-math--mark-dirty (beg end)
  "Add BEG..END to the region to re-render."
  (let ((beg (max beg (point-min)))
        (end (min end (point-max))))
    (if-let* ((d djot-ts-math--dirty))
        (progn (when (< beg (car d)) (set-marker (car d) beg))
               (when (> end (cdr d)) (set-marker (cdr d) end)))
      (setq djot-ts-math--dirty (cons (copy-marker beg) (copy-marker end t))))))

(defun djot-ts-math--after-change (beg end _len)
  "Mark BEG..END for re-rendering."
  (djot-ts-math--mark-dirty beg end))

(defun djot-ts-math--notifier (ranges _parser)
  "Mark the RANGES whose syntax tree changed for re-rendering."
  (dolist (r ranges)
    (djot-ts-math--mark-dirty (car r) (cdr r))))

(defun djot-ts-math--refresh (pt)
  "Re-render the math in the dirty region, except the fragment at PT.
That fragment stays dirty."
  (let* ((beg (car djot-ts-math--dirty))
         (end (cdr djot-ts-math--dirty))
         (frags (djot-ts-math--fragments beg end))
         (here (seq-find (lambda (f) (and (<= (nth 0 f) pt) (< pt (nth 1 f))))
                         frags)))
    (dolist (ov (djot-ts-math--overlays beg end))
      (unless (seq-find (lambda (f) (and (= (nth 0 f) (overlay-start ov))
                                         (= (nth 1 f) (overlay-end ov))))
                        frags)
        (djot-ts-math--unlayout ov)
        (delete-overlay ov)))
    (djot-ts-math--place (remq here frags))
    (if here
        (progn (set-marker beg (nth 0 here))
               (set-marker end (nth 1 here)))
      (set-marker beg nil)
      (set-marker end nil)
      (setq djot-ts-math--dirty nil))))

(defun djot-ts-math--update-reveal (pt)
  "Reveal the previews at PT and conceal the others."
  (let ((new (seq-filter (lambda (o) (djot-ts-math--call :overlay-p o))
                         (overlays-at pt))))
    (dolist (o djot-ts-math--revealed)
      (when (and (overlay-buffer o) (not (memq o new)))
        (djot-ts-math--call :conceal o)
        (djot-ts-math--layout o)))
    (dolist (o new)
      (unless (memq o djot-ts-math--revealed)
        (djot-ts-math--unlayout o)
        (djot-ts-math--call :reveal o)))
    (setq djot-ts-math--revealed new)))

;;;; Live preview of the math being edited

(defun djot-ts-math--fragment-at (pt)
  "Return the entry of the math containing PT, or nil.
As for overlays, the math's end position is not contained."
  (seq-find (lambda (f) (and (<= (nth 0 f) pt) (< pt (nth 1 f))))
            (djot-ts-math--fragments (max (point-min) (1- pt))
                                     (min (point-max) (1+ pt)))))

(defun djot-ts-math--live-string (img display end)
  "Return the string showing IMG after math ending at END.
DISPLAY non-nil means display math, shown on its own line."
  (let ((s (propertize " " 'display img)))
    (if (not display)
        s
      (concat "\n"
              (and djot-ts-math-center-display (djot-ts-math--center-space img))
              s
              (and (save-excursion (goto-char end)
                                   (skip-chars-forward " \t")
                                   (not (eolp)))
                   "\n")))))

(defun djot-ts-math--live-display ()
  "Show the image of `djot-ts-math--live-ov' in the live preview.
Keep the previous image if there is none yet."
  (when-let* ((show djot-ts-math--live-show)
              ((overlay-buffer show))
              (ov djot-ts-math--live-ov)
              ((overlay-buffer ov))
              (img (djot-ts-math--call :image ov)))
    (overlay-put show 'after-string
                 (djot-ts-math--live-string
                  img (overlay-get show 'djot-ts-math-display)
                  (overlay-start show)))))

(defun djot-ts-math--live-render (buf)
  "Render the math at point in BUF, keeping its source shown."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (setq djot-ts-math--live-timer nil)
      (when djot-ts-math-mode
        (with-demoted-errors "djot-ts-math: %S"
          (treesit-parser-root-node djot-ts-math--parser)
          (when-let* ((f (djot-ts-math--fragment-at (point))))
            (djot-ts-math--place (list f))
            (when-let* ((ov (djot-ts-math--overlay (nth 0 f) (nth 1 f))))
              (unless (memq ov djot-ts-math--revealed)
                (djot-ts-math--unlayout ov)
                (djot-ts-math--call :reveal ov)
                (push ov djot-ts-math--revealed))
              (setq djot-ts-math--live-ov ov)
              (djot-ts-math--live-display))))))))

(defun djot-ts-math--live-schedule ()
  "Re-render the math at point after `djot-ts-math-live-delay'."
  (when (timerp djot-ts-math--live-timer)
    (cancel-timer djot-ts-math--live-timer))
  (setq djot-ts-math--live-timer
        (run-with-timer djot-ts-math-live-delay nil
                        #'djot-ts-math--live-render (current-buffer))))

(defun djot-ts-math--live-changed (_ov after &rest _)
  "Overlay modification hook: debounce re-rendering if AFTER."
  (when after
    (djot-ts-math--live-schedule)))

(defun djot-ts-math--live-stop ()
  "Remove the live preview."
  (when (timerp djot-ts-math--live-timer)
    (cancel-timer djot-ts-math--live-timer))
  (when djot-ts-math--live-watch (delete-overlay djot-ts-math--live-watch))
  (when djot-ts-math--live-show (delete-overlay djot-ts-math--live-show))
  (setq djot-ts-math--live-timer nil
        djot-ts-math--live-watch nil
        djot-ts-math--live-show nil
        djot-ts-math--live-ov nil))

(defun djot-ts-math--live-update (pt)
  "Track the math at PT for the live preview."
  (if-let* ((f (and djot-ts-math-live-preview (djot-ts-math--fragment-at pt))))
      (pcase-let* ((`(,beg ,end ,tex ,display) f)
                   (watch djot-ts-math--live-watch)
                   (show djot-ts-math--live-show)
                   (ov (or (djot-ts-math--overlay beg end)
                           (seq-find (lambda (o) (djot-ts-math--call :overlay-p o))
                                     (overlays-at pt)))))
        (if (and watch (overlay-buffer watch))
            (move-overlay watch beg end)
          (setq watch (make-overlay beg end nil t))
          (overlay-put watch 'modification-hooks '(djot-ts-math--live-changed))
          (setq djot-ts-math--live-watch watch))
        (if (and show (overlay-buffer show))
            (move-overlay show end end)
          (setq djot-ts-math--live-show (make-overlay end end)))
        (overlay-put djot-ts-math--live-show 'djot-ts-math-display display)
        (unless (eq ov djot-ts-math--live-ov)
          (overlay-put djot-ts-math--live-show 'after-string nil)
          (setq djot-ts-math--live-ov ov))
        (djot-ts-math--live-display)
        ;; New math, or math changed before it was tracked.
        (unless (or djot-ts-math--live-timer
                    (and ov (equal (overlay-get ov 'djot-ts-math-source)
                                   (list tex display))))
          (djot-ts-math--live-schedule)))
    (djot-ts-math--live-stop)))

(defun djot-ts-math--post-command ()
  "Re-render edited math and update the revealed previews."
  (with-demoted-errors "djot-ts-math: %S"
    ;; Reparse now, so that the notifier runs before the refresh.
    (treesit-parser-root-node djot-ts-math--parser)
    (djot-ts-math--gc-layouts)
    (when djot-ts-math--dirty
      (djot-ts-math--refresh (point)))
    (djot-ts-math--update-reveal (point))
    (djot-ts-math--live-update (point))))

;;;###autoload
(define-minor-mode djot-ts-math-mode
  "Show Djot math as images, and the source at point.
The images are made by `djot-ts-math-backend'."
  :lighter nil
  (if djot-ts-math-mode
      (let ((parser (car (treesit-parser-list nil 'djot))))
        (unless parser
          (setq djot-ts-math-mode nil)
          (user-error "No Djot parser in this buffer"))
        (setq djot-ts-math--backend (djot-ts-math--get-backend djot-ts-math-backend)
              djot-ts-math--parser parser)
        (djot-ts-math--call :setup)
        (treesit-parser-add-notifier parser #'djot-ts-math--notifier)
        (add-hook 'after-change-functions #'djot-ts-math--after-change nil t)
        (add-hook 'post-command-hook #'djot-ts-math--post-command nil t)
        (djot-ts-math--place (djot-ts-math--fragments))
        (djot-ts-math--update-reveal (point))
        (djot-ts-math--live-update (point)))
    (djot-ts-math--live-stop)
    (remove-hook 'after-change-functions #'djot-ts-math--after-change t)
    (remove-hook 'post-command-hook #'djot-ts-math--post-command t)
    (when (treesit-parser-p djot-ts-math--parser)
      (treesit-parser-remove-notifier djot-ts-math--parser #'djot-ts-math--notifier))
    (mapc #'delete-overlay djot-ts-math--layouts)
    (when djot-ts-math--backend
      (mapc #'delete-overlay (djot-ts-math--overlays))
      (djot-ts-math--call :teardown))
    (setq djot-ts-math--backend nil
          djot-ts-math--parser nil
          djot-ts-math--dirty nil
          djot-ts-math--layouts nil
          djot-ts-math--revealed nil)))

(provide 'djot-ts-math)
;;; djot-ts-math.el ends here
