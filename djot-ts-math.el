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
                 shown if OV's image arrives later.
:conceal OV      Required.  Show OV's image.
:setup           Optional.  Called in the buffer when the mode is
:teardown        enabled, resp. disabled.

Previews are removed with `delete-overlay'."
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
        (overlay-put ov 'djot-ts-math-source (cddr e))))))

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
         (here (seq-find (lambda (f) (<= (nth 0 f) pt (nth 1 f))) frags)))
    (dolist (ov (djot-ts-math--overlays beg end))
      (unless (seq-find (lambda (f) (and (= (nth 0 f) (overlay-start ov))
                                         (= (nth 1 f) (overlay-end ov))))
                        frags)
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
        (djot-ts-math--call :conceal o)))
    (dolist (o new)
      (unless (memq o djot-ts-math--revealed)
        (djot-ts-math--call :reveal o)))
    (setq djot-ts-math--revealed new)))

(defun djot-ts-math--post-command ()
  "Re-render edited math and update the revealed previews."
  (with-demoted-errors "djot-ts-math: %S"
    ;; Reparse now, so that the notifier runs before the refresh.
    (treesit-parser-root-node djot-ts-math--parser)
    (when djot-ts-math--dirty
      (djot-ts-math--refresh (point)))
    (djot-ts-math--update-reveal (point))))

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
        (djot-ts-math--update-reveal (point)))
    (remove-hook 'after-change-functions #'djot-ts-math--after-change t)
    (remove-hook 'post-command-hook #'djot-ts-math--post-command t)
    (when (treesit-parser-p djot-ts-math--parser)
      (treesit-parser-remove-notifier djot-ts-math--parser #'djot-ts-math--notifier))
    (when djot-ts-math--backend
      (mapc #'delete-overlay (djot-ts-math--overlays))
      (djot-ts-math--call :teardown))
    (setq djot-ts-math--backend nil
          djot-ts-math--parser nil
          djot-ts-math--dirty nil
          djot-ts-math--revealed nil)))

(provide 'djot-ts-math)
;;; djot-ts-math.el ends here
