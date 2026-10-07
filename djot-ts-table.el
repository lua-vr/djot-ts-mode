;;; djot-ts-table.el --- Pixel-aligned Djot pipe tables -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: text

;;; Commentary:

;; `djot-ts-table-mode' aligns the columns of Djot pipe tables by the
;; pixel width of their cells, honoring the column alignments of the
;; separator lines.  Only the display changes: padding is shown as
;; stretches, separator lines are extended with dashes.
;;
;; Tables are aligned from `jit-lock-functions', after font-lock, so
;; that faces and invisible text (e.g. of `djot-ts-appear-mode') are
;; measured as displayed.  Whatever refontifies a table realigns it.
;; Alignment uses overlays, which font-lock leaves alone.
;;
;; Tables wider than the window are shrunk to fit: the widest columns
;; are cut to a common width, ending in `djot-ts-table-ellipsis'.  The
;; cell at point is shown in full, pushing the rest of its row to the
;; right; the full text of a cut cell is also its tooltip.

;;; Code:

(require 'treesit)
(require 'seq)

(defvar-local djot-ts-table--query nil
  "Compiled query for table nodes.")

(defvar-local djot-ts-table--current nil
  "(TBEG TEND (CBEG . CEND)): bounds of the table and the cell at point.
Nil if point is not in a content cell.")

(defconst djot-ts-table-ellipsis "…"
  "String shown in place of the cut end of a shrunk cell.")

(defun djot-ts-table--width (from to win)
  "Return the pixel width of the text from FROM to TO, as shown in WIN.
WIN nil means no window shows the buffer; then text properties are
measured, but not overlays and line prefixes."
  (cond ((>= from to) 0)
        (win (car (window-text-pixel-size win from to t)))
        (t (string-pixel-width (buffer-substring from to) (current-buffer)))))

(defun djot-ts-table--alignment (node)
  "Return the alignment of the separator cell NODE.
This is `left', `right', `center' or nil for the default."
  (let ((s (treesit-node-text node t)))
    (pcase (cons (string-prefix-p ":" s) (string-suffix-p ":" s))
      ('(t . nil) 'left)
      ('(nil . t) 'right)
      ('(t . t) 'center))))

(defun djot-ts-table--measure-row (row win)
  "Measure ROW in WIN.
Return a plist (:sep :pipe :x0 :cells), or nil for a malformed row.
:x0 is the pixel position after the opening pipe, :pipe the width of
a pipe.  Each cell is a plist with the positions :beg, :end and the
pixel width :need it requires between its pipes.  Content cells also
have :cbeg, :cend and the content width :w; separator cells have
:natural, :dash (position of a `-') and :dashw."
  (let* ((rbeg (treesit-node-start row))
         (sep (equal (treesit-node-type row) "table_separator"))
         (pad (* 2 (djot-ts-table--space-width win)))
         (cells (treesit-node-children row t)))
    (when (and cells (eq (char-after rbeg) ?|))
      (list
       :sep sep
       :x0 (djot-ts-table--width (save-excursion (goto-char rbeg) (pos-bol))
                                 (1+ rbeg) win)
       :pipe (let ((e (treesit-node-end (car cells))))
               (djot-ts-table--width e (1+ e) win))
       :cells
       (mapcar
        (lambda (c)
          (let ((beg (treesit-node-start c))
                (end (treesit-node-end c)))
            (if sep
                (let* ((natural (djot-ts-table--width beg end win))
                       (dash (save-excursion
                               (goto-char beg)
                               (skip-chars-forward ":" end)
                               (point))))
                  (list :beg beg :end end :need natural :natural natural
                        :align (djot-ts-table--alignment c)
                        :dash dash
                        :dashw (max 1 (djot-ts-table--width dash (1+ dash) win))))
              (let* ((cbeg (save-excursion
                             (goto-char beg) (skip-chars-forward " \t" end) (point)))
                     (cend (save-excursion
                             (goto-char end) (skip-chars-backward " \t" cbeg) (point)))
                     (w (djot-ts-table--width cbeg cend win)))
                (list :beg beg :end end :cbeg cbeg :cend cend
                      :w w :need (+ w pad))))))
        cells)))))

(defun djot-ts-table--space-width (win)
  "Return the pixel width of the padding around cell contents in WIN."
  (if win
      (window-font-width win)
    (string-pixel-width " " (current-buffer))))

(defun djot-ts-table--overlay (beg end prop value)
  "Make a table overlay from BEG to END with PROP set to VALUE."
  (let ((ov (make-overlay beg end nil t nil)))
    (overlay-put ov 'djot-ts-table t)
    (overlay-put ov prop value)
    ov))

(defun djot-ts-table--space (beg end x)
  "Show the text from BEG to END as space up to pixel X.
If BEG = END, insert that space at BEG instead."
  (let ((spec `(space :align-to (,(round x)))))
    (if (< beg end)
        (overlay-put (djot-ts-table--overlay beg end 'display spec) 'evaporate t)
      (djot-ts-table--overlay beg beg 'before-string
                              (propertize " " 'display spec)))))

(defun djot-ts-table--remove (beg end)
  "Delete the table overlays between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'djot-ts-table)
      (delete-overlay ov))))

(defun djot-ts-table--rows (table win)
  "Return the measured rows of TABLE in WIN, with their alignments.
Each row gets an :aligns list, see `djot-ts-table--alignment'."
  (let (rows aligns)
    (dolist (node (treesit-node-children table t))
      (when-let* (((member (treesit-node-type node)
                           '("table_header" "table_row" "table_separator")))
                  (row (djot-ts-table--measure-row node win)))
        (when (plist-get row :sep)
          (setq aligns (mapcar (lambda (c) (plist-get c :align))
                               (plist-get row :cells)))
          ;; The row before a separator is a header aligned by it.
          (when-let* ((prev (car rows))
                      ((not (plist-get prev :sep))))
            (setcar rows (plist-put prev :aligns aligns))))
        (push (plist-put row :aligns aligns) rows)))
    (nreverse rows)))

(defun djot-ts-table--pipes (rows ncols &optional cap)
  "Return a vector of the pixel positions of the pipes after each column.
With CAP, content cells take at most CAP pixels.  A cell with
:expanded counts as that many pixels wide."
  (let ((pipes (make-vector ncols 0))
        (starts (mapcar (lambda (r) (plist-get r :x0)) rows)))
    (dotimes (j ncols)
      (seq-mapn (lambda (r x)
                  (when-let* ((c (nth j (plist-get r :cells))))
                    (let ((need (or (plist-get c :expanded) (plist-get c :need))))
                      (when (and cap (not (plist-get r :sep)))
                        (setq need (min need cap)))
                      (aset pipes j (max (aref pipes j) (+ x need))))))
                rows starts)
      (setq starts (mapcar (lambda (r) (+ (aref pipes j) (plist-get r :pipe)))
                           rows)))
    pipes))

(defun djot-ts-table--cap (rows ncols win s)
  "Return the widest content cell width fitting ROWS in WIN, or nil.
Nil means the table fits unshrunk.  S is the width of a space.  The
cap is at least two characters plus padding."
  (let* ((avail (- (window-body-width win t) s
                   (if display-line-numbers (line-number-display-width t) 0)))
         (pipe (seq-max (mapcar (lambda (r) (plist-get r :pipe)) rows)))
         (fits (lambda (cap)
                 (<= (+ (aref (djot-ts-table--pipes rows ncols cap) (1- ncols))
                        pipe)
                     avail))))
    (unless (funcall fits nil)
      (let ((lo (* 4 s))
            (hi (seq-max (mapcar (lambda (r) (if (plist-get r :sep) 0
                                               (seq-max (mapcar (lambda (c) (plist-get c :need))
                                                                (plist-get r :cells)))))
                                 rows))))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi 1) 2)))
            (if (funcall fits mid) (setq lo mid) (setq hi (1- mid)))))
        lo))))

(defun djot-ts-table--shrink (cell width win ew &optional measure)
  "Cut content CELL to show at most WIDTH pixels in WIN.
EW is the width of `djot-ts-table-ellipsis'.  Update the widths of
CELL to the shrunk ones.  Return the shrunk :need.  With MEASURE,
only return it."
  (let* ((beg (plist-get cell :cbeg))
         (end (plist-get cell :cend))
         (lo beg)
         (hi end))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (+ (djot-ts-table--width beg mid win) ew) width)
            (setq lo mid)
          (setq hi (1- mid)))))
    (let* ((w (+ (djot-ts-table--width beg lo win) ew))
           (need (+ (plist-get cell :need) (- w (plist-get cell :w)))))
      (unless measure
        (let ((ov (djot-ts-table--overlay lo end 'display djot-ts-table-ellipsis)))
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'help-echo (buffer-substring-no-properties beg end)))
        (plist-put cell :need need)
        (plist-put cell :w w))
      need)))

(defun djot-ts-table--align (table)
  "Align the pipe table node TABLE.
Shrink it to fit its window, except for the cell in
`djot-ts-table--current', which pushes the rest of its row to the
right."
  (let* ((tbeg (treesit-node-start table))
         (tend (treesit-node-end table))
         ;; Not point: under jit-lock, point is wherever earlier
         ;; fontification functions left it.
         (pt (car (nth 2 djot-ts-table--current)))
         (win (get-buffer-window (current-buffer) t))
         (s (djot-ts-table--space-width win))
         rows ncols cap pipes)
    ;; Measure without our overlays.
    (djot-ts-table--remove tbeg tend)
    (setq rows (djot-ts-table--rows table win))
    (setq ncols (seq-max (cons 0 (mapcar (lambda (r) (length (plist-get r :cells)))
                                         rows))))
    (when (and win (> ncols 0))
      (setq cap (djot-ts-table--cap rows ncols win s)))
    (when cap
      (let ((ew (string-pixel-width djot-ts-table-ellipsis (current-buffer))))
        (dolist (r rows)
          (unless (plist-get r :sep)
            (dolist (c (plist-get r :cells))
              (when (> (plist-get c :need) cap)
                (if (and pt (<= (plist-get c :beg) pt (plist-get c :end)))
                    (plist-put c :expanded
                               (djot-ts-table--shrink c (- cap (* 2 s)) win ew t))
                  (djot-ts-table--shrink c (- cap (* 2 s)) win ew))))))))
    ;; Column J ends at pixel (aref pipes J), where the next pipe starts.
    (setq pipes (djot-ts-table--pipes rows ncols))
    (dolist (r rows)
      ;; SHIFT is how far an expanded cell pushed the rest of the row.
      (let ((x (plist-get r :x0)) (j 0) (shift 0))
        (dolist (c (plist-get r :cells))
          (let ((pipe (+ (aref pipes j) shift))
                (align (nth j (plist-get r :aligns))))
            (when (> (+ x (plist-get c :need)) pipe)
              (setq shift (+ shift (- (+ x (plist-get c :need)) pipe))
                    pipe (+ x (plist-get c :need))))
            (if (plist-get r :sep)
                (let* ((dashw (plist-get c :dashw))
                       (n (floor (- pipe x (plist-get c :natural)) dashw))
                       (last (1- (plist-get c :end))))
                  (when (> n 0)
                    (djot-ts-table--overlay
                     last last 'before-string
                     (propertize (make-string n ?-)
                                 'face (get-text-property (plist-get c :dash) 'face)))))
              (let* ((free (- pipe x (plist-get c :need)))
                     (cx (+ x s (pcase align
                                  ('right free)
                                  ('center (/ free 2.0))
                                  (_ 0)))))
                (djot-ts-table--space (plist-get c :beg) (plist-get c :cbeg) cx)))
            (djot-ts-table--space (if (plist-get r :sep)
                                      (plist-get c :end)
                                    (plist-get c :cend))
                                  (plist-get c :end) pipe)
            (setq x (+ pipe (plist-get r :pipe))
                  j (1+ j))))))))

(defun djot-ts-table--tables (beg end)
  "Return the table nodes intersecting BEG..END."
  (unless djot-ts-table--query
    (setq djot-ts-table--query (treesit-query-compile 'djot '((table) @t))))
  (when-let* ((parser (car (treesit-parser-list nil 'djot))))
    (treesit-query-capture (treesit-parser-root-node parser)
                           djot-ts-table--query beg end t)))

(defun djot-ts-table--align-region (beg end)
  "Align the tables intersecting BEG..END.
Also delete the table overlays left there by removed tables."
  (with-demoted-errors "djot-ts-table: %S"
    (save-excursion
      (save-restriction
        (widen)
        (djot-ts-table--remove beg end)
        (mapc #'djot-ts-table--align (djot-ts-table--tables beg end))))))

(defun djot-ts-table--at (pos)
  "Return the table node containing POS, or nil."
  (seq-find (lambda (tb) (<= (treesit-node-start tb) pos (treesit-node-end tb)))
            (djot-ts-table--tables (max (point-min) (1- pos))
                                   (min (point-max) (1+ pos)))))

(defun djot-ts-table--cell-at (table pos)
  "Return the bounds (BEG . END) of the content cell of TABLE at POS, or nil."
  (catch 'found
    (dolist (row (treesit-node-children table t))
      (when (member (treesit-node-type row) '("table_header" "table_row"))
        (dolist (c (treesit-node-children row t))
          (when (<= (treesit-node-start c) pos (treesit-node-end c))
            (throw 'found (cons (treesit-node-start c) (treesit-node-end c)))))))))

(defun djot-ts-table--post-command ()
  "Realign tables when point enters or leaves a cell.
The cell at point is shown unshrunk."
  (with-demoted-errors "djot-ts-table: %S"
    (let* ((tb (djot-ts-table--at (point)))
           (cell (and tb (djot-ts-table--cell-at tb (point))))
           (new (and cell (list (treesit-node-start tb) (treesit-node-end tb) cell)))
           (old djot-ts-table--current))
      (unless (equal new old)
        (setq djot-ts-table--current new)
        (dolist (b (seq-uniq (delq nil (list (take 2 old) (take 2 new)))))
          (when b
            (djot-ts-table--align-region (min (car b) (point-max))
                                         (min (cadr b) (point-max)))))))))

(defun djot-ts-table--window-size-change (win)
  "Realign the tables of WIN's buffer to its new width."
  (with-current-buffer (window-buffer win)
    (djot-ts-table-align)))

(defun djot-ts-table-align ()
  "Align all tables in the buffer.
Useful after changing fonts; edits and refontification realign
tables automatically."
  (interactive)
  (djot-ts-table--align-region (point-min) (point-max)))

(defun djot-ts-table--jit (beg end)
  "Align the tables between BEG and END.  For `jit-lock-functions'."
  (djot-ts-table--align-region beg end)
  nil)

(defconst djot-ts-table--realign-hooks
  '(text-scale-mode-hook buffer-face-mode-hook mixed-pitch-mode-hook)
  "Hooks after which tables are realigned, as fonts may have changed.
`buffer-face-mode-hook' covers `variable-pitch-mode'.")

;;;###autoload
(define-minor-mode djot-ts-table-mode
  "Align Djot pipe tables by the pixel width of their cells.
The buffer text is not changed."
  :lighter nil
  (if djot-ts-table-mode
      (progn
        (unless (treesit-parser-list nil 'djot)
          (setq djot-ts-table-mode nil)
          (user-error "No Djot parser in this buffer"))
        ;; After `font-lock-fontify-region', so that faces and
        ;; invisibility are in place when measuring.
        (add-hook 'jit-lock-functions #'djot-ts-table--jit 90 t)
        (dolist (h djot-ts-table--realign-hooks)
          (add-hook h #'djot-ts-table-align nil t))
        (add-hook 'post-command-hook #'djot-ts-table--post-command nil t)
        (add-hook 'window-size-change-functions
                  #'djot-ts-table--window-size-change nil t)
        (djot-ts-table-align))
    (remove-hook 'jit-lock-functions #'djot-ts-table--jit t)
    (dolist (h djot-ts-table--realign-hooks)
      (remove-hook h #'djot-ts-table-align t))
    (remove-hook 'post-command-hook #'djot-ts-table--post-command t)
    (remove-hook 'window-size-change-functions
                 #'djot-ts-table--window-size-change t)
    (setq djot-ts-table--current nil)
    (save-restriction
      (widen)
      (djot-ts-table--remove (point-min) (point-max)))))

(provide 'djot-ts-table)
;;; djot-ts-table.el ends here
