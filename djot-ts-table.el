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
;; A table is fontified whole before it is aligned, so it is aligned
;; once however many jit-lock chunks it spans.  Alignment uses
;; overlays, which font-lock leaves alone.
;;
;; Row measurements are cached per table, keyed by the row text with
;; its properties and the width-changing properties of other overlays,
;; so realigning a table only measures its changed rows.
;;
;; Tables wider than the window are shrunk to fit: the widest columns
;; are cut to a common width, ending in `djot-ts-table-ellipsis'.  The
;; cell at point is shown in full, pushing the rest of its row to the
;; right; the full text of a cut cell is also its tooltip.

;;; Code:

(require 'treesit)
(require 'seq)
(require 'jit-lock)

(defvar-local djot-ts-table--query nil
  "Compiled query for table nodes.")

(defvar-local djot-ts-table--current nil
  "(TBEG TEND (CBEG . CEND)): bounds of the table and the cell at point.
Nil if point is not in a content cell.")

(defvar-local djot-ts-table--body-width nil
  "Body width in pixels of the window that tables were aligned for.")

(defvar djot-ts-table--busy nil
  "Start positions of the tables being fontified to be aligned.
Fontifying them runs `djot-ts-table--jit', which skips them.")

(defconst djot-ts-table-ellipsis "…"
  "String shown in place of the cut end of a shrunk cell.")

(defconst djot-ts-table--width-props
  '(display invisible before-string after-string)
  "Overlay properties that can change the width of a row.")

(define-hash-table-test 'djot-ts-table--equal
  #'equal-including-properties #'sxhash-equal-including-properties)

(defun djot-ts-table--width (from to win)
  "Return the pixel width of the text from FROM to TO, as shown in WIN.
WIN nil means no window shows the buffer; then text properties are
measured, but not overlays and line prefixes."
  (cond ((>= from to) 0)
        (win (car (window-text-pixel-size win from to t)))
        (t (string-pixel-width (buffer-substring from to) (current-buffer)))))

(defun djot-ts-table--space-width (win)
  "Return the pixel width of the padding around cell contents in WIN."
  (if win
      (window-font-width win)
    (string-pixel-width " " (current-buffer))))

(defun djot-ts-table--avail (win s)
  "Return the pixel width available to tables in WIN.
S is the width of a space."
  (- (window-body-width win t) s
     (if display-line-numbers (line-number-display-width t) 0)))

(defun djot-ts-table--alignment (node)
  "Return the alignment of the separator cell NODE.
This is `left', `right', `center' or nil for the default."
  (let ((s (treesit-node-text node t)))
    (pcase (cons (string-prefix-p ":" s) (string-suffix-p ":" s))
      ('(t . nil) 'left)
      ('(nil . t) 'right)
      ('(t . t) 'center))))

(defun djot-ts-table--measure-row (row sep win)
  "Measure ROW, a separator row if SEP, in WIN.
Return a plist (:x0 :pipe :cells), or nil for a malformed row.
Positions are relative to the start of ROW.  :x0 is the pixel
position after the opening pipe, :pipe the width of a pipe.  Each
cell is a plist with the positions :beg and :end.  Content cells also
have the positions :cbeg, :cend and the content width :w; separator
cells have :natural (their width), :align, :dash (position of a `-')
and :dashw."
  (let ((rbeg (treesit-node-start row))
        (cells (treesit-node-children row t)))
    (when (and cells (eq (char-after rbeg) ?|))
      (list
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
                (let ((dash (save-excursion
                              (goto-char beg)
                              (skip-chars-forward ":" end)
                              (point))))
                  (list :beg (- beg rbeg) :end (- end rbeg)
                        :natural (djot-ts-table--width beg end win)
                        :align (djot-ts-table--alignment c)
                        :dash (- dash rbeg)
                        :dashw (max 1 (djot-ts-table--width dash (1+ dash) win))))
              (let* ((cbeg (save-excursion
                             (goto-char beg) (skip-chars-forward " \t" end) (point)))
                     (cend (save-excursion
                             (goto-char end) (skip-chars-backward " \t" cbeg) (point))))
                (list :beg (- beg rbeg) :end (- end rbeg)
                      :cbeg (- cbeg rbeg) :cend (- cend rbeg)
                      :w (djot-ts-table--width cbeg cend win))))))
        cells)))))

(defun djot-ts-table--row-key (row sep)
  "Return the cache key of ROW, a separator row if SEP.
It holds the text from the line start to the end of ROW, with its
properties and the `djot-ts-table--width-props' of the overlays
there.  Table overlays must be removed first."
  (let* ((end (treesit-node-end row))
         (bol (save-excursion (goto-char (treesit-node-start row)) (pos-bol)))
         ovs)
    (dolist (o (overlays-in bol end))
      (let ((props (mapcar (lambda (p) (overlay-get o p))
                           djot-ts-table--width-props)))
        (when (seq-some #'identity props)
          (push `(,(- (overlay-start o) bol) ,(- (overlay-end o) bol) ,@props)
                ovs))))
    (list sep (buffer-substring bol end) ovs)))

(defun djot-ts-table--rows (table win s old new)
  "Return the rows of TABLE, measured in WIN with spaces S pixels wide.
Measurements are looked up in the list OLD of hash tables, else
measured, and stored in the hash table NEW.  Each row is a plist with
its start :beg, :sep, :x0 and :pipe as in
`djot-ts-table--measure-row', :aligns (see
`djot-ts-table--alignment') and :cells.  Each cell is a plist with
its measurement :m and the pixel width :need it requires between its
pipes."
  (let (rows aligns)
    (dolist (node (treesit-node-children table t))
      (let ((type (treesit-node-type node)))
        (when (member type '("table_header" "table_row" "table_separator"))
          (let* ((sep (equal type "table_separator"))
                 (key (djot-ts-table--row-key node sep))
                 (m (or (gethash key new)
                        (seq-some (lambda (h) (gethash key h)) old)
                        (djot-ts-table--measure-row node sep win)
                        'malformed)))
            (puthash key m new)
            (when (consp m)
              (when sep
                (setq aligns (mapcar (lambda (c) (plist-get c :align))
                                     (plist-get m :cells)))
                ;; The row before a separator is a header aligned by it.
                (when-let* ((prev (car rows))
                            ((not (plist-get prev :sep))))
                  (plist-put prev :aligns aligns)))
              (push (list :beg (treesit-node-start node) :sep sep
                          :x0 (plist-get m :x0) :pipe (plist-get m :pipe)
                          :aligns aligns
                          :cells (mapcar (lambda (c)
                                           (list :m c :need (if sep
                                                                (plist-get c :natural)
                                                              (+ (plist-get c :w) (* 2 s)))))
                                         (plist-get m :cells)))
                    rows))))))
    (nreverse rows)))

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

(defun djot-ts-table--remove (beg end &optional keep)
  "Delete the table overlays between BEG and END.
Keep those inside one of the ranges (FROM . TO) in KEEP."
  (dolist (ov (overlays-in beg end))
    (when (and (overlay-get ov 'djot-ts-table)
               (not (seq-some (lambda (r) (<= (car r) (overlay-start ov)
                                              (overlay-end ov) (cdr r)))
                              keep)))
      (delete-overlay ov))))

(defun djot-ts-table--info (beg end)
  "Return the infos of the tables aligned between BEG and END.
See the `djot-ts-table-info' overlay made by `djot-ts-table--align'."
  (delq nil (mapcar (lambda (o) (overlay-get o 'djot-ts-table-info))
                    (overlays-in beg end))))

(defun djot-ts-table--pipes (rows ncols &optional cap)
  "Return a vector of the pixel positions of the pipes after each column.
With CAP, content cells take at most CAP pixels.  A cell with
:expanded counts as that many pixels wide."
  ;; Called repeatedly by `djot-ts-table--cap', so it avoids consing.
  (let ((pipes (make-vector ncols 0))
        ;; Where column J starts in each row.
        (starts (vconcat (mapcar (lambda (r) (plist-get r :x0)) rows))))
    (dotimes (j ncols)
      (let ((i 0) (p 0))
        (dolist (r rows)
          (when-let* ((c (nth j (plist-get r :cells))))
            (let ((need (or (plist-get c :expanded) (plist-get c :need))))
              (when (and cap (not (plist-get r :sep)))
                (setq need (min need cap)))
              (setq p (max p (+ (aref starts i) need)))))
          (setq i (1+ i)))
        (aset pipes j p)
        (setq i 0)
        (dolist (r rows)
          (aset starts i (+ p (plist-get r :pipe)))
          (setq i (1+ i)))))
    pipes))

(defun djot-ts-table--cap (rows ncols avail s pipe)
  "Return the widest content cell width fitting ROWS in AVAIL pixels.
S is the width of a space, PIPE that of the widest pipe.  The cap is
at least two characters plus padding."
  (let ((lo (* 4 s))
        (hi (seq-max (mapcar (lambda (r) (if (plist-get r :sep) 0
                                           (seq-max (mapcar (lambda (c) (plist-get c :need))
                                                            (plist-get r :cells)))))
                             rows))))
    (while (< lo hi)
      (let ((mid (/ (+ lo hi 1) 2)))
        (if (<= (+ (aref (djot-ts-table--pipes rows ncols mid) (1- ncols)) pipe)
                avail)
            (setq lo mid)
          (setq hi (1- mid)))))
    lo))

(defun djot-ts-table--cut (m rbeg width win ew)
  "Return (POS . W) to show content cell M of the row at RBEG in WIDTH.
M is a cell measurement, see `djot-ts-table--measure-row'.  The text
from POS, relative to RBEG, is replaced by an ellipsis EW pixels
wide; W is the width of the shown text, ellipsis included.  The
result is cached in M."
  (let ((cached (plist-get m :cut)))
    (if (equal (car cached) (cons width ew))
        (cdr cached)
      (let* ((beg (+ rbeg (plist-get m :cbeg)))
             (lo beg)
             (hi (+ rbeg (plist-get m :cend))))
        (while (< lo hi)
          (let ((mid (/ (+ lo hi 1) 2)))
            (if (<= (+ (djot-ts-table--width beg mid win) ew) width)
                (setq lo mid)
              (setq hi (1- mid)))))
        (let ((res (cons (- lo rbeg) (+ (djot-ts-table--width beg lo win) ew))))
          (plist-put m :cut (cons (cons width ew) res))
          res)))))

(defun djot-ts-table--align (table)
  "Align the pipe table node TABLE.
Shrink it to fit its window, except for the cell in
`djot-ts-table--current', which pushes the rest of its row to the
right.  Record the measurements, the shrinking and the start of the
cut cells in a `djot-ts-table-info' overlay over TABLE."
  (let* ((tbeg (treesit-node-start table))
         (tend (treesit-node-end table))
         ;; Not point: under jit-lock, point is wherever earlier
         ;; fontification functions left it.
         (pt (car (nth 2 djot-ts-table--current)))
         (win (get-buffer-window (current-buffer) t))
         (s (djot-ts-table--space-width win))
         (old (mapcar (lambda (i) (plist-get i :cache))
                      (djot-ts-table--info tbeg tend)))
         (new (make-hash-table :test 'djot-ts-table--equal))
         rows ncols pipe (natural 0) cap cut pipes)
    ;; Measure without our overlays.
    (djot-ts-table--remove tbeg tend)
    (setq rows (djot-ts-table--rows table win s old new))
    (setq ncols (seq-max (cons 0 (mapcar (lambda (r) (length (plist-get r :cells)))
                                         rows))))
    (when (> ncols 0)
      (setq pipe (seq-max (mapcar (lambda (r) (plist-get r :pipe)) rows))
            natural (+ (aref (djot-ts-table--pipes rows ncols) (1- ncols)) pipe))
      (when-let* ((win)
                  (avail (djot-ts-table--avail win s))
                  ((> natural avail)))
        (setq cap (djot-ts-table--cap rows ncols avail s pipe))))
    (when cap
      (let ((ew (string-pixel-width djot-ts-table-ellipsis (current-buffer))))
        (dolist (r rows)
          (unless (plist-get r :sep)
            (dolist (c (plist-get r :cells))
              (when (> (plist-get c :need) cap)
                (let* ((m (plist-get c :m))
                       (rbeg (plist-get r :beg))
                       (beg (+ rbeg (plist-get m :beg)))
                       (res (djot-ts-table--cut m rbeg (- cap (* 2 s)) win ew))
                       (need (+ (plist-get c :need) (- (cdr res) (plist-get m :w)))))
                  (push beg cut)
                  (if (and pt (<= beg pt (+ rbeg (plist-get m :end))))
                      (plist-put c :expanded need)
                    (plist-put c :need need)
                    (plist-put c :cut (+ rbeg (car res)))))))))))
    ;; Column J ends at pixel (aref pipes J), where the next pipe starts.
    (setq pipes (djot-ts-table--pipes rows ncols))
    (dolist (r rows)
      ;; SHIFT is how far an expanded cell pushed the rest of the row.
      (let ((rbeg (plist-get r :beg)) (x (plist-get r :x0)) (j 0) (shift 0))
        (dolist (c (plist-get r :cells))
          (let* ((m (plist-get c :m))
                 (need (plist-get c :need))
                 (end (+ rbeg (plist-get m :end)))
                 (pipe (+ (aref pipes j) shift))
                 (align (nth j (plist-get r :aligns))))
            (when (> (+ x need) pipe)
              (setq shift (+ shift (- (+ x need) pipe))
                    pipe (+ x need)))
            (if (plist-get r :sep)
                (let ((n (floor (- pipe x (plist-get m :natural)) (plist-get m :dashw))))
                  (when (> n 0)
                    (djot-ts-table--overlay
                     (1- end) (1- end) 'before-string
                     (propertize (make-string n ?-)
                                 'face (get-text-property (+ rbeg (plist-get m :dash))
                                                          'face)))))
              (let* ((cbeg (+ rbeg (plist-get m :cbeg)))
                     (cend (+ rbeg (plist-get m :cend)))
                     (free (- pipe x need))
                     (cx (+ x s (pcase align
                                  ('right free)
                                  ('center (/ free 2.0))
                                  (_ 0)))))
                (djot-ts-table--space (+ rbeg (plist-get m :beg)) cbeg cx)
                (when-let* ((from (plist-get c :cut)))
                  (let ((ov (djot-ts-table--overlay from cend 'display
                                                    djot-ts-table-ellipsis)))
                    (overlay-put ov 'evaporate t)
                    (overlay-put ov 'help-echo
                                 (buffer-substring-no-properties cbeg cend))))))
            (djot-ts-table--space (if (plist-get r :sep)
                                      end
                                    (+ rbeg (plist-get m :cend)))
                                  end pipe)
            (setq x (+ pipe (plist-get r :pipe))
                  j (1+ j))))))
    (djot-ts-table--overlay tbeg tend 'djot-ts-table-info
                            (list :cache new :cap cap :natural natural :cut cut))))

(defun djot-ts-table--align-now (table)
  "Fontify TABLE whole, then align it."
  (let ((djot-ts-table--busy (cons (treesit-node-start table) djot-ts-table--busy)))
    (when jit-lock-mode
      (jit-lock-fontify-now (treesit-node-start table) (treesit-node-end table)))
    (djot-ts-table--align table)))

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
        (let* ((tables (seq-remove (lambda (tb)
                                     (memql (treesit-node-start tb) djot-ts-table--busy))
                                   (djot-ts-table--tables beg end)))
               (ranges (mapcar (lambda (tb)
                                 (cons (treesit-node-start tb) (treesit-node-end tb)))
                               tables)))
          (mapc #'djot-ts-table--align-now tables)
          ;; Within a table being fontified, its overlays are not stale.
          (unless djot-ts-table--busy
            (djot-ts-table--remove beg end ranges)))))))

(defun djot-ts-table--at (pos)
  "Return the table node containing POS, or nil."
  (seq-find (lambda (tb) (<= (treesit-node-start tb) pos (treesit-node-end tb)))
            (djot-ts-table--tables (max (point-min) (1- pos))
                                   (min (point-max) (1+ pos)))))

(defun djot-ts-table--cell-at (table pos)
  "Return the bounds (BEG . END) of the content cell of TABLE at POS, or nil."
  (when-let* ((row (treesit-node-first-child-for-pos table pos t))
              ((member (treesit-node-type row) '("table_header" "table_row"))))
    (seq-some (lambda (c)
                (and (<= (treesit-node-start c) pos (treesit-node-end c))
                     (cons (treesit-node-start c) (treesit-node-end c))))
              (treesit-node-children row t))))

(defun djot-ts-table--cut-p (tbeg cbeg)
  "Non-nil if the table aligned at TBEG has cut the cell at CBEG."
  (and (< tbeg (point-max))
       (seq-some (lambda (o)
                   (memql cbeg (plist-get (overlay-get o 'djot-ts-table-info) :cut)))
                 (overlays-at tbeg))))

(defun djot-ts-table--post-command ()
  "Realign tables when point enters or leaves a cut cell.
The cell at point is shown uncut."
  (with-demoted-errors "djot-ts-table: %S"
    (let* ((tb (djot-ts-table--at (point)))
           (cell (and tb (djot-ts-table--cell-at tb (point))))
           (new (and cell (list (treesit-node-start tb) (treesit-node-end tb) cell)))
           (old djot-ts-table--current)
           todo)
      (unless (equal new old)
        (setq djot-ts-table--current new)
        (dolist (cur (list old new))
          (when (and cur
                     (djot-ts-table--cut-p (car cur) (car (nth 2 cur)))
                     (not (member (take 2 cur) todo)))
            (push (take 2 cur) todo)))
        (dolist (b todo)
          (djot-ts-table--align-region (min (car b) (point-max))
                                       (min (cadr b) (point-max))))))))

(defun djot-ts-table--window-size-change (win)
  "Realign the tables of WIN's buffer that its new width affects.
These are the shrunk tables and those that no longer fit."
  (with-current-buffer (window-buffer win)
    (when-let* ((w (get-buffer-window (current-buffer) t))
                (width (window-body-width w t))
                ((not (eql width djot-ts-table--body-width))))
      (setq djot-ts-table--body-width width)
      (let ((avail (djot-ts-table--avail w (djot-ts-table--space-width w))))
        (save-restriction
          (widen)
          (dolist (o (overlays-in (point-min) (point-max)))
            (when-let* ((info (overlay-get o 'djot-ts-table-info))
                        ((or (plist-get info :cap)
                             (> (plist-get info :natural) avail))))
              (font-lock-flush (overlay-start o) (overlay-end o)))))))))

(defun djot-ts-table-align ()
  "Remeasure and realign all tables in the buffer.
Tables are realigned when next displayed.  Useful after changing
fonts; edits and refontification realign tables automatically."
  (interactive)
  (save-restriction
    (widen)
    (djot-ts-table--remove (point-min) (point-max))
    (dolist (tb (djot-ts-table--tables (point-min) (point-max)))
      (font-lock-flush (treesit-node-start tb) (treesit-node-end tb)))))

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
    (setq djot-ts-table--current nil
          djot-ts-table--body-width nil)
    (save-restriction
      (widen)
      (djot-ts-table--remove (point-min) (point-max)))))

(provide 'djot-ts-table)
;;; djot-ts-table.el ends here
