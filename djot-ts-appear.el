;;; djot-ts-appear.el --- Show hidden Djot markers at point -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: text

;;; Commentary:

;; Minor mode for `djot-ts-mode' that hides the markers of inline
;; markup (emphasis, strong, verbatim, ...) and shows them while point
;; is on the element, in the spirit of org-appear.  Point tracking uses
;; `cursor-sensor-mode'.

;;; Code:

(require 'treesit)
(require 'cursor-sensor)
(require 'text-property-search)

(defgroup djot-ts-appear nil
  "Hide Djot markup markers except at point."
  :group 'djot-ts)

(defcustom djot-ts-appear-elements
  '("emphasis" "strong" "highlighted" "insert" "delete"
    "superscript" "subscript" "verbatim")
  "Tree-sitter node types whose markers are hidden.
Each must have \"begin_marker\" and \"end_marker\" fields.
Changes take effect when `djot-ts-appear-mode' is re-enabled."
  :type '(repeat string))

(defvar djot-ts-appear-mode)

(defvar-local djot-ts-appear--revealed nil
  "List of (BEG . END) of the elements whose markers are shown.")

(defvar-local djot-ts-appear--settings nil
  "Font-lock settings added by `djot-ts-appear-mode'.")

(defvar-local djot-ts-appear--own-cursor-sensor nil
  "Non-nil if `djot-ts-appear-mode' enabled `cursor-sensor-mode'.")

(defconst djot-ts-appear--props '(invisible cursor-sensor-functions)
  "Text properties managed by `djot-ts-appear-mode'.")

(defun djot-ts-appear--fontify (node _override start end &rest _)
  "Hide the markers of NODE unless it is revealed.
Only touch text between START and END."
  (let ((nbeg (treesit-node-start node))
        (nend (treesit-node-end node)))
    (put-text-property (max start nbeg) (min end nend)
                       'cursor-sensor-functions '(djot-ts-appear--sensor))
    (unless (member (cons nbeg nend) djot-ts-appear--revealed)
      (dolist (field '("begin_marker" "end_marker"))
        (when-let* ((m (treesit-node-child-by-field-name node field))
                    (b (max start (treesit-node-start m)))
                    (e (min end (treesit-node-end m)))
                    ((< b e)))
          (put-text-property b e 'invisible 'djot-ts-appear))))))

(defun djot-ts-appear--ranges (pos)
  "Return (BEG . END) of the elements around POS, ends included."
  (let (ranges)
    (dolist (p (list pos (max (point-min) (1- pos))))
      (let ((node (treesit-node-at p 'djot)))
        (while node
          (let ((r (cons (treesit-node-start node) (treesit-node-end node))))
            (when (and (member (treesit-node-type node) djot-ts-appear-elements)
                       (<= (car r) pos (cdr r))
                       (not (member r ranges)))
              (push r ranges)))
          (setq node (treesit-node-parent node)))))
    ranges))

(defun djot-ts-appear--update (pos)
  "Show the markers of the elements around POS and hide the others."
  (let ((new (djot-ts-appear--ranges pos))
        (old djot-ts-appear--revealed))
    (unless (equal new old)
      (setq djot-ts-appear--revealed new)
      (dolist (r (append old new))
        (font-lock-flush (min (car r) (point-max)) (min (cdr r) (point-max)))))))

(defun djot-ts-appear--sensor (window _oldpos _dir)
  "Function for `cursor-sensor-functions'; update around WINDOW's point."
  (with-current-buffer (window-buffer window)
    (when djot-ts-appear-mode
      (djot-ts-appear--update (window-point window)))))

(defun djot-ts-appear--clear ()
  "Remove the text properties added by `djot-ts-appear-mode'."
  (with-silent-modifications
    (remove-list-of-text-properties (point-min) (point-max)
                                    '(cursor-sensor-functions))
    (save-excursion
      (goto-char (point-min))
      (let (m)
        (while (setq m (text-property-search-forward
                        'invisible 'djot-ts-appear t))
          (remove-list-of-text-properties
           (prop-match-beginning m) (prop-match-end m) '(invisible)))))))

;;;###autoload
(define-minor-mode djot-ts-appear-mode
  "Hide Djot inline markup markers, showing them when point is on them."
  :lighter nil
  (if djot-ts-appear-mode
      (progn
        (setq djot-ts-appear--settings
              (treesit-font-lock-rules
               :language 'djot
               :feature 'appear
               `(,(vconcat (mapcar (lambda (type) (list (intern type)))
                                   djot-ts-appear-elements))
                 @djot-ts-appear--fontify)))
        (setq-local treesit-font-lock-settings
                    (append treesit-font-lock-settings djot-ts-appear--settings)
                    treesit-font-lock-feature-list
                    (cons (append (car treesit-font-lock-feature-list) '(appear))
                          (cdr treesit-font-lock-feature-list))
                    font-lock-extra-managed-props
                    (seq-union font-lock-extra-managed-props
                               djot-ts-appear--props))
        (treesit-font-lock-recompute-features)
        (add-to-invisibility-spec 'djot-ts-appear)
        (unless cursor-sensor-mode
          (setq djot-ts-appear--own-cursor-sensor t)
          (cursor-sensor-mode 1)))
    (setq-local treesit-font-lock-settings
                (seq-remove (lambda (s) (memq s djot-ts-appear--settings))
                            treesit-font-lock-settings)
                treesit-font-lock-feature-list
                (mapcar (lambda (l) (remq 'appear l))
                        treesit-font-lock-feature-list)
                font-lock-extra-managed-props
                (seq-difference font-lock-extra-managed-props
                                djot-ts-appear--props))
    (treesit-font-lock-recompute-features)
    (remove-from-invisibility-spec 'djot-ts-appear)
    (djot-ts-appear--clear)
    (setq djot-ts-appear--settings nil
          djot-ts-appear--revealed nil)
    (when djot-ts-appear--own-cursor-sensor
      (setq djot-ts-appear--own-cursor-sensor nil)
      (cursor-sensor-mode -1)))
  (font-lock-flush))

(provide 'djot-ts-appear)
;;; djot-ts-appear.el ends here
