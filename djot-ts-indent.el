;;; djot-ts-indent.el --- Virtual indentation for Djot -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>

;;; Commentary:

;; `djot-ts-indent-mode' indents the text under each heading by its
;; section depth, like `org-indent-mode'.  Only the display changes,
;; through `line-prefix' and `wrap-prefix'; the buffer text is untouched.

;;; Code:

(require 'treesit)
(require 'jit-lock)

(defgroup djot-ts-indent nil
  "Virtual indentation for Djot."
  :group 'djot-ts)

(defcustom djot-ts-indent-indentation-per-level 1
  "Number of columns of indentation per section level."
  :type 'natnum)

(defcustom djot-ts-indent-offset 1
  "Extra columns of indentation for lines inside a section.
With the default of 1, body text aligns with the heading text."
  :type 'natnum)

(defun djot-ts-indent--depth (pos)
  "Return the number of sections enclosing POS."
  (let ((node (treesit-node-at pos 'djot))
        (depth 0))
    (while node
      (when (equal (treesit-node-type node) "section")
        (setq depth (1+ depth)))
      (setq node (treesit-node-parent node)))
    depth))

(defun djot-ts-indent--heading-marker (bol)
  "Return the marker node of the section heading starting at BOL, or nil."
  (let ((node (treesit-node-at bol 'djot)))
    (and (= (treesit-node-start node) bol)
         (equal (treesit-node-type node) "marker")
         (equal (treesit-node-type (treesit-node-parent node)) "heading")
         (equal (treesit-node-type (treesit-node-parent (treesit-node-parent node)))
                "section")
         node)))

(defun djot-ts-indent--attribute-heading-marker (bol eol)
  "Return the heading marker node if BOL..EOL is in a heading's attribute."
  (let ((node (treesit-node-at bol 'djot)))
    (while (and node (not (member (treesit-node-type node)
                                  '("block_attribute" "section" "document"))))
      (setq node (treesit-node-parent node)))
    (when-let* (((equal (treesit-node-type node) "block_attribute"))
                ;; `treesit-node-at' may skip past a blank line.
                ((<= (treesit-node-start node) eol))
                ((> (treesit-node-end node) bol))
                (section (treesit-node-parent node))
                ((equal (treesit-node-type section) "section"))
                (heading (treesit-node-child-by-field-name section "heading")))
      (treesit-node-child-by-field-name heading "marker"))))

(defun djot-ts-indent--marker-at (pos)
  "Return the block quote or list marker node starting at POS, or nil."
  ;; Not `treesit-node-at': it skips to the next leaf, and the "- " of
  ;; a task marker is not a leaf.
  (let ((node (treesit-node-on pos (1+ pos) 'djot)))
    (while (and node (= (treesit-node-start node) pos)
                (not (string-match-p "\\`\\(?:block_quote_marker\\|list_marker_\\)"
                                     (treesit-node-type node))))
      (setq node (treesit-node-parent node)))
    (and node (= (treesit-node-start node) pos) node)))

(defun djot-ts-indent--continuation (bol eol)
  "Return the extra wrap prefix for the line between BOL and EOL.
This is the line's indentation, with list markers turned into spaces
and block quote markers repeated."
  (let ((pos bol) (parts nil) node)
    (while (progn
             (let ((ws (save-excursion
                         (goto-char pos) (skip-chars-forward " \t" eol) (point))))
               (when (> ws pos)
                 (push (buffer-substring-no-properties pos ws) parts)
                 (setq pos ws)))
             (and (< pos eol) (setq node (djot-ts-indent--marker-at pos))))
      (let ((end (min (treesit-node-end node) eol)))
        (push (if (equal (treesit-node-type node) "block_quote_marker")
                  (propertize (buffer-substring-no-properties pos end) 'face 'shadow)
                (make-string (- end pos) ?\s))
              parts)
        (setq pos end)))
    (apply #'concat (nreverse parts))))

(defun djot-ts-indent--fontify (beg end)
  "Set `line-prefix' and `wrap-prefix' on the lines between BEG and END."
  (save-excursion
    (goto-char beg)
    (setq beg (pos-bol))
    (goto-char end)
    (setq end (if (bolp) end (min (1+ (pos-eol)) (point-max))))
    (goto-char beg)
    (with-silent-modifications
      (while (< (point) end)
        (let* ((bol (point))
               (eol (pos-eol))
               (depth (djot-ts-indent--depth bol))
               (indent (if (> depth 0)
                           (+ (* depth djot-ts-indent-indentation-per-level)
                              djot-ts-indent-offset)
                         0))
               (heading (djot-ts-indent--heading-marker bol))
               (marker (or heading
                           (djot-ts-indent--attribute-heading-marker bol eol)))
               (prefix (make-string
                        (if marker
                            (max 0 (- indent (- (treesit-node-end marker)
                                                (treesit-node-start marker))))
                          indent)
                        ?\s))
               (wrap (cond (heading (make-string indent ?\s))
                           (marker prefix)
                           (t (concat prefix (djot-ts-indent--continuation bol eol))))))
          (add-text-properties bol (min (1+ eol) (point-max))
                               `(line-prefix ,prefix wrap-prefix ,wrap))
          (forward-line 1)))))
  `(jit-lock-bounds ,beg . ,end))

(defun djot-ts-indent--notify (ranges _parser)
  "Mark RANGES, which changed in the syntax tree, for re-indentation."
  (with-silent-modifications
    (dolist (range ranges)
      (put-text-property (max (point-min) (car range))
                         (min (point-max) (cdr range))
                         'fontified nil))))

;;;###autoload
(define-minor-mode djot-ts-indent-mode
  "Indent the text under Djot headings by section depth, for display only."
  :lighter " Ind"
  (let ((parser (car (treesit-parser-list nil 'djot))))
    (cond
     ((and djot-ts-indent-mode (not parser))
      (setq djot-ts-indent-mode nil)
      (user-error "No Djot parser in this buffer"))
     (djot-ts-indent-mode
      (treesit-parser-add-notifier parser #'djot-ts-indent--notify)
      (jit-lock-register #'djot-ts-indent--fontify)
      (jit-lock-refontify))
     (t
      (when parser
        (treesit-parser-remove-notifier parser #'djot-ts-indent--notify))
      (jit-lock-unregister #'djot-ts-indent--fontify)
      (with-silent-modifications
        (remove-text-properties (point-min) (point-max)
                                '(line-prefix nil wrap-prefix nil)))))))

(provide 'djot-ts-indent)
;;; djot-ts-indent.el ends here
