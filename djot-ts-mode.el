;;; djot-ts-mode.el --- Major mode for Djot using tree-sitter -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>
;; Version: 0.1
;; Package-Requires: ((emacs "31.1"))
;; Keywords: text, languages

;;; Commentary:

;; Tree-sitter major mode for Djot, derived from `outline-mode'.
;; Grammar: https://codeberg.org/treeman/tree-sitter-djot

;;; Code:

(require 'treesit)
(require 'outline)
(require 'hideshow)
(require 'org-faces)

(defgroup djot-ts nil
  "Tree-sitter support for Djot."
  :group 'text)

(defface djot-ts-delete '((t :strike-through t))
  "Face for deleted text.")

(defface djot-ts-code-block '((t :inherit org-block))
  "Face for the body of code and raw blocks.")

(defface djot-ts-code-block-delimiter '((t :inherit org-block-begin-line))
  "Face for the opening and closing lines of code and raw blocks.")

(defface djot-ts-heading-marker '((t :inherit shadow))
  "Face for heading markers, on top of the heading's `outline-N' face.")

(defcustom djot-ts-hide-leading-markers nil
  "Non-nil means display all but the last `#' of heading markers as dots.
Takes effect after refontification, e.g. with \\[font-lock-update]."
  :type 'boolean
  :group 'djot-ts)

(defun djot-ts-mode--opener (node)
  "Return the child of block NODE where its own syntax begins.
This skips the block attributes and block quote markers before it.
A block attribute is its own opener."
  (if (equal (treesit-node-type node) "block_attribute")
      node
    (seq-find (lambda (child)
                (not (member (treesit-node-type child)
                             '("block_attribute" "block_quote_marker"))))
              (treesit-node-children node)
              node)))

(defun djot-ts-mode--fontify-code-block (node override start end &rest _)
  "Give code or raw block NODE an Org-like background.
The fence lines get `djot-ts-code-block-delimiter', the rest
`djot-ts-code-block'.  OVERRIDE, START and END are as in
`treesit-fontify-with-override'."
  (let* ((beg (treesit-node-start (djot-ts-mode--opener node)))
         (fin (treesit-node-end node))
         (body (save-excursion (goto-char beg) (min fin (1+ (pos-eol)))))
         (closed (string-suffix-p "_end" (treesit-node-type
                                          (treesit-node-child node -1))))
         (tail (if closed
                   (save-excursion (goto-char (max body (1- fin))) (pos-bol))
                 fin)))
    (treesit-fontify-with-override
     beg body 'djot-ts-code-block-delimiter override start end)
    (treesit-fontify-with-override
     body tail 'djot-ts-code-block override start end)
    (when closed
      (treesit-fontify-with-override
       tail fin 'djot-ts-code-block-delimiter override start end))))

(defvar djot-ts-mode--syntax-table
  (let ((table (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." table)
    table)
  "Syntax table for `djot-ts-mode'.")

(defun djot-ts-mode--fontify-heading (node override start end &rest _)
  "Fontify heading NODE with `outline-N' by its marker length.
The marker also gets `djot-ts-heading-marker'.  If
`djot-ts-hide-leading-markers' is non-nil, each `#' of the marker but
the last is displayed as a middle dot with that face.
OVERRIDE, START and END are as in `treesit-fontify-with-override'."
  (let* ((marker (treesit-node-child-by-field-name node "marker"))
         (mbeg (treesit-node-start marker))
         (level (or (string-search " " (treesit-node-text marker t)) 1))
         (face (intern (format "outline-%d" (min level 8)))))
    (treesit-fontify-with-override
     mbeg (treesit-node-end node) face override start end)
    (treesit-fontify-with-override
     mbeg (treesit-node-end marker) 'djot-ts-heading-marker 'prepend start end)
    (when djot-ts-hide-leading-markers
      ;; One display string per character, so that each is a separate
      ;; replacement (`eq' adjacent strings would merge into one).
      (dotimes (i (1- level))
        (let ((pos (+ mbeg i)))
          (when (and (<= start pos) (< pos end))
            (put-text-property
             pos (1+ pos) 'display
             (propertize " " 'face 'djot-ts-heading-marker))))))))

(defvar djot-ts-mode--font-lock-settings
  (treesit-font-lock-rules
   :language 'djot
   :feature 'comment
   '([(comment) (inline_comment)] @font-lock-comment-face
     [(todo) (fixme)] @font-lock-warning-face
     (note) @font-lock-doc-face)

   :language 'djot
   :feature 'heading
   '((heading) @djot-ts-mode--fontify-heading
     (table_header) @bold
     (list_item (term) @bold))

   :language 'djot
   :feature 'markup
   '((emphasis) @italic
     (strong) @bold
     (insert) @underline
     (delete) @djot-ts-delete
     (highlighted) @highlight
     [(verbatim) (raw_inline) (frontmatter)]
     @font-lock-string-face
     (math) @font-lock-constant-face
     (symbol) @font-lock-builtin-face
     (block_quote) @font-lock-doc-face
     (image_description) @italic)

   :language 'djot
   :feature 'link
   '([(link_text) (link_label) (reference_label)] @link
     [(autolink) (inline_link_destination) (link_destination)]
     @font-lock-string-face)

   :language 'djot
   :feature 'attribute
   '((language) @font-lock-type-face
     [(class) (class_name)] @font-lock-type-face
     (identifier) @font-lock-variable-name-face
     (key_value (key) @font-lock-property-name-face)
     (key_value (value) @font-lock-string-face))

   :language 'djot
   :feature 'delimiter
   :override t
   '([(div_marker_begin) (div_marker_end)
      (code_block_marker_begin) (code_block_marker_end)
      (raw_block_marker_begin) (raw_block_marker_end)
      (frontmatter_marker) (block_quote_marker)
      (emphasis_begin) (emphasis_end) (strong_begin) (strong_end)
      (superscript_begin) (superscript_end)
      (subscript_begin) (subscript_end)
      (highlighted_begin) (highlighted_end)
      (insert_begin) (insert_end) (delete_begin) (delete_end)
      (verbatim_marker_begin) (verbatim_marker_end)
      (math_marker) (math_marker_begin) (math_marker_end)
      (raw_inline_marker_begin) (raw_inline_marker_end)
      (footnote_marker_begin) (footnote_marker_end)
      (table_separator) (thematic_break) (hard_line_break)]
     @shadow)

   :language 'djot
   :feature 'list
   '([(list_marker_dash) (list_marker_plus) (list_marker_star)
      (list_marker_definition) (list_marker_task)
      (list_marker_decimal_period) (list_marker_decimal_paren)
      (list_marker_decimal_parens)
      (list_marker_lower_alpha_period) (list_marker_lower_alpha_paren)
      (list_marker_lower_alpha_parens)
      (list_marker_upper_alpha_period) (list_marker_upper_alpha_paren)
      (list_marker_upper_alpha_parens)
      (list_marker_lower_roman_period) (list_marker_lower_roman_paren)
      (list_marker_lower_roman_parens)
      (list_marker_upper_roman_period) (list_marker_upper_roman_paren)
      (list_marker_upper_roman_parens)]
     @font-lock-builtin-face)

   :language 'djot
   :feature 'block
   :override t
   '([(code_block) (raw_block)] @djot-ts-mode--fontify-code-block))
  "Tree-sitter font-lock settings for `djot-ts-mode'.")

(defun djot-ts-mode--heading-name (node)
  "Return the heading text of section NODE."
  (when-let* ((heading (treesit-node-child-by-field-name node "heading"))
              (content (treesit-node-child-by-field-name heading "content")))
    (string-trim (treesit-node-text content t))))

;;; Outline

;; Outline headings are the headings of sections, not the sections:
;; a section starts at the block attributes above its heading.

(defun djot-ts-mode--section-heading-p (node)
  "Return non-nil if NODE is the heading of a section."
  (and (equal (treesit-node-type node) "heading")
       (equal (treesit-node-type (treesit-node-parent node)) "section")))

(defun djot-ts-mode--outline-level ()
  "Return the number of sections enclosing the heading at point."
  (let ((node (treesit-node-at (pos-bol)))
        (level 0))
    (while (setq node (treesit-parent-until node "\\`section\\'"))
      (setq level (1+ level)))
    (max level 1)))

;;; Folding of blocks with hideshow

;; Top-level headings are `section' nodes and fold with outline.  Divs,
;; code blocks, raw blocks and headings inside divs (which the grammar
;; does not wrap in sections) fold with hideshow.  A block starts at its
;; opener, after the block attributes it contains.

(defconst djot-ts-mode--block-regexp
  "\\`\\(?:div\\|code_block\\|raw_block\\|block_attribute\\)\\'"
  "Node types folded by hideshow as blocks.")

(defun djot-ts-mode--nested-heading-p (node)
  "Return non-nil if NODE is a heading outside a section."
  (and (equal (treesit-node-type node) "heading")
       (not (equal (treesit-node-type (treesit-node-parent node)) "section"))))

(defun djot-ts-mode--multiline-p (node)
  "Return non-nil if NODE ends on a later line than it starts."
  (save-excursion
    (goto-char (treesit-node-start node))
    (< (pos-eol) (treesit-node-start (treesit-node-child node -1)))))

(defun djot-ts-mode--foldable-p (node)
  "Return non-nil if NODE is folded by hideshow.
A one-line block attribute is not, so its block folds instead."
  (if (equal (treesit-node-type node) "block_attribute")
      (djot-ts-mode--multiline-p node)
    (or (string-match-p djot-ts-mode--block-regexp (treesit-node-type node))
        (djot-ts-mode--nested-heading-p node))))

(defun djot-ts-mode--opener-p (node)
  "Return non-nil if NODE is the opener of a foldable block."
  (if (equal (treesit-node-type node) "block_attribute")
      (djot-ts-mode--foldable-p node)
    (when-let* ((parent (treesit-node-parent node)))
      (and (djot-ts-mode--foldable-p parent)
           (treesit-node-eq node (djot-ts-mode--opener parent))))))

(defun djot-ts-mode--heading-level (node)
  "Return the level of heading NODE."
  (length (string-trim
           (treesit-node-text (treesit-node-child-by-field-name node "marker") t))))

(defun djot-ts-mode--fold-end (node)
  "Return the position where the fold of NODE ends.
For a block, this is after its closing marker.  For a block attribute,
this is before its closing brace.  For a nested heading, this is before
the next sibling heading of the same or lower level."
  (if (equal (treesit-node-type node) "block_attribute")
      (treesit-node-start (treesit-node-child node -1))
   (let ((end
         (if (djot-ts-mode--nested-heading-p node)
             (let ((level (djot-ts-mode--heading-level node))
                   (last node) next)
               (while (and (setq next (treesit-node-next-sibling last))
                           (not (and (equal (treesit-node-type next) "heading")
                                     (<= (djot-ts-mode--heading-level next) level))))
                 (setq last next))
               (treesit-node-end last))
           (let ((close (treesit-node-child node -1)))
             (if (string-suffix-p "_end" (treesit-node-type close))
                 (treesit-node-end close)
               (treesit-node-end node))))))
    (save-excursion (goto-char end) (skip-chars-backward " \t\n") (point)))))

(defun djot-ts-mode--hs-forward (&optional _arg)
  "Move to the fold end of the block at point.  For `hs-forward-sexp-function'."
  (when-let* ((node (treesit-thing-at (point) #'djot-ts-mode--foldable-p)))
    (goto-char (djot-ts-mode--fold-end node))))

(defun djot-ts-mode--hs-block-end ()
  "Match the empty string at point.  For `hs-block-end-regexp'."
  (set-match-data (list (point) (point)))
  t)

(defun djot-ts-mode--hs-find-block-beginning ()
  "Move to the start of the innermost foldable block around point.
Inside a div, this is the nested heading whose body contains point,
if any.  For `hs-find-block-beginning-function'."
  (when-let* ((pos (point))
              (node (treesit-thing-at pos #'djot-ts-mode--foldable-p)))
    (when (equal (treesit-node-type node) "div")
      (let ((child (treesit-node-at pos)))
        (while (and child (not (equal (treesit-node-type (treesit-node-parent child))
                                      "content")))
          (setq child (treesit-node-parent child)))
        (when (and child (treesit-node-eq
                          (treesit-node-parent (treesit-node-parent child)) node))
          (let ((sib child) found)
            (while (and (not found) (setq sib (treesit-node-prev-sibling sib)))
              (when (and (djot-ts-mode--nested-heading-p sib)
                         (>= (djot-ts-mode--fold-end sib) pos))
                (setq found sib)))
            (when found (setq node found))))))
    (goto-char (treesit-node-start (djot-ts-mode--opener node)))
    t))

(defun djot-ts-mode--hs-looking-at-block-start ()
  "Return non-nil if a block opener starts at point.
For `hs-looking-at-block-start-predicate'."
  (when-let* ((opener (treesit-thing-at (point) #'djot-ts-mode--opener-p))
              ((= (treesit-node-start opener) (point))))
    (set-match-data (list (point) (min (1+ (point)) (point-max))))
    t))

(defun djot-ts-mode--hs-find-next-block (_regexp maxp _comments)
  "Move past the start of the next block opener before MAXP.
For `hs-find-next-block-function'."
  (let* ((current (treesit-thing-at (point) #'djot-ts-mode--opener-p))
         (beg (if (and current (= (treesit-node-start current) (point)))
                  (point)
                (treesit-navigate-thing (point) 1 'beg #'djot-ts-mode--opener-p)))
         (end (and beg (min (1+ beg) (point-max)))))
    (when (and end (<= end maxp))
      (goto-char end)
      (set-match-data (list beg end beg end))
      t)))

(defun djot-ts-mode--outline-toggle ()
  "Toggle the body of the current section, keeping its children's state.
Unlike `outline-hide-subtree', hiding does not remove the folds inside
the section, so they come back unchanged when it is shown again."
  (outline-back-to-heading)
  (let* ((beg (pos-eol))
         (end (save-excursion (outline-end-of-subtree) (point)))
         (ovs (seq-filter (lambda (o)
                            (and (eq (overlay-get o 'invisible) 'outline)
                                 (= (overlay-start o) beg)))
                          (overlays-at beg))))
    (cond (ovs (mapc #'delete-overlay ovs))
          ((< beg end)
           (let ((o (make-overlay beg end nil 'front-advance)))
             (overlay-put o 'evaporate t)
             (overlay-put o 'invisible 'outline)
             (overlay-put o 'isearch-open-invisible
                          (or outline-isearch-open-invisible-function
                              #'outline-isearch-open-invisible)))))
    (when (fboundp 'outline--fix-buttons)
      (outline--fix-buttons beg end))
    (run-hooks 'outline-view-change-hook)))

(defun djot-ts-mode-toggle ()
  "Fold or unfold the block or section at point.
Folds inside it keep their state.  Use hideshow inside divs, code
blocks and block attributes, and outline elsewhere."
  (interactive)
  (if (and (not (outline-on-heading-p))
           (save-excursion (hs-get-near-block)))
      (hs-toggle-hiding)
    (djot-ts-mode--outline-toggle)))

(defvar-keymap djot-ts-mode-map
  :doc "Keymap for `djot-ts-mode'."
  :parent outline-mode-map
  "<tab>" #'djot-ts-mode-toggle)

;;;###autoload
(define-derived-mode djot-ts-mode outline-mode "Djot"
  "Major mode for Djot, powered by tree-sitter."
  :syntax-table djot-ts-mode--syntax-table
  (when (treesit-ensure-installed 'djot)
    (setq treesit-primary-parser (treesit-parser-create 'djot))
    (setq-local comment-start "{% "
                comment-end " %}"
                comment-start-skip "{%+[ \t]*"
                comment-end-skip "[ \t]*%+}")
    (setq-local font-lock-extra-managed-props
                (cons 'display font-lock-extra-managed-props))
    (setq-local treesit-font-lock-settings djot-ts-mode--font-lock-settings
                treesit-font-lock-feature-list
                '((comment heading block)
                  (markup link)
                  (attribute delimiter list)))
    (setq-local treesit-defun-name-function #'djot-ts-mode--heading-name
                treesit-simple-imenu-settings
                '((nil "\\`section\\'" nil nil))
                treesit-outline-predicate #'djot-ts-mode--section-heading-p)
    (treesit-major-mode-setup)
    ;; `outline-mode' sets these already, so `treesit-major-mode-setup'
    ;; leaves them alone.
    (setq-local outline-search-function #'treesit-outline-search
                outline-level #'djot-ts-mode--outline-level)
    (setq-local hs-treesit-things #'djot-ts-mode--foldable-p
                hs-c-start-regexp nil
                hs-block-start-regexp nil
                hs-block-end-regexp #'djot-ts-mode--hs-block-end
                hs-forward-sexp-function #'djot-ts-mode--hs-forward
                hs-find-block-beginning-function #'djot-ts-mode--hs-find-block-beginning
                hs-find-next-block-function #'djot-ts-mode--hs-find-next-block
                hs-looking-at-block-start-predicate
                #'djot-ts-mode--hs-looking-at-block-start
                hs-inside-comment-predicate #'ignore)
    (setq-local hs-allow-nesting t)
    (hs-minor-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.dj\\'" . djot-ts-mode))

(provide 'djot-ts-mode)
;;; djot-ts-mode.el ends here
