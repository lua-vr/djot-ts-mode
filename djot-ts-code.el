;;; djot-ts-code.el --- Tree-sitter highlighting of Djot code blocks -*- lexical-binding: t -*-

;;; Commentary:

;; Highlight code and raw blocks whose language has a tree-sitter
;; major mode, with that mode's font-lock rules.  Each block gets its
;; own local parser.  Language tags map to modes as in Org source
;; blocks (`org-src-get-lang-mode'), then through
;; `treesit-major-mode-remap-alist'.  Blocks in other languages keep
;; only the code block faces.

;;; Code:

(require 'treesit)
(require 'org-src)

(declare-function djot-ts-mode--opener "djot-ts-mode" (node))
(defvar warning-minimum-level)
(defvar warning-minimum-log-level)

(defcustom djot-ts-code-fontify-natively t
  "Non-nil means highlight code blocks with their language's tree-sitter mode.
Takes effect when `djot-ts-mode' is next turned on."
  :type 'boolean
  :group 'djot-ts)

(defvar djot-ts-code--modes (make-hash-table :test #'equal)
  "Map language tags to major modes, or to nil if there is none.")

(defvar djot-ts-code--configs (make-hash-table :test #'eq)
  "Map major modes to (LANGUAGE SETTINGS FEATURES), or to `none'.
SETTINGS and FEATURES are the mode's `treesit-font-lock-settings' and
`treesit-font-lock-feature-list'.  `none' means the mode has no
tree-sitter font-lock.")

(defvar-local djot-ts-code--languages nil
  "Tree-sitter languages whose font-lock settings this buffer has.")

(defun djot-ts-code--mode (tag)
  "Return the major mode for language TAG, a string, or nil.
This is the mode of `org-src-get-lang-mode', remapped by
`treesit-major-mode-remap-alist', or else TAG-ts-mode if its library
exists."
  (let ((mode (org-src-get-lang-mode tag))
        (ts-mode (intern (concat (downcase tag) "-ts-mode"))))
    (cond ((alist-get mode treesit-major-mode-remap-alist))
          ((and (not (string-suffix-p "-ts-mode" (symbol-name mode)))
                (or (fboundp ts-mode)
                    (and (locate-library (symbol-name ts-mode))
                         (require ts-mode nil t)
                         (fboundp ts-mode))))
           ts-mode)
          ((fboundp mode) mode))))

(defun djot-ts-code--harvest (mode)
  "Return (LANGUAGE SETTINGS FEATURES) of MODE, or `none'.
Run MODE in a work buffer, without hooks, messages or grammar
installation prompts."
  (with-work-buffer
    (let ((treesit-auto-install-grammar 'never)
          (inhibit-message t)
          (message-log-max nil)
          (warning-minimum-level :emergency)
          (warning-minimum-log-level :emergency))
      (ignore-errors (delay-mode-hooks (funcall mode))))
    (let ((parser (or treesit-primary-parser (car (treesit-parser-list)))))
      (if (and parser treesit-font-lock-settings)
          (list (treesit-parser-language parser)
                (mapcar #'copy-sequence treesit-font-lock-settings)
                treesit-font-lock-feature-list)
        'none))))

(defun djot-ts-code--add (language settings features)
  "Add font-lock SETTINGS and FEATURES of LANGUAGE to the buffer.
SETTINGS go before the `block' settings of Djot, so the code block
background is appended to their faces."
  (push language djot-ts-code--languages)
  (unless (seq-some (lambda (s) (eq (treesit-font-lock-setting-language s)
                                    language))
                    treesit-font-lock-settings)
    (let ((i (or (seq-position
                  treesit-font-lock-settings nil
                  (lambda (s _)
                    (and (eq (treesit-font-lock-setting-language s) 'djot)
                         (eq (treesit-font-lock-setting-feature s) 'block))))
                 (length treesit-font-lock-settings))))
      (setq-local treesit-font-lock-settings
                  (append (take i treesit-font-lock-settings)
                          settings
                          (nthcdr i treesit-font-lock-settings))))
    (setq-local treesit-font-lock-feature-list
                (treesit-merge-font-lock-feature-list
                 treesit-font-lock-feature-list features))))

(defun djot-ts-code--language (node)
  "Return the tree-sitter language for language tag NODE, or nil.
The first time a language occurs in the buffer, add its font-lock
settings."
  (when-let* ((tag (treesit-node-text node t))
              (mode (with-memoization (gethash tag djot-ts-code--modes)
                      (djot-ts-code--mode tag)))
              (config (with-memoization (gethash mode djot-ts-code--configs)
                        (djot-ts-code--harvest mode)))
              ((consp config)))
    (let ((language (car config)))
      (unless (memq language djot-ts-code--languages)
        (apply #'djot-ts-code--add config))
      language)))

(defun djot-ts-code--ranges (node _offset)
  "Return the ranges of code NODE, for `treesit-range-rules'.
On each line, leave out the block quote markers and the whitespace
left of the opening fence."
  (let ((col (save-excursion
               (goto-char (treesit-node-start
                           (djot-ts-mode--opener (treesit-node-parent node))))
               (current-column)))
        (end (treesit-node-end node))
        (markers (mapcar #'treesit-node-end
                         (treesit-filter-child
                          node (lambda (n) (equal (treesit-node-type n)
                                                  "block_quote_marker")))))
        ranges)
    (save-excursion
      (goto-char (treesit-node-start node))
      (while (< (point) end)
        (let ((eol (min end (1+ (pos-eol)))))
          (while (and markers (<= (car markers) eol))
            (goto-char (max (point) (pop markers))))
          (while (and (< (current-column) col) (memq (char-after) '(?\s ?\t)))
            (forward-char))
          (when (< (point) eol)
            (push (cons (point) eol) ranges))
          (goto-char eol))))
    (or (nreverse ranges)
        (cons (treesit-node-start node) end))))

(defun djot-ts-code-setup ()
  "Set up tree-sitter highlighting of code blocks in the current buffer.
Call before `treesit-major-mode-setup'."
  (when djot-ts-code-fontify-natively
    (setq-local treesit-range-settings
                (treesit-range-rules
                 :embed #'djot-ts-code--language
                 :host 'djot
                 :local t
                 :range-fn #'djot-ts-code--ranges
                 '((code_block language: (language) @language
                               code: (code) @content)
                   (raw_block info: (raw_block_info language: (language) @language)
                              content: (content) @content)))
                ;; Commands keep seeing the Djot tree inside code blocks.
                treesit-language-at-point-function (lambda (_) 'djot))))

(provide 'djot-ts-code)
;;; djot-ts-code.el ends here
