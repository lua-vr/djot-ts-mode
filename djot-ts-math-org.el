;;; djot-ts-math-org.el --- Org LaTeX preview backend for djot-ts-math -*- lexical-binding: t -*-

;; Author: Lua <me@lua.blog.br>
;; Version: 0.1
;; Package-Requires: ((emacs "31.1") (org "9.8pre"))
;; Keywords: text, tex

;;; Commentary:

;; The `org' backend of `djot-ts-math-mode', rendering with
;; `org-latex-preview-place'.  The images follow the `org-latex-preview'
;; options: `org-latex-preview-process-default',
;; `org-latex-preview-appearance-options', `org-latex-preview-preamble'...
;; Uses internals of org-latex-preview.el (Org 9.8).

;;; Code:

(require 'djot-ts-math)
(require 'org-latex-preview)

(defvar djot-ts-math-org--preamble nil
  "Cached LaTeX preamble.  Set to nil to recompute it.")

(defun djot-ts-math-org--preamble ()
  "Return the preamble of an empty Org buffer, cached."
  (or djot-ts-math-org--preamble
      (setq djot-ts-math-org--preamble
            (with-temp-buffer
              (let ((org-mode-hook nil))
                (org-mode))
              (org-latex-preview--get-preamble)))))

(defun djot-ts-math-org--place (entries)
  "Preview ENTRIES with `org-latex-preview-place'."
  (org-latex-preview-place
   org-latex-preview-process-default
   (mapcar (pcase-lambda (`(,beg ,end ,tex ,display))
             (list beg end (format (if display "\\[%s\\]" "\\(%s\\)") tex)))
           entries)
   nil (djot-ts-math-org--preamble)))

(defun djot-ts-math-org--overlay-p (ov)
  "Non-nil if OV is an Org LaTeX preview overlay."
  (eq (overlay-get ov 'org-overlay-type) 'org-latex-overlay))

;; As `org-latex-preview-mode--open-this-overlay'.
(defun djot-ts-math-org--reveal (ov)
  "Show the source under OV."
  (overlay-put ov 'display nil)
  (overlay-put ov 'org-view-text t)
  (when-let* ((f (overlay-get ov 'face)))
    (overlay-put ov 'org-hidden-face f)
    (overlay-put ov 'face nil)))

;; As `org-latex-preview-mode--close-previous-overlay'.
(defun djot-ts-math-org--conceal (ov)
  "Show the image of OV."
  (overlay-put ov 'org-view-text nil)
  (when-let* ((f (overlay-get ov 'org-hidden-face)))
    (unless (eq f 'org-latex-preview-processing-face)
      (overlay-put ov 'face f))
    (overlay-put ov 'org-hidden-face nil))
  (overlay-put ov 'display (overlay-get ov 'org-preview-image)))

(defun djot-ts-math-org--image (ov)
  "Return the image spec of OV."
  (overlay-get ov 'org-preview-image))

(defun djot-ts-math-org--setup ()
  "Report arriving images to `djot-ts-math'."
  (add-hook 'org-latex-preview-overlay-update-functions
            #'djot-ts-math-image-updated nil t))

(defun djot-ts-math-org--teardown ()
  "Undo `djot-ts-math-org--setup'."
  (remove-hook 'org-latex-preview-overlay-update-functions
               #'djot-ts-math-image-updated t))

(djot-ts-math-define-backend
 'org
 :place #'djot-ts-math-org--place
 :overlay-p #'djot-ts-math-org--overlay-p
 :reveal #'djot-ts-math-org--reveal
 :conceal #'djot-ts-math-org--conceal
 :image #'djot-ts-math-org--image
 :setup #'djot-ts-math-org--setup
 :teardown #'djot-ts-math-org--teardown)

(provide 'djot-ts-math-org)
;;; djot-ts-math-org.el ends here
