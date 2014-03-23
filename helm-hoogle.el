;;; helm-hoogle.el --- Source and configured helm for searching hoogle:

;;; Commentary:
;; This package provides a helm source `helm-source-hoogle' and a
;; configured helm `helm-hoogle' to search hoogle offline.

;;; Code:
(with-no-warnings
  (require 'cl))

(defvar helm-hoogle-executable-args
  )

(defvar helm-source-hoogle
  '((name . "Hoogle")
    (volatile)
    (requires-pattern . 2)
    (candidates-process . helm-hoogle-search)
    (action . (("Kill whole item" . helm-hoogle--kill-item)
               ("Kill type" . helm-hoogle--kill-type)
               ("Kill name" . helm-hoogle--kill-name)
               ("Kill module" . helm-hoogle--kill-module)))))

(defun helm-hoogle-search ()
  (with-temp-buffer
    (call-process "hoogle" nil t nil "search" "-n 50"
                  helm-pattern)
    (let ((lines (split-string
                  (buffer-substring-no-properties (point-min) (point-max)) "\n")))
      (cl-loop for s in lines collect `(,s . ,s)))))

(defun helm-hoogle-action-on--item (fun item)
  (funcall fun item))

(defun helm-hoogle-action-on--item-type (fun item)
  (funcall fun (replace-regexp-in-string ".* :: ?" "" item)))

(defun helm-hoogle-action-on--item-name (fun item)
  (funcall fun (progn
                 (string-match "[^ ] \\(.*?\\) ::.*" item)
                 (match-string 1 item))))

(defun helm-hoogle-action-on--item-module (fun item)
  (funcall fun (progn (string-match "\\(.+?\\) +" item)
                      (match-string 1 item))))

(helm-hoogle-action-on--item-module 'kill-new "Prelude min :: a -> a -> a")

(defun helm-hoogle--kill-item (item)
  (helm-hoogle-action-on--item 'kill-new item))

(defun helm-hoogle--kill-type (item)
  (helm-hoogle-action-on--item-type 'kill-new item))

(defun helm-hoogle--kill-module (item)
  (helm-hoogle-action-on--item-module 'kill-new item))

(defun helm-hoogle--kill-name (item)
  (helm-hoogle-action-on--item-name 'kill-new item))

(defun helm-hoogle ()
  (interactive)
  (helm :sources '(helm-source-hoogle)))

(provide 'helm-hoogle)
