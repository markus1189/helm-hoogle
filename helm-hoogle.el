;;; helm-hoogle.el --- Use helm to search hoogle

;; Copyright (C) 2014 Markus Hauck

;; Author: Markus Hauck <markus1189@gmail.com>
;; Maintainer: Markus Hauck <markus1189@gmail.com>
;; Keywords: helm hoogle haskell
;; Version: 0.0.1
;; Package-requires: ((helm "1.6.0"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; With this package, you can use `helm-hoogle' to search hoogle as
;; you would do normally and get incremental helm matching.

;;; Code:
(require 'helm)
(require 'cl-lib)

(defvar helm-hoogle-executable-args '("-n" "50"))

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
    (call-process "hoogle" nil t nil "search" (s-join " " helm-hoogle-executable-args)
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
