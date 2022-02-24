;;; skerrick.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimitoraf>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: January 01, 2022
;; Modified: January 01, 2022
;; Version: 0.0.1
;; Keywords: javascript js repl repl-driven
;; Homepage: https://github.com/anonimitoraf/skerrick
;; Package-Requires: ((emacs "27.1") (request "0.2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;; REPL-driven development for Javascript
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'request)

(defface skerrick-result-overlay-face
  '((((class color) (background light))
     :background "grey90"
     :foreground "black"
     :box (:line-width -1 :color "#ECBE7B"))
    (((class color) (background dark))
     :background "grey10"
     :foreground "#ECBE7B"
     :box (:line-width -1 :color "#ECBE7B")))
  "Face used to display evaluation results at the end of line."
  :group 'skerrick)

(defvar skerrick--server-url "http://localhost:4321")
(defvar skerrick--process-buffer "*skerrick-stdout-stderr*")
(defvar skerrick--eval-overlay nil)
(defvar skerrick--remove-eval-overlay-on-next-cmd? nil)

(defun skerrick--propertize-error (error) (propertize error 'face '(:foreground "red")))

(defun skerrick--display-overlay (value face)
  (overlay-put skerrick--eval-overlay 'before-string
               (propertize value 'face face)))

(defun skerrick--append-to-process-buffer (value)
  (with-current-buffer (get-buffer-create skerrick--process-buffer)
    (goto-char (point-max)) ; Append to buffer
    (insert value ?\n)))

(defun skerrick--process-server-response (response)
  (let* ((stdout (alist-get 'stdout response))
          (stderr (alist-get 'stderr response))
          (result (alist-get 'result response)))
    (when stdout (skerrick--append-to-process-buffer stdout))
    (when stderr (skerrick--append-to-process-buffer (skerrick--propertize-error stderr)))
    (skerrick--display-overlay (format " => %s " (if result (json-encode result) "undefined")) 'skerrick-result-overlay-face)
    (setq skerrick--remove-eval-overlay-on-next-cmd? t)))

(defun skerrick--send-eval-req (code module-path)
  "Send CODE and MODULE-PATH to sever."
  (request
    (concat skerrick--server-url "/eval")
    :type "POST"
    :data (json-encode `(("code" . ,code)
                          ("modulePath" . ,module-path)))
    :parser 'json-read
    :encoding 'utf-8
    :headers '(("Content-Type" . "application/json"))
    :success (cl-function (lambda (&key data &allow-other-keys)
                            ;; (message "DATA %s" data)
                            (skerrick--process-server-response data)))))

(defun skerrick-remove-eval-overlay ()
  (interactive)
  (when (overlayp skerrick--eval-overlay)
    (delete-overlay skerrick--eval-overlay)
    (setq skerrick--remove-eval-overlay-on-next-cmd? nil)))

(add-hook 'post-command-hook (lambda () (when skerrick--remove-eval-overlay-on-next-cmd?
                                     (skerrick-remove-eval-overlay))))

(defun skerrick-eval-region ()
  "Evaluate the selected JS code."
  (interactive)
  (let* ((beg (region-beginning))
          (end (region-end))
          (selected-code (format "%s" (buffer-substring-no-properties beg end))))
    ;; Clean up previous eval overlay
    (skerrick-remove-eval-overlay)
    (save-excursion
      (goto-char end)
      ;; Make sure the overlay is actually at the end of the evaluated region, not on a newline
      (skip-chars-backward "\r\n[:blank:]")
      ;; Seems like the END arg of make-overlay is useless. Just use the same value as BEGIN
      (setq skerrick--eval-overlay (make-overlay (point) (point) (current-buffer))))
    (skerrick--send-eval-req selected-code (buffer-file-name))))

;; (request
;;   "http://localhost:4321/eval"
;;   :type "POST"
;;   :data (json-encode '(("code" . "throw new Error(\"blah\")") ("modulePath" . "/module-a.js")))
;;   :parser 'json-read
;;   :encoding 'utf-8
;;   :headers '(("Content-Type" . "application/json"))
;;   :success (cl-function (lambda (&key data &allow-other-keys)
;;                           (setq temp-var data))))

;; temp-var

(provide 'skerrick)
;;; skerrick.el ends here
