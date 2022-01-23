;;; jive.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Rafael Nicdao
;;
;; Author: Rafael Nicdao <https://github.com/anonimitoraf>
;; Maintainer: Rafael Nicdao <nicdaoraf@gmail.com>
;; Created: January 01, 2022
;; Modified: January 01, 2022
;; Version: 0.0.1
;; Keywords: javascript js repl repl-driven
;; Homepage: https://github.com/anonimitoraf/jive
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

(defface jive-result-overlay-face
  '((((class color) (background light))
     :background "grey90"
     :foreground "black"
     :box (:line-width -1 :color "#ECBE7B"))
    (((class color) (background dark))
     :background "grey10"
     :foreground "#ECBE7B"
     :box (:line-width -1 :color "#ECBE7B")))
  "Face used to display evaluation results at the end of line."
  :group 'jive)

(defvar jive--server-url "http://localhost:4321")
(defvar jive--process-buffer "*jive-stdout-stderr*")
(defvar jive--eval-overlay nil)

(defun jive--propertize-error (error) (propertize error 'face '(:foreground "red")))

(defun jive--display-overlay (value face)
  (overlay-put jive--eval-overlay 'before-string
               (propertize value 'face face)))

(defun jive--append-to-process-buffer (value)
  (with-current-buffer (get-buffer-create jive--process-buffer)
    (goto-char (point-max)) ; Append to buffer
    (insert value ?\n)))

(defun jive--process-server-response (response)
  (let* ((stdout (alist-get 'stdout response))
          (stderr (alist-get 'stderr response))
          (result (alist-get 'result response)))
    (when stdout (jive--append-to-process-buffer stdout))
    (when stderr (jive--append-to-process-buffer (jive--propertize-error stderr)))
    (jive--display-overlay (format " => %s " (if result result "undefined")) 'jive-result-overlay-face)))

(defun jive--send-eval-req (code module-path)
  "Send CODE and MODULE-PATH to sever."
  (request
    (concat jive--server-url "/eval")
    :type "POST"
    :data (json-encode `(("code" . ,code) ("modulePath" . ,module-path)))
    :parser 'json-read
    :encoding 'utf-8
    :headers '(("Content-Type" . "application/json"))
    :success (cl-function (lambda (&key data &allow-other-keys)
                            ;; (message "DATA %s" data)
                            (jive--process-server-response data)))))

(defun jive-remove-eval-overlay ()
  (interactive)
  (when (overlayp jive--eval-overlay) (delete-overlay jive--eval-overlay)))

(defun jive-eval-region ()
  "Evaluate the selected JS code."
  (interactive)
  (let* ((beg (region-beginning))
         (end (region-end))
         (selected-code (format "%s" (buffer-substring-no-properties beg end))))
    ;; Clean up previous eval overlay
    (jive-remove-eval-overlay)
    (save-excursion
      (goto-char end)
      ;; Make sure the overlay is actually at the end of the evaluated region, not on a newline
      (skip-chars-backward "\r\n[:blank:]")
      ;; Seems like the END arg of make-overlay is useless. Just use the same value as BEGIN
      (setq jive--eval-overlay (make-overlay (point) (point) (current-buffer))))
    (jive--send-eval-req selected-code (buffer-file-name))))

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

(provide 'jive)
;;; jive.el ends here
