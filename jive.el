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
;; Package-Requires: ((emacs "27.1") (popup))
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
(require 'popup)

(defvar jive--process nil)
(defvar jive--process-buffer "*jive-stdout-stderr*")
(defvar jive--replete-js-path nil)
(defvar jive--region-beginning nil)
(defvar jive--region-end nil)

(defun jive--propertize-error (error) (propertize error 'face '(:foreground "red")))
(defun jive--append-to-process-buffer (value)
  (with-current-buffer jive--process-buffer
    (goto-char (point-max)) ; Append to buffer
    (insert value ?\n)))

(defun jive--process-filter (_process data-raw)
  (dolist (portion (cl-remove-if (lambda (s) (<= (string-width s) 0))
                                 (split-string data-raw "\n")))
    (let* ((data (json-parse-string portion :object-type 'plist))
           (data-type (plist-get data :type))
           (data-value (plist-get data :string)))
      (pcase data-type
        ("out"          (jive--append-to-process-buffer data-value))
        ("err"          (jive--append-to-process-buffer (jive--propertize-error data-value)))
        ("exception"    (progn (jive--append-to-process-buffer (jive--propertize-error data-value))
                               (popup-tip (format "Error: See the %s buffer for details" jive--process-buffer)
                                          :point jive--region-end)))
        ("evaluation"   (popup-tip (concat "=> " data-value) :point jive--region-end))
        (_              (jive--append-to-process-buffer
                         (jive--propertize-error (format "Unexpected data type: %s. Data = %s" data-type portion))))))))

(defun jive-start ()
  "Start JIVE"
  (interactive)
  (unless (and jive--process (process-live-p jive--process))
    (if jive--replete-js-path
        (setq jive--process (make-process :name "jive"
                                          :buffer jive--process-buffer
                                          :filter #'jive--process-filter
                                          :command (list "node" jive--replete-js-path "--experimental-import-meta-resolve" )))
      (user-error "jive--replete-js-path needs to be set!"))))

(defun jive-stop ()
  "Stop JIVE"
  (interactive)
  (when (and jive--process (process-live-p jive--process))
    (kill-process jive--process)))

(defun jive--eval-region (platform)
  (setq jive--region-beginning (region-beginning)
        jive--region-end (region-end))
  (let ((selected-code (format "%s" (buffer-substring-no-properties jive--region-beginning
                                                                    jive--region-end))))
    (process-send-string jive--process (concat (json-serialize (list :platform platform
                                                                     :source selected-code
                                                                     :locator (buffer-file-name)))
                                               "\n"))))

(defun jive-node-eval-region ()
  "Evaluate the selected JS code via Node"
  (interactive)
  (jive--eval-region "node"))

(defun jive-deno-eval-region ()
  "Evaluate the selected TS code via Deno"
  (interactive)
  (jive--eval-region "deno"))

;; TODO
;; (defun jive-browser-eval-region ()
;;   "Evaluate the selected JS code via browser"
;;   (interactive)
;;   (jive--eval-region "browser"))

(provide 'jive)
;;; jive.el ends here
