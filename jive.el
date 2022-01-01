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

(require 'popup)

(defvar jive--process nil)
(defvar jive--process-buffer "*jive-stdout-stderr*")
(defvar jive--replete-js-path nil)
(defvar jive--region-beginning nil)
(defvar jive--region-end nil)

(defun jive--process-filter (_process output)
  ;; TODO Exempt the initial messages
  ;; TODO Remove repetitive code
  (let* ((stdout+return (when (string-match "<replete_stdout>\\(\\(?:.\\|\n\\)*\\)\n</replete_stdout>\\(.*\\)" output)
                          (list (match-string-no-properties 1 output)
                                (match-string-no-properties 2 output))))
         (stdout (car stdout+return))
         (stderr+return (when (string-match "<replete_stderr>\\(\\(?:.\\|\n\\)*\\)\n</replete_stderr>\\(.*\\)" output)
                          (list (match-string-no-properties 1 output)
                                (match-string-no-properties 2 output))))
         (stderr (car stderr+return))
         (return (or (cadr stdout+return) (cadr stdout+return) output)))
    (with-current-buffer jive--process-buffer
      (goto-char (point-max))
      (when stdout (insert stdout ?\n))
      (when stderr (insert (propertize stderr 'face '(:foreground "red")))))
    (popup-tip (concat "=> " return) :point jive--region-end)))

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

(defun jive-eval-region ()
  "Evaluate the selected JS code"
  (interactive)
  (setq jive--region-beginning (region-beginning)
        jive--region-end (region-end))
  (let ((selected-code (format "%s" (buffer-substring-no-properties jive--region-beginning
                                                                    jive--region-end))))
    ;; TODO Un-hardcode platform
    ;; TODO Locator/file path
    (process-send-string jive--process (concat (json-serialize (list :platform "node"
                                                                     :source selected-code))
                                               "\n"))))

(provide 'jive)
;;; jive.el ends here
