;;; plz-test.el --- Tests helpers for plz -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: r0man <r0man@burningswell.com>
;; Maintainer: r0man <r0man@burningswell.com>

;; This file is part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains tests helpers for plz.

;;; Code:

;;;; Requirements

(require 'ert)
(require 'json)
(require 'let-alist)
(require 'map)

(require 'plz)

;;;; Variables

(defvar plz-test-openai-token
  (auth-source-pick-first-password :host "openai.com" :user "ellama"))

(defvar plz-test-uri-prefix
  ;; "https://httpbin.org"
  "http://localhost"
  "URI prefix for HTTP requests, without trailing slash.
If running httpbin locally, set to \"http://localhost\".")

;;;; Customization


;;;; Commands


;;;; Macros

(cl-defun plz-test-wait (process &optional (seconds 0.1) (times 100))
  "Wait for SECONDS seconds TIMES times for PROCESS to finish."
  (when process
    ;; Sometimes it seems that the process is killed, the THEN
    ;; function called by its sentinel, and its buffer killed, all
    ;; before this function gets called with the process argument;
    ;; when that happens, tests that use this can fail.  Testing
    ;; whether PROCESS is non-nil seems to fix it, but it's possible
    ;; that something funny is going on...
    (cl-loop for i upto times ;; 10 seconds
             while (equal 'run (process-status process))
             do (sleep-for seconds))))

(cl-defmacro plz-deftest (name () &body docstring-keys-and-body)
  "Like `ert-deftest', but defines tests for both HTTP/1.1 and HTTP/2.
Also defines local function `url' which returns its argument
appended to `plz-test-uri-prefix' (and any instance of
\"URI-PREFIX\" in URL-PART is replaced with `plz-test-uri-prefix'
in URL-encoded form)."
  (declare (debug (&define [&name "test@" symbolp]
			   sexp [&optional stringp]
			   [&rest keywordp sexp] def-body))
           (doc-string 3)
           (indent 2))
  `(progn
     ,@(cl-loop for http-version in '("1.1" "2")
                collect (let ((name (intern (format "%s-http%s" name http-version))))
                          `(ert-deftest ,name ()
                             (let ((plz-curl-default-args
                                    ',(append plz-curl-default-args (list (format "--http%s" http-version)))))
                               (cl-labels ((url (part)
                                             (setf part (replace-regexp-in-string
                                                         "URI-PREFIX" (url-hexify-string plz-test-uri-prefix)
                                                         part t t))
                                             (concat plz-test-uri-prefix part)))
                                 ,@docstring-keys-and-body)))))))

;;;; Footer

(provide 'plz-test)

;;; plz-test.el ends here
