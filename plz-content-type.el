;;; plz-content-type.el --- plz content types -*- lexical-binding: t; -*-

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

;; This file handles content type.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)
(require 'plz)

(defclass plz:content-type ()
  ((mime-type
    :documentation "The MIME Type of the handler."
    :initarg :mime-type
    :initform "application/octet-stream"
    :type string)))

(cl-defgeneric plz-content-type-else (content-type error)
  "Transform the ERROR into a format suitable for CONTENT-TYPE.")

(cl-defgeneric plz-content-type-then (content-type response)
  "Transform the RESPONSE into a format suitable for CONTENT-TYPE.")

(cl-defgeneric plz-content-type-process (content-type process chunk)
  "Process the CHUNK according to CONTENT-TYPE using PROCESS.")

(defun plz-content-type--from-response (response)
  "Return the content type of RESPONSE, or nil if it's not set."
  (let ((headers (plz-response-headers response)))
    (when-let (header (cdr (assoc 'content-type headers)))
      (replace-regexp-in-string "\s*\\(;.*\\)?" "" header))))

(defun plz-content-type-find (content-types content-type)
  "Lookup the CONTENT-TYPE in CONTENT-TYPES."
  (or (alist-get content-type content-types nil nil #'equal)
      (alist-get t content-types)
      (plz-content-type:application/octet-stream)))

(defun plz-content-type-of-response (content-types response)
  "Lookup the content type of RESPONSE in CONTENT-TYPES."
  (let ((content-type (plz-content-type--from-response response)))
    (plz-content-type-find content-types content-type)))

(defun plz-content-type-process-filter (process content-types chunk)
  "The process filter that handles different content types.

PROCESS is the process.

CONTENT-TYPES is an association list from media type to an
instance of a content type class.

CHUNK is a part of the HTTP body."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (if-let (content-type (process-get process :plz-content-type))
            (let ((response (process-get process :plz-content-type-response)))
              (setf (plz-response-body response) chunk)
              (plz-content-type-process content-type process response))
          (progn
            (save-excursion
              (goto-char (process-mark process))
              (insert chunk)
              (set-marker (process-mark process) (point)))
            (goto-char (point-min))
            (when (re-search-forward plz-http-end-of-headers-regexp nil t)
              (let ((body-start (point)))
                (goto-char (point-min))
                (let* ((response (prog1 (plz--response) (widen)))
                       (content-type (plz-content-type-of-response content-types response)))
                  (when-let (body (plz-response-body response))
                    (when (> (length body) 0)
                      (delete-region body-start (point))
                      (set-marker (process-mark process) (point))
                      (plz-content-type-process content-type process response)))
                  (process-put process :plz-content-type content-type)
                  (setf (plz-response-body response) nil)
                  (process-put process :plz-content-type-response response))))))
        (when moving
          (goto-char (process-mark process)))))))

;; Content Type: application/octet-stream

(defclass plz-content-type:application/octet-stream (plz:content-type)
  ((mime-type :initform "application/octet-stream")))

(cl-defmethod plz-content-type-else ((content-type plz-content-type:application/octet-stream) error)
  "Transform the ERROR into a format suitable for CONTENT-TYPE."
  (let ((response (plz-error-response error)))
    (setf (plz-error-response error) (plz-content-type-then content-type response))
    error))

(cl-defmethod plz-content-type-then ((content-type plz-content-type:application/octet-stream) response)
  "Transform the RESPONSE into a format suitable for CONTENT-TYPE."
  (ignore content-type)
  response)

(cl-defmethod plz-content-type-process ((content-type plz-content-type:application/octet-stream) process chunk)
  "Process the CHUNK according to CONTENT-TYPE using PROCESS."
  (ignore content-type)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          (goto-char (process-mark process))
          (insert (plz-response-body chunk))
          (set-marker (process-mark process) (point)))
        (when moving
          (goto-char (process-mark process)))))))

;; Content Type: application/json

(defclass plz-content-type:application/json (plz-content-type:application/octet-stream)
  ((mime-type :initform "application/json")
   (array-type :initform 'array)
   (false-object :initform :false)
   (null-object :initform :null)
   (object-type :initform 'alist)))

(cl-defmethod plz-content-type-then ((content-type plz-content-type:application/json) response)
  "Transform the RESPONSE into a format suitable for CONTENT-TYPE."
  (with-slots (array-type false-object null-object object-type) content-type
    (setf (plz-response-body response)
          (with-temp-buffer
            (insert (plz-response-body response))
            (goto-char (point-min))
            (json-parse-buffer :array-type array-type
                               :false-object false-object
                               :null-object null-object
                               :object-type object-type)))
    response))

;; Content Type: application/xml

(defclass plz-content-type:application/xml (plz-content-type:application/octet-stream)
  ((mime-type :initform "application/xml")))

(cl-defmethod plz-content-type-then ((content-type plz-content-type:application/xml) response)
  "Transform the RESPONSE into a format suitable for CONTENT-TYPE."
  (with-slots (array-type false-object null-object object-type) content-type
    (setf (plz-response-body response)
          (with-temp-buffer
            (insert (plz-response-body response))
            (libxml-parse-html-region)))
    response))

;; Content Type: text/html

(defclass plz-content-type:text/html (plz-content-type:application/octet-stream)
  ((mime-type :initform "text/html")))

(cl-defmethod plz-content-type-then ((content-type plz-content-type:text/html) response)
  "Transform the RESPONSE into a format suitable for CONTENT-TYPE."
  (with-slots (array-type false-object null-object object-type) content-type
    (setf (plz-response-body response)
          (with-temp-buffer
            (insert (plz-response-body response))
            (libxml-parse-html-region)))
    response))

(defvar plz-content-types
  `(("application/json" . ,(plz-content-type:application/json))
    ("application/octet-stream" . ,(plz-content-type:application/json))
    ("application/xml" . ,(plz-content-type:application/xml))
    ("text/html" . ,(plz-content-type:text/html))
    (t . ,(plz-content-type:application/octet-stream)))
  "Alist from media type to content type.")

(cl-defun plz-content-type-request
    (method
     url
     &rest rest &key headers body else finally noquery
     (as 'string)
     (body-type 'text)
     (connect-timeout plz-connect-timeout)
     (content-types plz-content-types)
     (decode t decode-s)
     (then 'sync)
     (timeout plz-timeout))
  "Request METHOD from URL with curl.
Return the curl process object or, for a synchronous request, the
selected result.

HEADERS may be an alist of extra headers to send with the
request.

BODY may be a string, a buffer, or a list like `(file FILENAME)'
to upload a file from disk.

BODY-TYPE may be `text' to send BODY as text, or `binary' to send
it as binary.

AS selects the kind of result to pass to the callback function
THEN, or the kind of result to return for synchronous requests.
It may be:

- `buffer' to pass the response buffer, which will be narrowed to
  the response body and decoded according to DECODE.

- `binary' to pass the response body as an un-decoded string.

- `string' to pass the response body as a decoded string.

- `response' to pass a `plz-response' structure.

- `file' to pass a temporary filename to which the response body
  has been saved without decoding.

- `(file FILENAME)' to pass FILENAME after having saved the
  response body to it without decoding.  FILENAME must be a
  non-existent file; if it exists, it will not be overwritten,
  and an error will be signaled.

- `(stream :through PROCESS-FILTER)' to asynchronously stream the
  HTTP response.  PROCESS-FILTER is an Emacs process filter
  function, and must accept two arguments: the curl process
  sending the request and a chunk of the HTTP body, which was
  just received.

- A function, which is called in the response buffer with it
  narrowed to the response body (suitable for, e.g. `json-read').

If DECODE is non-nil, the response body is decoded automatically.
For binary content, it should be nil.  When AS is `binary',
DECODE is automatically set to nil.

THEN is a callback function, whose sole argument is selected
above with AS; if the request fails and no ELSE function is
given (see below), the argument will be a `plz-error' structure
describing the error.  Or THEN may be `sync' to make a
synchronous request, in which case the result is returned
directly from this function.

ELSE is an optional callback function called when the request
fails (i.e. if curl fails, or if the HTTP response has a non-2xx
status code).  It is called with one argument, a `plz-error'
structure.  If ELSE is nil, a `plz-curl-error' or
`plz-http-error' is signaled when the request fails, with a
`plz-error' structure as the error data.  For synchronous
requests, this argument is ignored.

NOTE: In v0.8 of `plz', only one error will be signaled:
`plz-error'.  The existing errors, `plz-curl-error' and
`plz-http-error', inherit from `plz-error' to allow applications
to update their code while using v0.7 (i.e. any `condition-case'
forms should now handle only `plz-error', not the other two).

FINALLY is an optional function called without argument after
THEN or ELSE, as appropriate.  For synchronous requests, this
argument is ignored.

CONNECT-TIMEOUT and TIMEOUT are a number of seconds that limit
how long it takes to connect to a host and to receive a response
from a host, respectively.

NOQUERY is passed to `make-process', which see.

When the HTTP response is streamed, the buffering in the curl
output stream is turned off and the PROCESS-FILTER may be called
multiple times, depending on the size of the HTTP body.  It is
the user's responsibility to understand and process each chunk,
and to construct the finalized response if necessary.  There are
no guarantees regarding the chunk, such as being line-based or
not.
\(To silence checkdoc, we mention the internal argument REST.)"
  ;; FIXME(v0.8): Remove the note about error changes from the docstring.
  ;; FIXME(v0.8): Update error signals in docstring.
  (declare (indent defun))
  (ignore as) ;; TODO: Handle as?
  (let ((plz-curl-default-args (cons "--no-buffer" plz-curl-default-args))
        (process))
    (setq process (plz method url
                    :as 'response
                    :body body
                    :body-type body-type
                    :connect-timeout connect-timeout
                    :decode decode
                    :headers headers
                    :noquery noquery
                    :timeout timeout
                    :then (cond ((symbolp then) then)
                                ((functionp then)
                                 (lambda (object)
                                   (let* ((content-type (plz-content-type-of-response content-types object))
                                          (object (plz-content-type-then content-type object)))
                                     (funcall then object)))))
                    :else (lambda (object)
                            (let* ((content-type (plz-content-type-of-response content-types (plz-error-response object)))
                                   (object (plz-content-type-else content-type object)))
                              (when (functionp else)
                                (funcall else object))))
                    :finally (lambda ()
                               (when (functionp finally)
                                 (funcall finally)))))
    (set-process-filter process (lambda (process chunk)
                                  (plz-content-type-process-filter process content-types chunk)))
    process))

;;;; Footer

(provide 'plz-content-type)

;;; plz-content-type.el ends here
