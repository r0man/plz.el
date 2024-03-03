;;; plz-stream.el --- HTTP stream library -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Maintainer: Adam Porter <adam@alphapapa.net>

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

;; This file implements HTTP streaming for plz.el.

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'plz)

(defun plz-stream-handler-default (process response)
  "The default PROCESS filter for a streaming HTTP RESPONSE."
  (save-excursion
    (goto-char (process-mark process))
    (insert (plz-response-body response))
    (set-marker (process-mark process) (point))))

(defvar plz-stream-content-types
  '((t . plz-stream-handler-default))
  "Alist of content types and their corresponding handlers.")

(defun plz-stream--response-content-type (response)
  "Return the content type of RESPONSE, or nil if it's not set."
  (let ((headers (plz-response-headers response)))
    (when-let (header (cdr (assoc 'content-type headers)))
      (replace-regexp-in-string "\s.*" "" header))))

(defun plz-stream--response-handler (process response)
  "Return the handler for PROCESS and RESPONSE."
  (let ((content-type (plz-stream--response-content-type response)))
    (or (alist-get content-type (process-get process :plz-stream-handlers) nil nil #'equal)
        #'plz-stream-handler-default)))

(defun plz-stream--process-filter (process chunk)
  "The process filter for streaming HTTP responses.

PROCESS is the process.

CHUNK is a part of the HTTP body."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (if-let (handler (process-get process :plz-stream-handler))
            (let ((response (process-get process :plz-stream-response)))
              (setf (oref response body) chunk)
              (funcall handler process response))
          (progn
            (save-excursion
              (goto-char (process-mark process))
              (insert chunk)
              (set-marker (process-mark process) (point)))
            (goto-char (point-min))
            (when (re-search-forward plz-http-end-of-headers-regexp nil t)
              (goto-char (point-min))
              (let* ((response (plz--response))
                     (body (plz-response-body response))
                     (handler (plz-stream--response-handler process response)))
                (when body
                  (funcall handler process response))
                (setf (oref response body) nil)
                (process-put process :plz-stream-handler handler)
                (process-put process :plz-stream-response response)
                (widen)))))
        (when moving
          (goto-char (process-mark process)))))))

(cl-defun plz-stream (method
                      url
                      &rest rest &key headers body else finally noquery
                      (as 'string)
                      (body-type 'text)
                      (connect-timeout plz-connect-timeout)
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
  (let ((plz-curl-default-args (cons "--no-buffer" plz-curl-default-args))
        (process)
        (stream-handlers plz-stream-content-types))
    (pcase as
      (`(stream . ,(map (:handlers handlers)))
       (setq as 'response)
       (when handlers
         (setq stream-handlers handlers))))
    (setq process (plz method url
                    :as as
                    :body body
                    :headers headers
                    :then (cond ((symbolp then) then)
                                ((functionp then)
                                 (lambda (object)
                                   (funcall then object))))
                    :else else
                    :finally finally))
    (process-put process :plz-stream-handlers stream-handlers)
    (set-process-filter process (lambda (process chunk)
                                  (plz-stream--process-filter process chunk)))
    process))

;;;; Footer

(provide 'plz-stream)

;;; plz-stream.el ends here

(let ((api-key (auth-source-pick-first-password :host "openai.com" :user "ellama")))
  (plz-stream 'post "https://api.openai.com/v1/chat/completions"
    :as `(stream :handlers ,plz-stream-content-types)
    :body (json-encode
           '(("model" . "gpt-3.5-turbo")
             ("messages" . [(("role" . "system")
                             ("content" . "You are an assistant."))
                            (("role" . "user")
                             ("content" . "Which model are you running?"))])
             ("stream" . t)))
    :headers `(("Authorization" . ,(format "Bearer %s" api-key))
               ("Content-Type" . "application/json"))
    :else (lambda (response)
            (message "Else!"))
    :finally (lambda ()
               (message "Finally!"))
    :then (lambda (response)
            (message "Then!")
            (setq my-response response))))
