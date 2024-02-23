;;; plz-stream.el --- HTTP library -*- lexical-binding: t; -*-

;;; Commentary:

;; Streaming support for plz.el

;;; Code:

;;;; Requirements

(require 'plz)

(cl-defun plz-stream (method url &rest rest &key headers body else finally noquery
                      (as 'string) (then 'sync)
                      (body-type 'text) (decode t decode-s)
                      (connect-timeout plz-connect-timeout) (timeout plz-timeout))
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

\(To silence checkdoc, we mention the internal argument REST.)"
  ;; FIXME(v0.8): Remove the note about error changes from the docstring.
  ;; FIXME(v0.8): Update error signals in docstring.
  (declare (indent defun))
  (setf decode (if (and decode-s (not decode))
                   nil decode))
  ;; NOTE: By default, for PUT requests and POST requests >1KB, curl sends an
  ;; "Expect:" header, which causes servers to send a "100 Continue" response, which
  ;; we don't want to have to deal with, so we disable it by setting the header to
  ;; the empty string.  See <https://gms.tf/when-curl-sends-100-continue.html>.
  ;; TODO: Handle "100 Continue" responses and remove this workaround.
  (push (cons "Expect" "") headers)
  (let* ((data-arg (pcase-exhaustive body-type
                     ('binary "--data-binary")
                     ('text "--data")))
         (curl-command-line-args (append plz-curl-default-args
                                         (list "--config" "-")))
         (curl-config-header-args (cl-loop for (key . value) in headers
                                           collect (cons "--header" (format "%s: %s" key value))))
         (curl-config-args (append curl-config-header-args
                                   (list (cons "--url" url))
                                   ;; (when filter
                                   ;;   (list (cons "--no-buffer" "")))
                                   (when connect-timeout
                                     (list (cons "--connect-timeout"
                                                 (number-to-string connect-timeout))))
                                   (when timeout
                                     (list (cons "--max-time" (number-to-string timeout))))
                                   ;; NOTE: To make a HEAD request
                                   ;; requires using the "--head"
                                   ;; option rather than "--request
                                   ;; HEAD", and doing so with
                                   ;; "--dump-header" duplicates the
                                   ;; headers, so we must instead
                                   ;; specify that for each other
                                   ;; method.
                                   (pcase method
                                     ('get
                                      (list (cons "--dump-header" "-")))
                                     ((or 'put 'post)
                                      (list (cons "--dump-header" "-")
                                            (cons "--request" (upcase (symbol-name method)))
                                            ;; It appears that this must be the last argument
                                            ;; in order to pass data on the rest of STDIN.
                                            (pcase body
                                              (`(file ,filename)
                                               ;; Use `expand-file-name' because curl doesn't
                                               ;; expand, e.g. "~" into "/home/...".
                                               (cons "--upload-file" (expand-file-name filename)))
                                              (_ (cons data-arg "@-")))))
                                     ('delete
                                      (list (cons "--dump-header" "-")
                                            (cons "--request" (upcase (symbol-name method)))))
                                     ('head
                                      (list (cons "--head" "")
                                            (cons "--request" "HEAD"))))))
         (curl-config (cl-loop for (key . value) in curl-config-args
                               concat (format "%s \"%s\"\n" key value)))
         (decode (pcase as
                   ('binary nil)
                   (_ decode)))
         (default-directory
          ;; Avoid making process in a nonexistent directory (in case the current
          ;; default-directory has since been removed).  It's unclear what the best
          ;; directory is, but this seems to make sense, and it should still exist.
          temporary-file-directory)
         (process-buffer (generate-new-buffer " *plz-request-curl*"))
         (stderr-process (make-pipe-process :name "plz-request-curl-stderr"
                                            :buffer (generate-new-buffer " *plz-request-curl-stderr*")
                                            :noquery t
                                            :sentinel #'plz--stderr-sentinel))
         (process (make-process :name "plz-request-curl"
                                :buffer process-buffer
                                :coding 'binary
                                :command (append (list plz-curl-program) curl-command-line-args)
                                :connection-type 'pipe
                                :filter #'plz--process-filter
                                :sentinel #'plz--sentinel-stream
                                :stderr stderr-process
                                :noquery noquery))
         sync-p)
    (process-put process :plz-filter #'plz--process-filter)
    (when (eq 'sync then)
      (setf sync-p t
            then (lambda (result)
                   (process-put process :plz-result result))
            else nil))
    (setf
     ;; Set the callbacks, etc. as process properties.
     (process-get process :plz-then)
     (pcase-exhaustive as
       ((or 'binary 'string)
        (lambda ()
          (let ((coding-system (or (plz--coding-system) 'utf-8)))
            (pcase as
              ('binary (set-buffer-multibyte nil)))
            (plz--narrow-to-body)
            (when decode
              (decode-coding-region (point) (point-max) coding-system))
            (funcall then (or (buffer-string)
                              (make-plz-error :message (format "buffer-string is nil in buffer:%S" process-buffer)))))))
       ('buffer (progn
                  (setf (process-get process :plz-as) 'buffer)
                  (lambda ()
                    (let ((coding-system (or (plz--coding-system) 'utf-8)))
                      (pcase as
                        ('binary (set-buffer-multibyte nil)))
                      (plz--narrow-to-body)
                      (when decode
                        (decode-coding-region (point) (point-max) coding-system)))
                    (funcall then (current-buffer)))))
       ('response (lambda ()
                    (funcall then (or (plz--response :decode-p decode)
                                      (make-plz-error :message (format "response is nil for buffer:%S  buffer-string:%S"
                                                                       process-buffer (buffer-string)))))))
       ('file (lambda ()
                (set-buffer-multibyte nil)
                (plz--narrow-to-body)
                (let ((filename (make-temp-file "plz-")))
                  (condition-case err
                      (progn
                        (write-region (point-min) (point-max) filename)
                        (funcall then filename))
                    ;; In case of an error writing to the file, delete the temp file
                    ;; and signal the error.  Ignore any errors encountered while
                    ;; deleting the file, which would obscure the original error.
                    (error (ignore-errors
                             (delete-file filename))
                           (funcall then (make-plz-error :message (format "error while writing to file %S: %S" filename err))))))))
       (`(file ,(and (pred stringp) filename))
        (lambda ()
          (set-buffer-multibyte nil)
          (plz--narrow-to-body)
          (condition-case err
              (progn
                (write-region (point-min) (point-max) filename nil nil nil 'excl)
                (funcall then filename))
            ;; Since we are creating the file, it seems sensible to delete it in case of an
            ;; error while writing to it (e.g. a disk-full error).  And we ignore any errors
            ;; encountered while deleting the file, which would obscure the original error.
            (error (ignore-errors
                     (when (file-exists-p filename)
                       (delete-file filename)))
                   (funcall then (make-plz-error :message (format "error while writing to file %S: %S" filename err)))))))
       ((pred functionp) (lambda ()
                           (let ((coding-system (or (plz--coding-system) 'utf-8)))
                             (plz--narrow-to-body)
                             (when decode
                               (decode-coding-region (point) (point-max) coding-system))
                             (funcall then (funcall as))))))
     (process-get process :plz-else) else
     (process-get process :plz-finally) finally
     (process-get process :plz-sync) sync-p
     ;; Record list of arguments for debugging purposes (e.g. when
     ;; using Edebug in a process buffer, this allows determining
     ;; which request the buffer is for).
     (process-get process :plz-args) (apply #'list method url rest)
     ;; HACK: We set the result to a sentinel value so that any other
     ;; value, even nil, means that the response was processed, and
     ;; the sentinel does not need to be called again (see below).
     (process-get process :plz-result) :plz-result)
    ;; Send --config arguments.
    (process-send-string process curl-config)
    (when body
      (cl-typecase body
        (string (process-send-string process body))
        (buffer (with-current-buffer body
                  (process-send-region process (point-min) (point-max))))))
    (process-send-eof process)
    (if sync-p
        (unwind-protect
            (with-local-quit
              ;; See Info node `(elisp)Accepting Output'.
              (unless (and process stderr-process)
                (error "Process unexpectedly nil"))
              (while (accept-process-output process))
              (while (accept-process-output stderr-process))
              (when (eq :plz-result (process-get process :plz-result))
                ;; HACK: Sentinel seems to not have been called: call it again.  (Although
                ;; this is a hack, it seems to be a necessary one due to Emacs's process
                ;; handling.)  See <https://github.com/alphapapa/plz.el/issues/3> and
                ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50166>.
                (plz--sentinel-stream process "finished\n")
                (when (eq :plz-result (process-get process :plz-result))
                  (error "Plz: NO RESULT FROM PROCESS:%S  ARGS:%S"
                         process rest)))
              ;; Sentinel seems to have been called: check the result.
              (pcase (process-get process :plz-result)
                ((and (pred plz-error-p) data)
                 ;; The AS function signaled an error, which was collected
                 ;; into a `plz-error' struct: re-signal the error here,
                 ;; outside of the sentinel.
                 (if (plz-error-response data)
                     ;; FIXME(v0.8): Signal only plz-error.
                     (signal 'plz-http-error (list "HTTP error" data))
                   (signal 'plz-curl-error (list "Curl error" data))))
                (else
                 ;; The AS function returned a value: return it.
                 else)))
          (unless (eq as 'buffer)
            (kill-buffer process-buffer))
          (kill-buffer (process-buffer stderr-process)))
      ;; Async request: return the process object.
      process)))

(defun plz--sentinel-stream (process status)
  "Sentinel for curl PROCESS.
STATUS should be the process's event string (see info
node `(elisp) Sentinels').  Calls `plz--respond' to process the
HTTP response (directly for synchronous requests, or from a timer
for asynchronous ones)."
  (pcase status
    ((or "finished\n" "killed\n" "interrupt\n"
         (pred numberp)
         (rx "exited abnormally with code " (group (1+ digit))))
     (let ((buffer (process-buffer process)))
       (if (process-get process :plz-sync)
           (plz--respond process buffer status)
         (run-at-time 0 nil #'plz--respond process buffer status))))))

(defun plz--respond-stream (process buffer status)
  "Respond to HTTP response from PROCESS in BUFFER.
Parses the response and calls the THEN/ELSE callbacks
accordingly.  To be called from `plz--sentinel'.  STATUS is the
argument passed to `plz--sentinel', which see."
  ;; Is it silly to call this function "please respond"?  Perhaps, but
  ;; naming things is hard.  The term "process" has another meaning in
  ;; this context, and the old standby, "handle," is much overused.
  ;; "Respond" also means "to react to something," which is what this
  ;; does--react to receiving the HTTP response--and it's an internal
  ;; name, so why not.
  (unwind-protect
      (with-current-buffer buffer
        (pcase-exhaustive status
          ((or 0 "finished\n")
           ;; Curl exited normally: check HTTP status code.
           (goto-char (point-min))
           (plz--skip-proxy-headers)
           (while (plz--skip-redirect-headers))
           (pcase (plz--http-status)
             ((and status (guard (<= 200 status 299)))
              ;; Any 2xx response is considered successful.
              (ignore status) ; Byte-compiling in Emacs <28 complains without this.
              ;; TODO: Move this funcall?
              (message "PLZ--RESPOND: %s" status)
              (unless (process-get process :plz-filter)
                (funcall (process-get process :plz-then))))
             (_
              ;; TODO: If using ":as 'response", the HTTP response
              ;; should be passed to the THEN function, regardless
              ;; of the status code.  Only for curl errors should
              ;; the ELSE function be called.  (Maybe in v0.8.)

              ;; Any other status code is considered unsuccessful
              ;; (for now, anyway).
              (let ((err (make-plz-error :response (plz--response))))
                (pcase-exhaustive (process-get process :plz-else)
                  (`nil (process-put process :plz-result err))
                  ((and (pred functionp) fn) (funcall fn err)))))))

          ((or (and (pred numberp) code)
               (rx "exited abnormally with code " (let code (group (1+ digit)))))
           ;; Curl error.
           (let* ((curl-exit-code (cl-typecase code
                                    (string (string-to-number code))
                                    (number code)))
                  (curl-error-message (alist-get curl-exit-code plz-curl-errors))
                  (err (make-plz-error :curl-error (cons curl-exit-code curl-error-message))))
             (pcase-exhaustive (process-get process :plz-else)
               (`nil (process-put process :plz-result err))
               ((and (pred functionp) fn) (funcall fn err)))))

          ((and (or "killed\n" "interrupt\n") status)
           ;; Curl process killed or interrupted.
           (let* ((message (pcase status
                             ("killed\n" "curl process killed")
                             ("interrupt\n" "curl process interrupted")))
                  (err (make-plz-error :message message)))
             (pcase-exhaustive (process-get process :plz-else)
               (`nil (process-put process :plz-result err))
               ((and (pred functionp) fn) (funcall fn err)))))))
    (when-let ((finally (process-get process :plz-finally)))
      (funcall finally))
    (unless (or (process-get process :plz-sync)
                (eq 'buffer (process-get process :plz-as)))
      ;; TODO: RESTORE
      (let ((response-buffer (get-buffer-create "CURL")))
        (with-current-buffer response-buffer
          ;; (switch-to-buffer-other-window response-buffer)
          (erase-buffer)
          (insert (with-current-buffer buffer
                    (widen)
                    (buffer-string)))))
      (kill-buffer buffer))))

(defun plz--parse-content-type-header (header)
  "Parse CONTENT-TYPE HEADER and return its components as an alist."
  (when (and header (not (string-blank-p header)))
    (let ((components (split-string header ";")))
      (let ((type (car components))
            (params (cdr components)))
        (list type
              (cl-loop for param in params
                       when (string-match "\\`[[:space:]]*\\([^=]+\\)=\\(.+\\)[[:space:]]*\\'" param)
                       collect (cons (intern (downcase (match-string 1 param)))
                                     (match-string 2 param))))))))

(defun plz--default-process-filter (proc string)
  "Insert STRING into the process buffer of PROC."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (if moving (goto-char (process-mark proc)))))))

(defun plz--process-filter (proc string)
  ;; (message "Filter called: %s" string)
  (plz--default-process-filter proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc))))
        ;; Find and set the HTTP status code of the response.
        (unless (process-get proc :plz-status)
          (save-excursion
            (goto-char (point-min))
            (plz--skip-proxy-headers)
            (while (plz--skip-redirect-headers))
            (when-let (status (plz--http-status))
              (message "STATUS: %s" status)
              (process-put proc :plz-status status))))
        ;; Find and set the HTTP headers of the response.
        (unless (process-get proc :plz-headers)
          (save-excursion
            (goto-char (point-min))
            (plz--skip-proxy-headers)
            (while (plz--skip-redirect-headers))
            (when-let (headers (plz--headers))
              (message "HEADERS: %s" headers)
              (setq my-headers headers)
              (process-put proc :plz-headers headers))))
        (when-let (headers (process-get proc :plz-headers))
          (when-let (content-type (alist-get 'content-type headers))
            (message "CONTENT-TYPE: %s" content-type)))))))

;;;; Footer

(provide 'plz-stream)

;;; plz-stream.el ends here
