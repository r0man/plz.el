;;; event-source.el --- Server Sent Event Source -*- lexical-binding: t; -*-

;; Author: r0man <r0man@burningswell.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: event, source

;;; Commentary:

;; This library provides a parser and an event source implementation
;; for the Server Sent Event (SSE) protocol.

;; See: https://html.spec.whatwg.org/multipage/server-sent-events.html#server-sent-events

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'pcase)
(require 'plz)
(require 'rx)

;; Event

(defclass event-source-event ()
  ((data
    :accessor event-source-event-data
    :initarg :data
    :initform ""
    :documentation "The event data."
    :type string)
   (last-event-id
    :accessor event-source-event-last-event-id
    :initarg :last-event-id
    :initform nil
    :documentation "The last event id."
    :type (or null string))
   (origin
    :accessor event-source-event-origin
    :initarg :origin
    :initform nil
    :documentation "The event origin."
    :type (or null string))
   (type
    :accessor event-source-event-type
    :initarg :type
    :initform "message"
    :documentation "The event type."
    :type string))
  "The server sent event class.")

;; Parser

(defclass event-source-parser ()
  ((buffer
    :documentation "The name of the buffer to read events from."
    :initarg :buffer
    :type string)
   (events
    :initarg :events
    :initform nil
    :documentation "The queue of events to dispatch."
    :type (list-of event-source-event))
   (data-buffer
    :initarg :data-buffer
    :initform ""
    :documentation "Data buffer."
    :type string)
   (event-type-buffer
    :initarg :event-type-buffer
    :initform ""
    :documentation "Event type buffer."
    :type string)
   (last-event-id
    :initarg :last-event-id
    :initform ""
    :documentation "Last event id."
    :type string)
   (last-event-id-buffer
    :initarg :last-event-id-buffer
    :initform ""
    :documentation "Last event id buffer."
    :type string)
   (position
    :initarg :position
    :initform 0
    :type integer
    :documentation "The position in the buffer."
    :type integer))
  "The server sent event stream parser.")

(defconst event-source--end-of-line-regexp
  (rx (or "\r\n" "\n" "\r"))
  "Regular expression matching the end of a line.")

(defconst event-source--line-regexp
  (rx (* not-newline) (or "\r\n" "\n" "\r"))
  "Regular expression matching a line of the event source stream.")

(defun event-source--parse-bom (line)
  "Parse the Byte Order Mark (BOM) from LINE."
  (if (string-prefix-p "\uFEFF" line)
      (substring line 1)
    line))

(defun event-source--looking-at-line-p ()
  "Return non-nil if the current line matches the event source line regexp."
  (looking-at event-source--line-regexp))

(defun event-source--parse-line ()
  "Return non-nil if the current line matches the event source line regexp."
  (when (looking-at event-source--line-regexp)
    (string-trim-right (delete-and-extract-region (match-beginning 0) (match-end 0))
                       event-source--end-of-line-regexp)))

(defun event-source--dispatch-event (parser)
  "Dispatch an event from PARSER to registered listeners."
  (with-slots (data-buffer event-type-buffer events last-event-id last-event-id-buffer) parser
    (setf last-event-id last-event-id-buffer)
    (if (string-empty-p data-buffer)
        (setf data-buffer ""
              event-type-buffer "")
      (progn
        (setf data-buffer (string-trim-right data-buffer "\n"))
        (let ((event (event-source-event
                      :data data-buffer
                      :last-event-id (unless (string-blank-p last-event-id)
                                       last-event-id)
                      :origin (buffer-name)
                      :type (if (string-blank-p event-type-buffer)
                                "message"
                              event-type-buffer))))
          (setf data-buffer ""
                event-type-buffer "")
          (setf events (cons event events))
          event)))))

(defun event-source--process-event (parser field value)
  "Process the FIELD and VALUE from PARSER as a event."
  (ignore field)
  (with-slots (event-type-buffer) parser
    (setf event-type-buffer value)))

(defun event-source--process-data (parser field value)
  "Process the FIELD and VALUE from PARSER as data."
  (ignore field)
  (with-slots (data-buffer) parser
    (setf data-buffer (concat data-buffer value "\n"))))

(defun event-source--process-id (parser field value)
  "Process the FIELD and VALUE from PARSER as event id."
  (ignore field)
  (unless (string-match "\u0000" value)
    (with-slots (last-event-id-buffer) parser
      (setf last-event-id-buffer value))))

(defun event-source--process-retry (parser field value)
  "Process the FIELD and VALUE from PARSER as event id."
  (ignore parser)
  (message "TODO: Process retry for field %s and value %s." field value))

(defun event-source--process-field (parser field value)
  "Process the FIELD and VALUE from PARSER."
  (cond ((equal "event" field)
         (event-source--process-event parser field value))
        ((equal "data" field)
         (event-source--process-data parser field value))
        ((equal "id" field)
         (event-source--process-id parser field value))
        ((equal "retry" field)
         (event-source--process-retry parser field value))))

(defun event-source--process-line (parser line)
  "Parse a LINE of the event stream PARSER and dispatch events."
  (cond ((string-prefix-p ":" line))
        ((string-blank-p line)
         (event-source--dispatch-event parser))
        ((string-match ":" line)
         (let ((field (substring line 0 (match-beginning 0)))
               (value (substring line (match-end 0))))
           (event-source--process-field parser field
                                        (if (string-prefix-p " " value)
                                            (substring value 1)
                                          value))))
        (t (event-source--process-field parser line ""))))

(defun event-source-parse-line (parser)
  "Parse a line from the event stream in the PARSER buffer."
  (with-slots (buffer position) parser
    (with-current-buffer buffer
      (save-excursion
        (goto-char position)
        (when-let (line (event-source--parse-line))
          (setf position (point))
          (event-source--process-line parser line)
          line)))))

(defun event-source-parse-stream (parser)
  "Parse the event stream in the the PARSER buffer."
  (with-slots (buffer handlers) parser
    (with-current-buffer (get-buffer buffer)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (event-source--parse-line)))
          (event-source--process-line parser line))))))

(defun event-source-parser-insert (parser string)
  "Insert STRING into the buffer of the event PARSER."
  (with-slots (buffer position) parser
    (with-current-buffer (get-buffer buffer)
      (insert string)
      (event-source-parse-stream parser))))

;; Event Source

(defclass event-source ()
  ((errors
    :initarg :errors
    :documentation "The errors of the event source.")
   (handlers
    :initarg :handlers
    :initform nil
    :documentation "Registered event handlers.")
   (last-event-id
    :initarg :last-event-id
    :initform ""
    :documentation "Last event id.")
   (options
    :initarg :options
    :documentation "The url of the event source."
    :type list)
   (ready-state
    :documentation "The ready state of the event source."
    :initarg :ready-state
    :initform 'closed
    :type (member closed connecting open))
   (url
    :initarg :url
    :documentation "The url of the event source."
    :type (or null string)))
  "The server sent event source class.")

(defun event-source-add-listener (source type listener)
  "Add an event LISTENER for event TYPE to the event SOURCE."
  (with-slots (handlers) source
    (setf handlers (append handlers (list (cons type listener))))
    source))

(defun event-source-remove-listener (source type listener)
  "Remove an event LISTENER for event TYPE from the event SOURCE."
  (with-slots (handlers) source
    (setf handlers (cl-remove-if (lambda (pair)
                                   (and (eq (car pair) type)
                                        (eq (cdr pair) listener)))
                                 handlers))
    source))

(defun event-source-dispatch-event (source event)
  "Dispatch the EVENT to the listeners of event SOURCE."
  (with-slots (handlers) source
    (dolist (pair handlers)
      (when (equal (car pair) (oref event type))
        (condition-case error
            (funcall (cdr pair) source event)
          (error
           (message "Error in event handler for event type %s:\n  %s\n"
                    (oref event type) error)))))))

(defun event-source-dispatch-events (source events)
  "Dispatch the EVENTS to the listeners of event SOURCE."
  (dolist (event (reverse events))
    (event-source-dispatch-event source event)))

(defun event-source--response-in-buffer-p ()
  "Return non-nil the if point is looking at a HTTP response."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward plz-http-end-of-headers-regexp nil t)))

(defun event-source-parser--end-of-headers ()
  "Return the end of headers position in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward plz-http-end-of-headers-regexp nil t)
    (point)))

(defclass plz-event-source (event-source)
  ((process
    :initarg :process
    :documentation "The process of the event source."
    :type (or null process))
   (response
    :initarg :response
    :documentation "The plz HTTP response."
    :type (or null plz-response)))
  "A server sent event source using curl for HTTP.")

(defun event-source--plz-process-filter (source process chunk)
  "The event source process filter.

SOURCE is the event source.

PROCESS is the process.

CHUNK is a part of the HTTP body."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          (goto-char (process-mark process))
          (insert chunk)
          (set-marker (process-mark process) (point)))
        (cond
         ((process-get process :event-source-parser)
          (let ((parser (process-get process :event-source-parser)))
            (with-slots (events) parser
              (while (event-source-parse-line parser))
              (event-source-dispatch-events source events)
              (setf events nil))))
         ((event-source--response-in-buffer-p)
          (goto-char (point-min))
          (let ((response (plz--response))
                (parser (event-source-parser
                         :buffer (buffer-name (current-buffer))
                         :position (event-source-parser--end-of-headers))))
            (setf (oref response body) nil)
            (process-put process :event-source-parser parser)
            (with-slots (ready-state response) source
              (setf ready-state 'open)
              (setf response response))
            (widen)
            (event-source-dispatch-event source (event-source-event :type "open")))))
        (when moving
          (goto-char (process-mark process)))))))

(cl-defmethod event-source-open ((source plz-event-source))
  "Open a connection to the URL of the event SOURCE."
  (with-slots (errors options process ready-state response url) source
    (setf ready-state 'connecting)
    (setf response nil)
    (let ((plz-curl-default-args (cons "--no-buffer" plz-curl-default-args)))
      (setf process (plz (or (alist-get 'method options) 'get) url
                      :as 'response
                      :body (alist-get 'body options)
                      :headers (alist-get 'headers options)
                      :then (lambda (object)
                              (setf response object))
                      :else (lambda (object)
                              (setf errors (push object errors))
                              (setf response (plz-error-response object))
                              (event-source-dispatch-event source (event-source-event :type "error")))
                      :finally (lambda ()
                                 (setf ready-state 'closed)
                                 (event-source-dispatch-event source (event-source-event :type "close")))))
      (set-process-filter process (lambda (process chunk)
                                    (event-source--plz-process-filter source process chunk)))
      source)))

(cl-defmethod event-source-close ((source plz-event-source))
  "Close the connection of the event SOURCE."
  (with-slots (process ready-state) source
    (delete-process process)
    (setf ready-state 'closed)))

(provide 'event-source)
;;; event-source.el ends here
