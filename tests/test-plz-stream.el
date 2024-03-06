;;; test-event-source.el --- Event Source Test Module -*- lexical-binding: t; -*-

;; Author: r0man <r0man@burningswell.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: event, source

;;; Commentary:

;; Event Source Test Module

;;; Code:

(require 'ert)
(require 'plz-event-source)
(require 'plz-stream)
(require 'test-plz)

(defvar test-plz-stream-token
  (auth-source-pick-first-password :host "openai.com" :user "ellama"))

(ert-deftest test-plz-stream-chat-completions-as-text/event-stream ()
  (when-let (api-key test-plz-stream-token)
    (let* ((close-events) (else) (error-events) (finally) (message-events) (open-events) (then)
           (process (plz-stream 'post "https://api.openai.com/v1/chat/completions"
                      :as `(stream :handlers (("text/event-stream"
                                               . ,(plz-stream:text/event-stream
                                                   :on `(("open" . ,(lambda (_ event)
                                                                      (push event open-events)))
                                                         ("message" . ,(lambda (_ event)
                                                                         (push event message-events)))
                                                         ("error" . ,(lambda (_ event)
                                                                       (push event error-events)))
                                                         ("close" . ,(lambda (_ event)
                                                                       (push event close-events))))))
                                              (t . ,(plz-stream:application/octet-stream))))
                      :body (json-encode
                             '(("model" . "gpt-3.5-turbo")
                               ("messages" . [(("role" . "system")
                                               ("content" . "You are an assistant."))
                                              (("role" . "user")
                                               ("content" . "Hello"))])
                               ("stream" . t)
                               ("temperature" . 0.001)))
                      :headers `(("Authorization" . ,(format "Bearer %s" api-key))
                                 ("Content-Type" . "application/json"))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal (list (plz-event-source-event :type "open")) open-events))
      (should (null error-events))
      (should (equal (list (plz-event-source-event :type "close")) close-events))
      (should (equal "Hello! How can I assist you today?"
                     (thread-last
                       (reverse message-events)
                       (seq-filter (lambda (event)
                                     (with-slots (data type) event
                                       (not (equal "[DONE]" data)))))
                       (seq-map (lambda (event)
                                  (with-slots (data) event
                                    (when-let ((data (json-parse-string data))
                                               (choice (seq-first (map-elt data "choices")))
                                               (delta (map-elt choice "delta"))
                                               (content (map-elt delta "content")))
                                      content))))
                       (string-join)))))))

(ert-deftest test-plz-stream-chat-completions-as-application/octet-stream ()
  (when-let (api-key test-plz-stream-token)
    (let* ((else) (finally) (then)
           (process (plz-stream 'post "https://api.openai.com/v1/chat/completions"
                      :as `(stream :handlers ((t . ,(plz-stream:application/octet-stream))))
                      :body (json-encode
                             '(("model" . "gpt-3.5-turbo")
                               ("messages" . [(("role" . "system")
                                               ("content" . "You are an assistant."))
                                              (("role" . "user")
                                               ("content" . "Hello"))])
                               ("stream" . t)
                               ("temperature" . 0.001)))
                      :headers `(("Authorization" . ,(format "Bearer %s" api-key))
                                 ("Content-Type" . "application/json"))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length then)))
      (seq-doseq (response then)
        (should (plz-response-p response))
        (should (equal 200 (plz-response-status response)))
        (should (string-match "[DONE]" (plz-response-body response)))))))

;;;; Footer

(provide 'test-plz-stream)

;;; test-plz-stream.el ends here
