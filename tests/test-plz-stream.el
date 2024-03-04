;;; test-event-source.el --- Event Source Test Module -*- lexical-binding: t; -*-

;; Author: r0man <r0man@burningswell.com>
;; Version: 0.1
;; Package-Requires: ((emacs "26.1"))
;; Keywords: event, source

;;; Commentary:

;; Event Source Test Module

;;; Code:

(require 'ert)
(require 'plz-stream)

(ert-deftest plz-stream-completions ()
  (when-let (api-key (auth-source-pick-first-password :host "openai.com" :user "ellama"))
    (let* ((open-events) (message-events) (error-events) (close-events)
           (process (plz-stream 'post "https://api.openai.com/v1/chat/completions"
                      :as `(stream :handlers (("text/event-stream"
                                               . ,(event-source-text-event-stream
                                                   `(("open" . ,(lambda (source event)
                                                                  (push event open-events)))
                                                     ("message" . ,(lambda (source event)
                                                                     (push event message-events)))
                                                     ("error" . ,(lambda (source event)
                                                                   (push event error-events)))
                                                     ("close" . ,(lambda (source event)
                                                                   (push event close-events))))))))
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
                      :else (lambda (response)
                              (message "Else!"))
                      :finally (lambda ()
                                 (message "Finally!"))
                      :then (lambda (response)
                              (message "Then!")
                              (setq my-response response)))))
      (plz-test-wait process)
      (should (equal (list (event-source-event :type "open")) open-events))
      (should (null error-events))
      ;; (should (equal (list (event-source-event :type "close")) close-events))
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

(ert-deftest test-plz-stream-application/octet-stream ()
  (when-let (api-key (auth-source-pick-first-password :host "openai.com" :user "ellama"))
    (let* ((open-events) (message-events) (error-events) (close-events)
           (process (plz-stream 'post "https://api.openai.com/v1/chat/completions"
                      :as `(stream :handlers ((t . (plz-stream:application/octet-stream))))
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
                      :else (lambda (response)
                              (message "Else!"))
                      :finally (lambda ()
                                 (message "Finally!"))
                      :then (lambda (response)
                              (message "Then!")
                              (setq my-response response)))))
      (plz-test-wait process)
      (should (equal (list (event-source-event :type "open")) open-events))
      (should (null error-events))
      ;; (should (equal (list (event-source-event :type "close")) close-events))
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

;;;; Footer

(provide 'test-plz-stream)

;;; test-plz-stream.el ends here
