;;; test-plz-media-type.el --- Event Source Test Module -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Free Software Foundation, Inc.

;; Author: r0man <roman@burningswell.com>
;; Maintainer: r0man <roman@burningswell.com>

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

;; Event Source Test Module

;;; Code:

(require 'ert)
(require 'plz-event-source)
(require 'plz-media-type)
(require 'plz-test)

(plz-deftest test-plz-media-type-json-async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(plz-deftest test-plz-media-type-json-sync ()
  (let ((response (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types))))
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(plz-deftest test-plz-media-type-html-async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-html-sync ()
  (let ((response (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types))))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-xml-async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(plz-deftest test-plz-media-type-xml-sync ()
  (let ((response (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types))))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(ert-deftest test-plz-media-type-event-stream ()
  (when-let (api-key plz-test-openai-token)
    (let* ((close-events) (else) (error-events) (finally) (message-events) (open-events) (then)
           (process (plz-media-type-request 'post "https://api.openai.com/v1/chat/completions"
                      :as `(media-types
                            ,(cons (cons "text/event-stream"
                                         (plz-media-type:text/event-stream
                                          :events `(("open" . ,(lambda (_ event)
                                                                 (push event open-events)))
                                                    ("message" . ,(lambda (_ event)
                                                                    (push event message-events)))
                                                    ("error" . ,(lambda (_ event)
                                                                  (push event error-events)))
                                                    ("close" . ,(lambda (_ event)
                                                                  (push event close-events))))))
                                   plz-media-types))
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
      (should (equal 1 (length open-events)))
      (seq-doseq (event open-events)
        (with-slots (data type) event
          (should (equal "open" type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
      (should (equal 0 (length error-events)))
      (should (equal 1 (length close-events)))
      (seq-doseq (event close-events)
        (with-slots (data type) event
          (should (equal "close" type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
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

(ert-deftest test-plz-media-type-chat-completions-as-application/octet-stream ()
  (when-let (api-key plz-test-openai-token)
    (let* ((else) (finally) (then)
           (process (plz-media-type-request 'post "https://api.openai.com/v1/chat/completions"
                      :as `(media-types `((t . ,(plz-media-type:application/octet-stream))))
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

(provide 'test-plz-media-type)

;;; test-plz-media-type.el ends here