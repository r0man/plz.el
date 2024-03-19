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

(plz-deftest test-plz-media-type:application/json:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(plz-deftest test-plz-media-type:application/json:sync ()
  (let ((response (plz-media-type-request 'get (url "/json")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (let-alist (plz-response-body response)
      (should (equal "Sample Slide Show" .slideshow.title)))))

(ert-deftest test-plz-media-type:application/json:sync-error ()
  (plz-test-with-mock-response (plz-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((result (condition-case error
                       (plz-media-type-request 'get "MOCK-URL"
                         :as `(media-types ((application/json . ,(plz-media-type:application/json)))))
                     (plz-error error))))
      (should (equal 'plz-http-error (car result)))
      (should (equal "HTTP error" (cadr result)))
      (let ((error (caddr result)))
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (equal '(code . 401) (cadar (elt (plz-response-body response) 0)))))))))

(plz-deftest test-plz-media-type:text/html:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type:text/html:sync ()
  (let ((response (plz-media-type-request 'get (url "/html")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'html (car (plz-response-body response))))))

(plz-deftest test-plz-media-type:application/xml:async ()
  (let* ((response)
         (process (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types)
                    :then (lambda (object)
                            (setf response object)))))
    (plz-test-wait process)
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(plz-deftest test-plz-media-type:application/xml:sync ()
  (let ((response (plz-media-type-request 'get (url "/xml")
                    :as `(media-types ,plz-media-types))))
    (should (plz-response-p response))
    (should (equal 200 (plz-response-status response)))
    (should (equal 'top (car (plz-response-body response))))))

(ert-deftest test-plz-media-type-parse ()
  (should (null (plz-media-type-parse nil)))
  (should (null (plz-media-type-parse "")))
  (should (equal (plz-media-type :type 'text :subtype 'html)
                 (plz-media-type-parse "text/html")))
  (should (equal (plz-media-type
                  :type 'text
                  :subtype 'html
                  :parameters '(("charset" . "UTF-8")))
                 (plz-media-type-parse "text/html;charset=UTF-8")))
  (should (equal (plz-media-type
                  :type 'text
                  :subtype 'html
                  :parameters '(("charset" . "UTF-8")
                                ("boundary" . "AaB03x\"")))
                 (plz-media-type-parse "text/html; charset=UTF-8; boundary=\"AaB03x\""))))

(ert-deftest test-plz-media-type-charset ()
  (let ((media-type (plz-media-type-parse "text/html; charset=UTF-8")))
    (should (equal "UTF-8" (plz-media-type-charset media-type)))))

(ert-deftest test-plz-media-type-coding-system ()
  (let ((media-type (plz-media-type-parse "text/html")))
    (should (equal 'utf-8 (plz-media-type-coding-system media-type))))
  (let ((media-type (plz-media-type-parse "text/html; charset=UTF-8")))
    (should (equal 'utf-8 (plz-media-type-coding-system media-type)))))

(ert-deftest test-plz-media-type-symbol ()
  (let ((media-type (plz-media-type-parse "text/html")))
    (should (equal 'text/html (plz-media-type-symbol media-type)))))

(ert-deftest test-plz-media-type:text/event-stream ()
  (plz-test-with-mock-response (plz-test-response "text/event-stream/openai-hello.txt")
    (let* ((close-events) (else) (error-events) (finally) (message-events) (open-events) (then)
           (process (plz-media-type-request 'post "MOCK-URL"
                      :as `(media-types
                            ,(cons (cons 'text/event-stream
                                         (plz-media-type:text/event-stream
                                          :events `((open . ,(lambda (_ event)
                                                               (push event open-events)))
                                                    (message . ,(lambda (_ event)
                                                                  (push event message-events)))
                                                    (error . ,(lambda (_ event)
                                                                (push event error-events)))
                                                    (close . ,(lambda (_ event)
                                                                (push event close-events))))))
                                   plz-media-types))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length open-events)))
      (seq-doseq (event open-events)
        (with-slots (data type) event
          (should (equal 'open type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
      (should (equal 0 (length error-events)))
      (should (equal 1 (length close-events)))
      (seq-doseq (event close-events)
        (with-slots (data type) event
          (should (equal 'close type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
      (should (equal 12 (length message-events)))
      (should (equal "Hello! How can I assist you today?"
                     (plz-test-openai-extract-content message-events))))))

(ert-deftest test-plz-media-type:text/event-stream:emoji ()
  (plz-test-with-mock-response (plz-test-response "text/event-stream/openai-emoji.txt")
    (let* ((close-events) (else) (error-events) (finally) (message-events) (open-events) (then)
           (process (plz-media-type-request 'post "MOCK-URL"
                      :as `(media-types
                            ,(cons (cons 'text/event-stream
                                         (plz-media-type:text/event-stream
                                          :events `((open . ,(lambda (_ event)
                                                               (push event open-events)))
                                                    (message . ,(lambda (_ event)
                                                                  (push event message-events)))
                                                    (error . ,(lambda (_ event)
                                                                (push event error-events)))
                                                    (close . ,(lambda (_ event)
                                                                (push event close-events))))))
                                   plz-media-types))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-test-wait process)
      (should (null else))
      (should (equal '(t) finally))
      (should (equal 1 (length open-events)))
      (seq-doseq (event open-events)
        (with-slots (data type) event
          (should (equal 'open type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
      (should (equal 0 (length error-events)))
      (should (equal 1 (length close-events)))
      (seq-doseq (event close-events)
        (with-slots (data type) event
          (should (equal 'close type))
          (should (plz-response-p data))
          (should (equal 200 (plz-response-status data)))
          (should (null (plz-response-body data)))))
      (should (equal 4 (length message-events)))
      (should (equal "🙂" (plz-test-openai-extract-content message-events))))))

(ert-deftest test-plz-media-type:application/octet-stream:stream ()
  (plz-test-with-mock-response (plz-test-response "text/event-stream/openai-hello.txt")
    (let* ((else) (finally) (then)
           (process (plz-media-type-request 'post "https://api.openai.com/v1/chat/completions"
                      :as `(media-types `((t . ,(plz-media-type:application/octet-stream))))
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

(ert-deftest test-plz-media-type:application/x-ndjson ()
  (plz-test-with-mock-response (plz-test-response "application/x-ndjson/ollama-hello.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/x-ndjson
                                          . ,(plz-media-type:application/x-ndjson
                                              :handler (lambda (object)
                                                         (push object objects))))))
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
        (should (string-match "" (plz-response-body response))))
      (should (equal 27 (length objects)))
      (should (equal '((model . "llama2")
                       (created_at . "2024-03-12T12:05:13.747334659Z")
                       (response . "Hello")
                       (done . :json-false))
                     (seq-elt objects 26)))
      (should (equal '((model . "llama2")
                       (created_at . "2024-03-12T12:05:15.467785437Z")
                       (response . "?")
                       (done . :json-false))
                     (seq-elt objects 1))))))

(ert-deftest test-plz-media-type:application/json-array:async ()
  (plz-test-with-mock-response (plz-test-response "application/json/vertex-hello.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/json
                                          . ,(plz-media-type:application/json-array
                                              :handler (lambda (object)
                                                         (push object objects))))))
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
        (should (string-match "" (plz-response-body response))))
      (should (equal 2 (length objects)))
      (should (equal '("Hi there!" " How can I assist you today?")
                     (plz-test-vertex-extract-content objects))))))

(ert-deftest test-plz-media-type:application/json-array:sync ()
  (plz-test-with-mock-response (plz-test-response "application/json/vertex-hello.txt")
    (let* ((objects)
           (response (plz-media-type-request 'get "MOCK-URL"
                       :as `(media-types ((application/json
                                           . ,(plz-media-type:application/json-array
                                               :handler (lambda (object)
                                                          (push object objects)))))))))
      (should (plz-response-p response))
      (should (equal 200 (plz-response-status response)))
      (should (string-match "" (plz-response-body response))))))

(ert-deftest test-plz-media-type:application/json-array:async-error ()
  (plz-test-with-mock-response (plz-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((else) (finally) (then) (objects)
           (process (plz-media-type-request 'get "MOCK-URL"
                      :as `(media-types ((application/json
                                          . ,(plz-media-type:application/json-array
                                              :handler (lambda (object)
                                                         (push object objects))))))
                      :else (lambda (object) (push object else))
                      :finally (lambda () (push t finally))
                      :then (lambda (object) (push object then)))))
      (plz-test-wait process)
      (should (equal '(t) finally))
      (should (null then))
      (should (equal 1 (length else)))
      (seq-doseq (error else)
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (string-match "" (plz-response-body response)))))
      (should (equal 1 (length objects)))
      (should (equal '(code . 401) (cadaar objects))))))

(ert-deftest test-plz-media-type:application/json-array:sync-error ()
  (plz-test-with-mock-response (plz-test-response "application/json/vertext-unauthenticated.txt")
    (let* ((objects)
           (result (condition-case error
                       (plz-media-type-request 'get "MOCK-URL"
                         :as `(media-types ((application/json
                                             . ,(plz-media-type:application/json-array
                                                 :handler (lambda (object)
                                                            (push object objects)))))))
                     (plz-error error))))
      (should (equal 'plz-http-error (car result)))
      (should (equal "HTTP error" (cadr result)))
      (let ((error (caddr result)))
        (should (plz-error-p error))
        (let ((response (plz-error-response error)))
          (should (plz-response-p response))
          (should (equal 401 (plz-response-status response)))
          (should (equal "" (plz-response-body response)))))
      (should (equal 1 (length objects)))
      (should (equal '(code . 401) (cadaar objects))))))

;;;; Footer

(provide 'test-plz-media-type)

;;; test-plz-media-type.el ends here
