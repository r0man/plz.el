;;; test-event-source.el --- Event Source Test Module -*- lexical-binding: t; -*-

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

;; Event Source Test Module

;;; Code:

(require 'ert)
(require 'plz-event-source)
(require 'plz-test)

(ert-deftest test-plz-event-source-parse-mutli-line-event ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :data "This is the first message."
                    :origin (buffer-name)))
          (event-2 (plz-event-source-event
                    :data "This is the second message, it\nhas two lines."
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser "data: This is the first message.\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data: This is the second message, it\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data: has two lines.\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "data: This is the third message.\n")
        (should (equal (list event-2 event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-parse-event-types ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :type "add"
                    :data "73857293"
                    :origin (buffer-name)))
          (event-2 (plz-event-source-event
                    :type "remove"
                    :data "2153"
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser "event: add\n")
        (should (null events))
        (plz-event-source-parser-insert parser "data: 73857293\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "event: remove\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data: 2153\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "event: add\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "data: 113411\n")
        (should (equal (list event-2 event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-parse-stock-ticker ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :data "YHOO\n+2\n10"
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser "data: YHOO\n")
        (should (null events))
        (plz-event-source-parser-insert parser "data: +2\n")
        (should (null events))
        (plz-event-source-parser-insert parser "data: 10\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-parse-four-blocks ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :data "first event"
                    :last-event-id "1"
                    :origin (buffer-name)))
          (event-2 (plz-event-source-event
                    :data "second event"
                    :origin (buffer-name)))
          (event-3 (plz-event-source-event
                    :data " third event"
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser ": test stream\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (null events))
        (plz-event-source-parser-insert parser "data: first event\n")
        (should (null events))
        (plz-event-source-parser-insert parser "id: 1\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data:second event\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "id\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "data:  third event\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-3 event-2 event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-parse-two-events ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :data ""
                    :origin (buffer-name)))
          (event-2 (plz-event-source-event
                    :data "\n"
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser "data\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-2 event-1) events))
        (plz-event-source-parser-insert parser "data:\n")
        (should (equal (list event-2 event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-parse-two-identical-events ()
  (with-temp-buffer
    (let ((event-1 (plz-event-source-event
                    :data "test"
                    :origin (buffer-name)))
          (parser (plz-event-source-parser :buffer (buffer-name))))
      (with-slots (events) parser
        (plz-event-source-parser-insert parser "data:test\n")
        (should (null events))
        (plz-event-source-parser-insert parser "\n")
        (should (equal (list event-1) events))
        (plz-event-source-parser-insert parser "data: test\n")
        (should (equal (list event-1) events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-event-source-buffer-event-source ()
  (with-temp-buffer
    (let* ((event-1 (plz-event-source-event
                     :data "This is the first message."
                     :origin (buffer-name)))
           (event-2 (plz-event-source-event
                     :data "This is the second message, it\nhas two lines."
                     :origin (buffer-name)))
           (open-events) (message-events) (close-events)
           (source (plz-buffer-event-source
                    :buffer (buffer-name)
                    :handlers `(("open" . ,(lambda (source event)
                                             (push event open-events)
                                             (should (cl-typep source 'plz-event-source))
                                             (should (cl-typep event 'plz-event-source-event))
                                             (should (equal "open" (plz-event-source-event-type event)))))
                                ("message" . ,(lambda (source event)
                                                (push event message-events)
                                                (should (cl-typep source 'plz-event-source))
                                                (should (cl-typep event 'plz-event-source-event))
                                                (should (equal "message" (plz-event-source-event-type event)))))
                                ("close" . ,(lambda (source event)
                                              (push event close-events)
                                              (should (cl-typep source 'plz-event-source))
                                              (should (cl-typep event 'plz-event-source-event))
                                              (should (equal "close" (plz-event-source-event-type event)))))))))
      (with-slots (parser) source
        (plz-event-source-open source)
        (should (equal (list (plz-event-source-event :type "open")) open-events))
        (plz-event-source-insert source "data: This is the first message.\n")
        (should (null message-events))
        (plz-event-source-insert source "\n")
        (should (equal (list event-1) message-events))
        (plz-event-source-insert source "data: This is the second message, it\n")
        (plz-event-source-insert source "data: has two lines.\n")
        (plz-event-source-insert source "\n")
        (plz-event-source-insert source "data: This is the third message.\n")
        (should (equal (list event-2 event-1) message-events))
        (plz-event-source-close source)
        (should (equal (list (plz-event-source-event :type "close")) close-events))
        (should (equal "" (buffer-string)))))))

(ert-deftest test-plz-http-event-source-completions ()
  (when-let (api-key (auth-source-pick-first-password :host "openai.com" :user "ellama"))
    (let ((events)
          (source (plz-http-event-source
                   :url "https://api.openai.com/v1/chat/completions"
                   :options `((body . ,(json-encode
                                        '(("model" . "gpt-3.5-turbo")
                                          ("messages" . [(("role" . "system")
                                                          ("content" . "You are an assistant."))
                                                         (("role" . "user")
                                                          ("content" . "Hello"))])
                                          ("stream" . t)
                                          ("temperature" . 0.001))))
                              (headers . (("Authorization" . ,(format "Bearer %s" api-key))
                                          ("Content-Type" . "application/json")))
                              (method . post)))))
      (plz-event-source-add-listener
       source "open" (lambda (source event)
                       (should (cl-typep source 'plz-event-source))
                       (should (cl-typep event 'plz-event-source-event))
                       (should (equal "open" (plz-event-source-event-type event)))
                       (push event events)))
      (plz-event-source-add-listener
       source "message" (lambda (source event)
                          (should (cl-typep source 'plz-event-source))
                          (should (cl-typep event 'plz-event-source-event))
                          (should (equal "message" (plz-event-source-event-type event)))
                          (push event events)))
      (plz-event-source-add-listener
       source "close" (lambda (source event)
                        (should (cl-typep source 'plz-event-source))
                        (should (cl-typep event 'plz-event-source-event))
                        (should (equal "close" (plz-event-source-event-type event)))
                        (push event events)))
      (with-slots (ready-state url) source
        (plz-event-source-open source)
        (should (equal 'connecting ready-state))
        (while (not (equal 'closed ready-state))
          (sit-for 0.1))
        (should (equal 'closed ready-state))
        (should (equal (list "open" "message" "close")
                       (cl-remove-duplicates
                        (seq-map #'plz-event-source-event-type
                                 (reverse events)))))
        (should (equal "Hello! How can I assist you today?"
                       (thread-last
                         (reverse events)
                         (seq-filter (lambda (event)
                                       (with-slots (data type) event
                                         (and (equal "message" type)
                                              (not (equal "[DONE]" data))))))
                         (seq-map (lambda (event)
                                    (with-slots (data) event
                                      (when-let ((data (json-parse-string data))
                                                 (choice (seq-first (map-elt data "choices")))
                                                 (delta (map-elt choice "delta"))
                                                 (content (map-elt delta "content")))
                                        content))))
                         (string-join))))))))

;;;; Footer

(provide 'test-plz-event-source)

;;; test-plz-event-source.el ends here
