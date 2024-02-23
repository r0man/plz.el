;;; test-plz-stream.el --- Streaming tests for plz.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Streaming tests for plz.el

;;; Code:

;;;; Requirements

(require 'ert)
(require 'json)
(require 'let-alist)
(require 'map)

(require 'plz-stream)

;; (plz-deftest plz-stream-bytes ()
;;   (let* ((complete-response)
;;          (partial-response)
;;          (process (plz 'get (url "/stream-bytes/100000")
;;                     :as 'binary
;;                     :during (lambda (content)
;;                               (add-to-list 'partial-response content))
;;                     :then (lambda (content)
;;                             (setq complete-response content)))))
;;     (plz-test-wait process)
;;     (should (> (length partial-response) 1))
;;     (should (string-equal complete-response
;;                           (with-temp-buffer
;;                             (toggle-enable-multibyte-characters)
;;                             (set-buffer-file-coding-system 'raw-text)
;;                             (seq-doseq (content (reverse partial-response))
;;                               (insert content))
;;                             (buffer-string))))))

;; (plz-deftest plz-stream-text ()
;;   (let* ((complete-response)
;;          (partial-response)
;;          (process (plz 'get (url "/stream/1000")
;;                     :during (lambda (content)
;;                               (add-to-list 'partial-response content))
;;                     :then (lambda (content)
;;                             (setq complete-response content)))))
;;     (plz-test-wait process)
;;     (should (> (length partial-response) 1))
;;     (should (equal complete-response (string-join (reverse partial-response) "")))))

(ert-deftest plz--parse-content-type-header-test ()
  (should (null (plz--parse-content-type-header nil)))
  (should (null (plz--parse-content-type-header "")))
  (should (equal '("text/html" nil) (plz--parse-content-type-header "text/html")))
  (should (equal '("text/html" ((charset . "utf-8")))
                 (plz--parse-content-type-header "text/html; charset=utf-8")))
  (should (equal '("multipart/form-data" ((boundary . "something ") (charset . "utf-8")))
                 (plz--parse-content-type-header "multipart/form-data; boundary=something ; charset=utf-8"))))

;;;; Footer

(provide 'test-plz-stream)

;;; test-plz-stream.el ends here
