(in-package #:wizard)

(setf drakma:*drakma-default-external-format* :utf-8)

(defun geo-coder (addr)
  (multiple-value-bind (body status headers ret-uri stream must-close reason)
      (drakma:http-request "http://geocode-maps.yandex.ru/1.x/"
                           :method :get
                           :parameters `(("geocode" . ,addr)
                                         ("key" . ,(format nil "~A" *key*))
                                         ("format" . "json")))
    (let ((tree (json:decode-json-from-string
                 (sb-ext:octets-to-string body))))
      (maybecall tree #'car #'cdr #'car #'cdr #'cdr #'car #'cdr #'caar #'cdr #'fourth #'cdr #'car #'cdr))))

(defparameter *key*  "AKOwoE4BAAAAzn_UAAQAmXdybST_B2x-mnLcto5q_tTa2B4AAAAAAAAAAAAtC7dNu632YaEJuBnHz1d5g8a1IQ==")
(defparameter *addr* "Проспект просвещения 10")

(restas:define-route maptest ("/maptest")
  (tpl:testmap (list :mapkey *key*
                     :iehack "xmlns:vml=\"urn:schemas-microsoft-com:vml\""
                     :viewporthack "<meta name=\"viewport\" content=\"initial-scale=1.0, user-scalable=no\" />"
                     :center (format nil "~{~A~^, ~}" (split-sequence:split-sequence #\Space (geo-coder *addr*)))
                     :addr *addr*
                     )))
