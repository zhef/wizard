(in-package #:WIZARD)

(defparameter *popups* nil)


(restas:define-route search-page ("/search")
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (show-acts (list
                (mi 'tpl :title "Результаты поиска:" :perm :ALL
                    :val (lambda () "Задан пустой поисковый запрос!"))))))


(defmethod searching ((category (eql :supplier)) text)
  (mapcar #'(lambda (x)
              (format nil "<a href=\"/supplier/~A\">~A</a><br/><br/>"
                      (car x)
                      (a-name (cdr x))))
          (remove-if-not #'(lambda (x)
                             (and
                              (equal 'supplier (type-of (cdr x)))
                              (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name (cdr x)))))
                              ))
                         (cons-hash-list *USER*))))


(restas:define-route search-page/post ("/search" :method :post)
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (show-acts
     (list
      (mi 'tpl :title "Результаты поиска" :perm :ALL
          :val (lambda ()
                 (let* ((text      (string-downcase (string-trim '(#\Space #\Tab #\Newline) (cdr (car (form-data))))))
                        (category  (cdr (cadr (form-data)))))
                   (cond ((= 0 (length text)) "Задан пустой поисковый запрос")
                         ((> 3 (length text)) "Слишком короткий поисковый запрос")
                         (t  (let ((results   (cond  ((string= "supplier" category) (searching :supplier text))
                                                      ((string= "all" category)      (concatenate 'list
                                                                                                  (searching :supplier text)))
                                                      (t (format nil "~A" (bprint category))))))
                                (if (null results)
                                    "Ничего не найдено"
                                    (format nil "<br/><br/>~{~A~}" results))))))))))))
