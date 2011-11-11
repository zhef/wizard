(in-package #:WIZARD)

(defparameter *popups* nil)


(restas:define-route search-page ("/search")
  (let ((session (hunchentoot:start-session)))
    (declare (ignore session))
    (show-acts (list
                (mi 'tpl :title "Результаты поиска:" :perm :ALL
                    :val (lambda () "Задан пустой поисковый запрос!"))))))


(defmethod searching ((category (eql :supplier)) text)
  (format nil "<br/><br/>~{~A~}"
          (mapcar #'(lambda (x)
                      (format nil "<a href=\"/supplier/~A\">~A</a><br/><br/>"
                              (car x)
                              (a-name (cdr x))))
                  (remove-if-not #'(lambda (x)
                                     (and
                                      (equal 'supplier (type-of (cdr x)))
                                      (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name (cdr x)))))
                                      ))
                                 (cons-hash-list *USER*)))))


(defmethod searching ((category (eql :map)) text)
  (let ((results)
        (points))
    (loop :for (id . supplier) :in (remove-if-not #'(lambda (x) (equal 'supplier (type-of (cdr x)))) (cons-hash-list *user*)) :collect
       (loop :for supplier-resource :in (a-resources supplier) :collect
          (if (not (null (search text (string-downcase (string-trim '(#\Space #\Tab #\Newline) (a-name (a-resource supplier-resource)))))))
              (progn
                (push supplier results)
                (return)))))
    (setf points
          (mapcar #'(lambda (x)
                      (mi 'yapoint
                          :title (a-name x)
                          :descr (a-actual-address x)
                          :coord (geo-coder (a-actual-address x))))
                  results))
    (show-block
     (mi 'yamap
         :center-coord "30.313622, 59.937720"
         :mark-points points))))


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
                                                     ((string= "map"      category) (searching :map text))
                                                     ((string= "all" category)      (concatenate 'list
                                                                                                 (searching :supplier text)))
                                                     (t (format nil "~A" (bprint category))))))
                               (if (null results)
                                   "Ничего не найдено"
                                   results)))))))))))
