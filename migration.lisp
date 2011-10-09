(in-package #:wizard)

(connect :user "root" :password "root" :database "ktopostavljaet")
(query "SET NAMES utf8")

(defmacro with-query-select ((query-str fields) &body body)
  (let* ((fld-str (format nil "~{`~A`~^, ~}" fields))
         (fld-sym (loop :for fld :in fields :collect (intern (string-upcase fld) (find-package "WIZARD")))))
    (with-gensyms (res row)
      `(progn
         (setf ,res (caar (query (replace-all ,query-str "|:::|" ,fld-str))))
         (aif ,res
              (loop :for ,row :in ,res :collect
                 (destructuring-bind ,fld-sym
                     ,row
                   ,@body))
              nil)))))


;; Пользователи
(progn
  ;; Очистка
  (defparameter *USER* (make-hash-table :test #'equal))
  ;; Нулевой - админ
  (push-hash *USER* 'ADMIN :login "admin" :password "admin")
  ;; Застройшики и поставщики
  (with-query-select ("SELECT |:::| FROM `jos_gt_company_group_bind`"
                      ("company_id" "group_id"))
    (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company` WHERE `id`='~A'" company_id)
                        ("juridical_address_id" "actual_address_id" "head_id" "name" "email" "site" "is_diligent"))
      (let ((juridical-address)
            (actual-address)
            (contacts)
            (heads)
            (divisions))
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" juridical_address_id)
                            ("street" "house"))
          (setf juridical-address (format nil "~A ~A" street house)))
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" actual_address_id)
                            ("street" "house"))
          (setf actual-address (format nil "~A ~A" street house)))
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_employee` WHERE `id`='~A'" head_id)
                            ("second_name" "name" "patronymic" "post" "phone" "email" "user_id"))
          (setf heads (format nil "~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] "
                              post second_name name patronymic phone email)))
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_division` WHERE `company_id`='~A'" company_id)
                            ("city_id" "name" "post_index" "street" "house" "office" "phone"))
          (let ((save-name name))
            (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_city` WHERE `id`='~A'" city_id)
                                ("name"))
              (push (format nil "~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~]"
                            name save-name post_index street house office phone)
                    divisions))))
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_phone` WHERE `company_id`='~A'" company_id)
                            ("number"))
          (setf contacts (append contacts (list number))))
        (if (< group_id 5)
            ;; Поставщики - это первые четыре группы
            (progn
              (format t "~%[SUPPLIER]: ~A | ~A" name company_id)
              (setf (gethash company_id *USER*)
                    (make-instance 'SUPPLIER
                                   :login (symbol-name (gensym "LOGIN"))
                                   :password (symbol-name (gensym "PASSWORD"))
                                   :name name
                                   :email email
                                   :site site
                                   :heads heads
                                   :addresses (format nil "~{~A ~}" #|"~{~@[~A~%~]~}"~|# divisions)
                                   :status (if (equal 1 is_diligent)  :fair  :unfair)
                                   :juridical-address juridical-address
                                   :actual-address actual-address
                                   :contacts contacts)))
            ;; Застройщики - это все остальные
            (progn
              ;; (format t "~%[BUILDER]: ~A" name )
              (setf (gethash company_id *USER*)
                    (make-instance 'BUILDER
                                   :login (symbol-name (gensym "LOGIN"))
                                   :password (symbol-name (gensym "PASSWORD"))
                                   :name name
                                   :email email
                                   :site site
                                   :juridical-address juridical-address
                                   :actual-address actual-address
                                   :contacts contacts)))))))
  ;; Эксперты
  (loop :for i :from 1 :to 9 :do
     (push-hash *USER* 'EXPERT
       :name (format nil "Эксперт-~A" i)
       :login (format nil "exp~A" i)
       :password (format nil "exp~A" i))))


;; test
;; (loop :for (id . obj) :in (remove-if-not #'(lambda (x)
;;                                     (equal 'supplier (type-of (cdr x))))
;;                                 (cons-hash-list *USER*)) :do
;;    (format t "~%~A | ~A" id (a-addresses obj)))


;; Извлекаем группы ресурсов, которые у нас называются категориями
(progn
  ;; Очищаем категории ресурсы и тендеры
  (defparameter *CATEGORY* (make-hash-table :test #'equal))
  (defparameter *RESOURCE* (make-hash-table :test #'equal))
  (defparameter *TENDER*   (make-hash-table :test #'equal))
  ;; Забираем сырые данные по категориям из базы
  (with-query-select ("SELECT |:::| FROM `jos_gt_resource_group`"
                    ("id" "name" "type" "parent_id"))
    (let ((this-category (setf (gethash id *CATEGORY*)
                               (make-instance 'CATEGORY
                                              :name name
                                              :parent parent_id))))
      ;; Забираем ресурсы, принадлежащие этой категории
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource` WHERE `group_id` = '~A'" id)
                          ("id" "name" "type" "unit_id"))
        (let ((this-resource (setf (gethash id *RESOURCE*)
                                   (make-instance 'RESOURCE
                                                  :name name
                                                  :category this-category
                                                  :resource-type (if (equal 1 type) :machine :material)
                                                  :unit  (if (equal 1 unit_id) "шт." "ед.изм")))))
          (setf (a-resources this-category)
                (append (a-resources this-category)
                        (list this-resource)))))))
  ;; Связываем категории в дерево
  (maphash #'(lambda (key category)
               (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
                 (setf (a-parent category) parent-category)
                 (when parent-category
                   (setf (a-child-categoryes parent-category)
                         (append (a-child-categoryes parent-category)
                                 (list category))))))
           *CATEGORY*)
  ;; Тендеры
  (with-query-select ("SELECT |:::| FROM `jos_gt_tender`"
                      ("id" "company_id" "name" "pricetype"))
    ;; Отыскиваем компанию, которой принадлежит тендер, по company_id
    (let ((builder (gethash company_id *USER*)))
      (let ((this-tender (setf (gethash id *TENDER*)
                               (make-instance 'TENDER
                                              :name name
                                              :owner builder))))
        (setf (a-tenders builder)
              (append (a-tenders builder)
                      (list this-tender)))))))
