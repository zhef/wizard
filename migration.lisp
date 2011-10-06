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


;; Извлекаем поставщиков и застройщиков
(with-query-select ("SELECT |:::| FROM `jos_gt_company_group_bind`"
                    ("company_id" "group_id"))
  (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company` WHERE `id`='~A'" company_id)
                      ("juridical_address_id" "actual_address_id" "name" "email" "site" "is_diligent"))
    (let ((juridical-address)
          (actual-address)
          (contacts))
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" juridical_address_id)
                          ("street" "house"))
        (setf juridical-address (format nil "~A ~A" street house)))
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" actual_address_id)
                          ("street" "house"))
        (setf actual-address (format nil "~A ~A" street house)))
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_phone` WHERE `company_id`='~A'" company_id)
                          ("number"))
        (setf contacts (append contacts (list number))))
      (if (< group_id 5)
          (progn
            (format t "~%[SUPPLIER]: ~A" name)
            ;; Поставщики - это первые четыре группы
            (push-hash *USER* 'SUPPLIER
            :id company_id
            :login (symbol-name (gensym "LOGIN"))
            :password (symbol-name (gensym "PASSWORD"))
            :name name
            :email email
            :site site
            :status (if (equal 1 is_diligent)  :fair  :unfair)
            :juridical-address juridical-address
            :actual-address actual-address
            :contacts contacts)
            )
          ;; Застройщики - это все остальные
          (progn
            (format t "~%[BUILDER]: ~A" name )
            (push-hash *USER* 'BUILDER
            :id company_id
            :login (symbol-name (gensym "LOGIN"))
            :password (symbol-name (gensym "PASSWORD"))
            :name name
            :email email
            :site site
            :juridical-address juridical-address
            :actual-address actual-address
            :contacts contacts))))))


;; Извлекаем группы ресурсов, которые у нас называются категориями
(progn
  ;; Очищаем категории
  (defparameter *CATEGORY* (make-hash-table :test #'equal))
  ;; Забираем сырые данные из базы
  (with-query-select ("SELECT |:::| FROM `jos_gt_resource_group`"
                    ("id" "name" "type" "parent_id"))
    (setf (gethash id *CATEGORY*)
          (make-instance 'CATEGORY
                         :name name
                         :parent parent_id)))
  ;; Связываем ресурсы в дерево
  (maphash #'(lambda (key category)
               (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
                 (setf (a-parent category) parent-category)
                 (when parent-category
                   (setf (a-child-categoryes parent-category)
                         (append (a-child-categoryes parent-category)
                                 (list category))))))
           *CATEGORY*))

;; test
;; (maphash #'(lambda (key category)
;;              (format t "~%~A : ~A" key (a-parent category)))
;;              *CATEGORY*)
