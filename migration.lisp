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


;; Извлекаем поставщиков
(with-query-select ("SELECT |:::| FROM `jos_gt_company_group_bind`"
                    ("company_id" "group_id"))
  (when (< group_id 5) ;; Поставщики - это первые четыре группы
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
        ;; (format t "~%~A : ~A" name (bprint init))))))
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
          :contacts contacts)))))
