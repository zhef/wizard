(in-package #:wizard)

(connect :user "root" :password "root" :database "ktopostavljaet")
(query "SET NAMES utf8")

(defmacro with-query-select ((query-str fields) &body body)
  (let* ((fld-str (format nil "~{`~A`~^, ~}" fields))
         (fld-sym (loop :for fld :in fields :collect (intern (string-upcase fld) (find-package "WIZARD")))))
    (with-gensyms (res row)
      `(let ((,res (caar (query (replace-all ,query-str "|:::|" ,fld-str)))))
         (aif ,res
              (loop :for ,row :in ,res :collect
                 (destructuring-bind ,fld-sym
                     ,row
                   ,@body))
              nil)))))


;; Таблицы
'(jos_gt_city
  :jos_gt_company
  :jos_gt_company_address
  :jos_gt_company_details
  :jos_gt_company_division
  :jos_gt_company_employee
  ;; jos_gt_company_group
  :jos_gt_company_group_bind
  :jos_gt_company_phone
  jos_gt_company_resource
  ;; jos_gt_company_right_form
  jos_gt_discount
  :jos_gt_measure_unit
  jos_gt_notification
  jos_gt_notification_type
  :jos_gt_resource
  :jos_gt_resource_code
  jos_gt_resource_duplicate
  :jos_gt_resource_group
  :jos_gt_resource_price
  :jos_gt_resource_price_level
  jos_gt_resource_request
  jos_gt_tender
  jos_gt_tender_form
  jos_gt_tender_form_answer_variant
  jos_gt_tender_form_question
  jos_gt_tender_offer
  jos_gt_tender_order
  jos_gt_tender_order_answer
  jos_gt_tender_order_resource
  jos_gt_tender_order_uploaded_file
  jos_gt_tender_resource
  jos_gt_tender_status
  jos_gt_tender_supplier
  jos_gt_tender_uploaded_file
  jos_gt_uploaded_file
  jos_gt_user_notification_type)


;; jos_gt_resource :
;; :id
;; :name
;; :unit_id    measure_unit
;; :group_id   resource_group
;; :type       mashine : matherial


(defun categoryes-and-resources ()
  "Переносим ресурсы и категории"
  (defparameter *CATEGORY* (make-hash-table :test #'equal))
  (defparameter *RESOURCE* (make-hash-table :test #'equal))

  ;; Забираем все единицы измерения
  (let ((measures (make-hash-table :test #'equal)))
    (with-query-select ("SELECT |:::| FROM `jos_gt_measure_unit`"
                        ("id" "name"))
      (setf (gethash id measures) name))

    ;; Забираем сырые данные по категориям из базы
    (defparameter gr-cnt 0)
    (with-query-select ("SELECT |:::| FROM `jos_gt_resource_group`"
                        ("id" "name" "type" "parent_id"))
      (let ((save-group-id     id)
            (save-group-name   name)
            (this-category     (setf (gethash id *CATEGORY*)
                                     (make-instance 'CATEGORY
                                                    :name name
                                                    ;; здесь parent еще числовой
                                                    :parent parent_id))))
        ;; Забираем ресурсы, принадлежащие этой категории
        (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource` WHERE `group_id` = '~A'" id)
                            ("id" "name" "unit_id" "group_id" "type"))

          ;; Создаем ресурс, связывая его с категорией
          (let ((this-resource (setf (gethash id *RESOURCE*)
                                     (make-instance 'RESOURCE
                                                    :name name
                                                    :category this-category
                                                    :resource-type (if (equal 1 type) :material :machine)
                                                    :unit (gethash unit_id measures)))))

            ;; Выдаем warning если в базе связь ресурс-категория не совпадает со связью категория-ресурс
            (unless (equal save-group-id group_id)
              (format t "~&warn: not equal link to category (~A | ~A) and resource (~A | ~A)"
                      save-group-id
                      save-group-name
                      id
                      name))

            ;; Добавляем этот ресурс в категорию, т.е. связываем категорию с ресурсом
            (append-link (a-resources this-category) this-resource)
            t)t)t)t)t)

  ;; Связываем категории в дерево - здесь parent становится категорией, и слот child-categoryes становится валидным
  (maphash #'(lambda (key category)
               (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
                 (setf (a-parent category) parent-category)
                 (when parent-category
                   (append-link (a-child-categoryes parent-category) category))))
           *CATEGORY*)
  t)


(categoryes-and-resources)


(defun resource-price ()
  (defparameter *PRICE-REFERENCE* (make-hash-table :test #'equal))
  ;; Забираем все справочники
  (with-query-select ("SELECT |:::| FROM `jos_gt_resource_price_level`"
                      ("id" "level" "title"))
    ;; Создаем справочник
    (let ((reference-id id)
          (this-price-reference (setf (gethash id *PRICE-REFERENCE*)
                                      (make-instance 'PRICE-REFERENCE
                                                     :date level
                                                     :name title
                                                     :resource-prices nil))))
      ;; (format t "~& ~A | ~A"  (a-date this-price-reference) (a-name this-price-reference)) ;;
      ;; Забираем все цены для этого справочника
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource_price` WHERE `level_id` = ~A" reference-id)
                          ("id" "code_id" "estimate" "wholesale" "level_id"))
        ;; Создаем цену, связывая ее со справочником
        (let ((this-resource-price (setf (gethash id *RESOURCE-PRICE*)
                                         (make-instance 'RESOURCE-PRICE
                                                        :estimate estimate
                                                        :wholesale wholesale
                                                        :price-level this-price-reference
                                                        :resource nil))))
          ;; Связываем справочник с созданной ценой
          (append-link (a-resource-prices this-price-reference) this-resource-price)
          ;; По code_id получаем код ресурса
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource_code` WHERE `code`=~A" code_id)
                              ("resource_id"))
            ;; По коду ресурса получаем ресурс, с которым связываем цену
            (let ((this-resource (gethash resource_id *RESOURCE*)))
              (when this-resource ;; иногда бывает nil, что вероятно связано с неполным дампом
                (setf (a-resource this-resource-price) this-resource)
                ;; Добавляем цену к ресурсу
                (append-link (a-resource-prices this-resource) this-resource-price))
              t)t)t)t)t)t)t)


;; tests
;; (hash-table-count *PRICE-REFERENCE*)

;; tests
;; (maphash #'(lambda (k v)
;;              (print (list k (a-name v)))
;;              (print (length (a-resource-prices v)))
;;              (loop :for a :in (a-resource-prices v) :do
;;                 (when (a-resource a)
;;                   (print (list (a-estimate a)
;;                                (a-wholesale a)
;;                                (a-price-level a)
;;                                (a-name (a-resource a))
;;                                (a-name (a-category (a-resource a)))))
;;                   (return))))
;;          *PRICE-REFERENCE*)

(resource-price)


(defun users ()
  ;; Очистка
  (defparameter *USER* (make-hash-table :test #'equal))
  ;; Нулевой - админ
  (push-hash *USER* 'ADMIN :login "admin" :password "admin")
  ;; Идентификаторы всех поставщиков предзаносим в company_type
  (let ((company_type (make-hash-table :test #'equal)))
    (with-query-select ("SELECT |:::| FROM `jos_gt_company_group_bind`"
                        ("company_id" "group_id"))
      (when (< group_id 5)
        (setf (gethash company_id company_type) 1)))
    ;; Забираем все компании
    (with-query-select ("SELECT |:::| FROM `jos_gt_company`"
                        ("id" "juridical_address_id" "actual_address_id" "head_id" "details_id" "name" "email" "site" "is_diligent"))
      (let ((company_id id))
        ;; Для каждой собираем все адреса, телефоны и прочее
        (let ((juridical-address) (actual-address) (contacts) (heads) (divisions)
              (inn*) (ogrn*) (bank-name*) (bik*) (correspondent_account*) (sattlement_account*))
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
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_details` WHERE `id`='~A'" details_id)
                              ("inn" "ogrn" "bank" "bik" "correspondent_account" "sattlement_account"))
            (setf inn* inn)
            (setf ogrn* ogrn)
            (setf bank* bank)
            (setf bik* bik)
            (setf correspondent_account* correspondent_account)
            (setf sattlement_account* sattlement_account))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_division` WHERE `company_id`='~A'" company_id)
                              ("city_id" "name" "post_index" "street" "house" "office" "phone"))
            (let ((save-name name))
              (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_city` WHERE `id`='~A'" city_id)
                                  ("name"))
                (push (format nil "~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~] ~@[~A~]"
                              name save-name post_index street house office phone)
                      divisions)
                t))t)
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_phone` WHERE `company_id`='~A'" company_id)
                              ("number"))
            (append-link contacts number)
            t)

          ;; Сохраняем в *USER*
          (if (not (null (gethash company_id company_type)))
              ;; Поставщик
              (progn
                (format t "~%[SUPPLIER]: ~A | ~A" name company_id)
                (let ((supplier (setf (gethash company_id *USER*)
                                      (make-instance 'SUPPLIER
                                                     :login (symbol-name (gensym "LOGIN"))
                                                     :password (symbol-name (gensym "PASSWORD"))
                                                     :name (format nil ":::~A" name)
                                                     :email email
                                                     :site site
                                                     :heads heads
                                                     :inn inn*
                                                     :ogrn ogrn*
                                                     :bank-name bank-name*
                                                     :bik bik*
                                                     :corresp-account correspondent_account*
                                                     :client-account sattlement_account*
                                                     :addresses (format nil "~{~A ~}" divisions)
                                                     :status (if (equal 1 is_diligent)  :fair  :unfair)
                                                     :juridical-address juridical-address
                                                     :actual-address actual-address
                                                     :contacts contacts))))))
              ;; Застройщик
              (progn
                (format t "~%[BUILDER]: ~A" name )
                (setf (gethash company_id *USER*)
                      (make-instance 'BUILDER
                                     :login (symbol-name (gensym "LOGIN"))
                                     :password (symbol-name (gensym "PASSWORD"))
                                     :name (format nil ":::~A" name)
                                     :email email
                                     :site site
                                     :juridical-address juridical-address
                                     :actual-address actual-address
                                     :contacts contacts))
                t))t))t)
    ;; Эксперты
    (loop :for i :from 1 :to 9 :do
       (push-hash *USER* 'EXPERT
         :name (format nil "Эксперт-~A" i)
         :login (format nil "exp~A" i)
         :password (format nil "exp~A" i)))))

(users)

;; tests
;; (maphash #'(lambda (k v)
;;              (if (and (not (equal 'ADMIN  (type-of v)))
;;                       (not (equal 'EXPERT (type-of v))))
;;                  (print (list k v (a-name v)))))
;;          *USER*)

;; test
;; (loop :for (id . obj) :in (remove-if-not #'(lambda (x)
;;                                              (equal 'supplier (type-of (cdr x))))
;;                                          (cons-hash-list *USER*)) :do
;;    (format t "~%~A | ~A" id (a-addresses obj)))



(defun tenders ()
  "Тендеры"
  (defparameter *TENDER*                      (make-hash-table :test #'equal))
  (defparameter *TENDER-RESOURCE*             (make-hash-table :test #'equal))
  (defparameter *OFFER*                       (make-hash-table :test #'equal))
  (defparameter *OFFER-RESOURCE*              (make-hash-table :test #'equal))
  ;; Забираем все тендеры
  (with-query-select ("SELECT |:::| FROM `jos_gt_tender`"
                      ("id" "company_id" "name" "tender_begin" "tender_end" "order_begin" "order_end" "process_begin" "process_end"
                            "talking_begin" "talking_end" "resulting_begin" "resulting_end" "total" "pricetype" "pricesource"
                            "coverage" "status_id"))
    (let* ((save-tender-id id)
           (builder (gethash company_id *USER*))           ;; Отыскиваем компанию, которой принадлежит тендер, по company_id
           (this-tender (setf (gethash id *TENDER*)        ;; Создаем тендер, связывая его с владельцем
                               (make-instance 'TENDER
                                              :name name
                                              :owner builder
                                              :all        (make-interval :begin tender_begin :end tender_end)
                                              :claim      (make-interval :begin order_begin :end order_end)
                                              :analize    (make-interval :begin process_begin :end process_end)
                                              :interview  (make-interval :begin talking_begin :end talking_end)
                                              :result     (make-interval :begin resulting_begin :end resulting_end)
                                              :status (ecase status_id (1 :unactive) (2 :active) (3 :cancelled) (4 :finished))))))

      ;; Связываем владельца с созданным тендером
      (append-link (a-tenders builder) this-tender)
      ;; Забираем ресурсы тендера
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_resource` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "tender_id" "resource_id" "quantity" "price" "pricedate" "comments" "deliver" "is_basis"))
        ;; Создаем объекты tender-resource, связанные с тендером
        (let ((this-tender-resource (setf (gethash id *TENDER-RESOURCE*)
                                          (make-instance 'TENDER-RESOURCE
                                                         :tender this-tender
                                                         :resource (gethash resource_id *TENDER-RESOURCE*)
                                                         :quantity quantity
                                                         :price price
                                                         :price-date pricedate
                                                         :comment comments
                                                         :delivery (if (equal deliver 1) T NIL)
                                                         :basic   (if (equal is_basis 1) T NIL)
                                                         ))))
          ;; Связываем тендер с созданным tender-resource
          (append-link (a-resources this-tender) this-tender-resource)
          t))
      ;; Забираем поставщиков тендера
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_supplier` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "company_id" "is_invited"))  ;;  в этой таблице is_invited - приглашен на собеседование, им потом можно allow_modify
        ;; Связываем их с тендером
        (append-link (a-suppliers this-tender) (gethash company_id *USER*)))
      ;; Забираем приглашения поставщикам на этот тендер
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_offer` WHERE `tender_id` = ~A" save-tender-id)
                          ("id" "tender_id" "company_id" "has_been_read"))
        (let ((supplier (gethash company_id *USER*)))
          (if (not (equal (type-of supplier) 'supplier))
              (format t "~%warn: company is not supplier ~A | ~A ::: ~A"
                      (type-of supplier)
                      (a-name supplier)
                      company_id)
              ;; ELSE
              (let ((this-offer (setf (gethash id *OFFER*)
                                      (make-instance 'OFFER
                                                     :supplier supplier
                                                     :tender this-tender
                                                     :status :sended ;; не паримся так как статусы уже отличаются
                                                     ))))
                ;; Связываем тендер c приглашением
                (append-link (a-offers this-tender) this-offer)
                ;; Связываем поставщика с приглашением
                (append-link (a-offers supplier) this-offer)
                ;; Забираем заявки поставщиков, которые поставщики отправили в ответ на каждое приглашение (один к одному)
                (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_order` WHERE `offer_id` = ~A" id)
                                    ("id" "offer_id" "status" "has_been_read" "allow_modify"))
                  ;; Забираем ресурсы, которые привязаны к заявке
                  (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_tender_order_resource` WHERE `order_id` = ~A" id)
                                      ("id" "order_id" "tender_resource_id" "quantity" "price" "comments"
                                            "deliver" "deliver_cost" "marked" "price2" "place"))
                    ;; Создаем offer-resource
                    (let ((this-offer-resource (setf (gethash id *OFFER-RESOURCE*)
                                                     (make-instance 'OFFER-RESOURCE
                                                                    :offer this-offer
                                                                    :tender-resource (gethash tender_resource_id *TENDER-RESOURCE*)
                                                                    :quantity quantity
                                                                    :price price
                                                                    :price-result price2
                                                                    :comment comments
                                                                    :delivery (if (equal 1 deliver) T NIL)
                                                                    :delivery-price deliver_cost
                                                                    :marked marked
                                                                    :rank place))))
                      ;; Связывать с tender-resouce не надо (!)
                      ;; А вот с offer свяжем
                      (append-link (a-resources this-offer) this-offer-resource)
                      ))))))))))

(tenders)


#|

Когда забили ресурсы в тендер система находит всех поставщиков этих ресурсов
и заносит их в tender_supplier, где создатель тендера может некоторых их них удалить или добавить своего
в этой таблице is_invited - мемоизация, не переносить

tender_offer - это приглашение на тендер поставщику

Поставщик в ответ на приглашение может создать заявку (tender_order), которая через offer_id привязана к приглашению
status - это для того чтобы можно было заявку отменить

При создании заявки, создаются записи в tender_order_resource, где
order_id - заявка
tender_resource_id - запись в таблице tender_resource, чтобы увидеть рекомендуемую цену, обьем итд

ФИНАЛЬНЫЕ ТАбЛИЦЫ - перечень ресурсов : поставщики

#|
