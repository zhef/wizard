(in-package #:wizard)

(connect :user "root" :password "root" :database "ktopostavlyaet")
(query "SET NAMES utf8")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; RESOURCES & CATEGORYES


(defun categoryes-and-resources ()
  "Переносим ресурсы и категории"
  (clrhash *CATEGORY*)
  (clrhash *RESOURCE*)
  ;; Забираем все единицы измерения
  (let ((measures (make-hash-table :test #'equal)))
    (with-query-select ("SELECT |:::| FROM `jos_gt_measure_unit`"
                        ("id" "name"))
      (setf (gethash id measures) name))
    ;; Забираем сырые данные по категориям из базы
    (with-query-select ("SELECT |:::| FROM `jos_gt_resource_group`"
                        ("id" "name" "type" "parent_id"))
      (let ((save-group-id   id)
            (save-group-name name)
            (this-category   (setf (gethash id *CATEGORY*)
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
            ;; TODO: Тут  нужно еще связать ресурс с ценой через справочник
            t)t)t)t)t)
  ;; Связываем категории в дерево - здесь parent становится категорией, и слот child-categoryes становится валидным
  (maphash #'(lambda (key category)
               (let ((parent-category (gethash (a-parent category) *CATEGORY*)))
                 (setf (a-parent category) parent-category)
                 (when parent-category
                   (append-link (a-child-categoryes parent-category) category))))
           *CATEGORY*)
  t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ADMIN, BUILDERS, SUPPLIERS and EXPERTS


(defun users ()
  ;; Очистка
  (clrhash *USER*)
  ;; Нулевой - админ
  (push-hash *USER* 'ADMIN :login "admin" :password "admin")
  ;;
  (let ((hash-city    (make-hash-table :test #'equal))
        (company_type (make-hash-table :test #'equal)))
    ;; Получаем города и записываем их в хэш-таблицу
    (with-query-select ("SELECT |:::| FROM `jos_gt_city`"
                        ("id" "name"))
      (setf (gethash id hash-city) name))
    ;; Идентификаторы всех поставщиков предзаносим в company_type
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
                              ("city_id" "street" "house"))
            (setf juridical-address (format nil "~A ~A ~A" (gethash city_id hash-city) street house)))
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_address` WHERE `id`='~A'" actual_address_id)
                              ("city_id" "street" "house"))
            (setf actual-address (format nil "~A ~A ~A" (gethash city_id hash-city) street house)))
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
          ;; to *USER*
          (if (gethash company_id company_type)
              ;; Поставщик
              (progn
                ;; (format t "~%[SUPPLIER]: ~A | ~A" name company_id)
                (let ((supplier (setf (gethash company_id *USER*)
                                      (make-instance 'SUPPLIER
                                                     :login (symbol-name (gensym "LOGIN"))
                                                     :password (symbol-name (gensym "PASSWORD"))
                                                     :name (format nil "~A" name)
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
                                                     :contacts contacts))))
                  ;; Это поставщик, значит у него могут быть ресурсы
                  (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_company_resource` WHERE `company_id`='~A'" company_id)
                                      ("id" "resource_id" "price"))
                    (let ((supplier-resource (setf (gethash id *SUPPLIER-RESOURCE*)
                                                   (make-instance 'SUPPLIER-RESOURCE
                                                                  :owner supplier
                                                                  :resource (gethash resource_id *RESOURCE*)
                                                                  :price price))))
                      (append-link (a-resources supplier) supplier-resource)))
                  )
                company_id)
              ;; else
              ;; Застройщик
              (progn
                ;; (format t "~%[BUILDER]: ~A" name )
                (setf (gethash company_id *USER*)
                      (make-instance 'BUILDER
                                     :login (symbol-name (gensym "LOGIN"))
                                     :password (symbol-name (gensym "PASSWORD"))
                                     :name (format nil "~A" name)
                                     :email email
                                     :site site
                                     :juridical-address juridical-address
                                     :actual-address actual-address
                                     :contacts contacts))
                nil))))))

  ;; ;; Эксперты
  (loop :for i :from 1 :to 9 :do
     (push-hash *USER* 'EXPERT
       :name (format nil "Эксперт-~A" i)
       :login (format nil "exp~A" i)
       :password (format nil "exp~A" i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TENDERS


(defun tenders ()
  "Тендеры"
  (clrhash *TENDER*)
  (clrhash *TENDER-RESOURCE*)
  (clrhash *OFFER*)
  (clrhash *OFFER-RESOURCE*)
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
                                             :all        (make-interval :begin tender_begin    :end tender_end)
                                             :claim      (make-interval :begin order_begin     :end order_end)
                                             :analize    (make-interval :begin process_begin   :end process_end)
                                             :interview  (make-interval :begin talking_begin   :end talking_end)
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
                                                         :resource (gethash resource_id *RESOURCE*)
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
                                                     :owner supplier
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
                                                                    :price-result (aif price2 price2 0)
                                                                    :comment comments
                                                                    :delivery (if (equal 1 deliver) T NIL)
                                                                    :delivery-price deliver_cost
                                                                    :marked marked
                                                                    :rank place))))
                      ;; Связывать с tender-resouce не надо (!)
                      ;; А вот с offer свяжем
                      (append-link (a-resources this-offer) this-offer-resource)
                      ))))))))))


(push-hash *POST* 'POST
  :title "Первая новость"
  :date  "07.11.2011"
  :photo-announce nil
  :announce "Это текст анонса первой новости"
  :text "Это текст первой новости")


(push-hash *POST* 'POST
  :title "Вторая новость"
  :date  "07.11.2011"
  :photo-announce nil
  :announce "Это текст анонса второй новости"
  :text "Это текст второй новости")

;; (clrhash *POST*)



(categoryes-and-resources)
(users)
(tenders)

(print "RELOAD FINISHED")

(restas:start '#:wizard :port 8081)

(restas:debug-mode-on)

