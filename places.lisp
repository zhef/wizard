(in-package #:wizard)

(defparameter *places*
  '(
    ;; Главная страница
    (:place                main
     :url                  "/"
     :navpoint             "Главная"
     :actions
     '((:tpl               "Главная"
        :val               (funcall (find-symbol "MAIN" 'tpl)))))

    ;; Страница регистрации
    (:place                register
     :url                  "/register"
     :actions
     '((:linear            "Регистрация"
        :entity            supplier
        :perm              :all
        :val               :clear
        :fields            '((:fld login)
                             (:fld password)
                             (:fld email)
                             (:fld name)
                             (:fld inn)
                             (:fld ogrn)
                             (:fld juridical-address)
                             (:fld actual-address)
                             (:fld contact-person)
                             (:fld contact-phone)
                             (:btn "Зарегистрироваться"
                              :perm :all
                              :width 120
                              :act (multiple-value-bind (obj id)
                                       (push-hash *USER* 'SUPPLIER)
                                     (PROGN
                                       (SETF (A-LOGIN OBJ) (CDR (ASSOC "LOGIN" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-PASSWORD OBJ) (CDR (ASSOC "PASSWORD" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-EMAIL OBJ) (CDR (ASSOC "EMAIL" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-NAME OBJ) (CDR (ASSOC "NAME" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-INN OBJ) (CDR (ASSOC "INN" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-OGRN OBJ) (CDR (ASSOC "OGRN" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-JURIDICAL-ADDRESS OBJ) (CDR (ASSOC "JURIDICAL-ADDRESS" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-ACTUAL-ADDRESS OBJ) (CDR (ASSOC "ACTUAL-ADDRESS" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-CONTACT-PERSON OBJ) (CDR (ASSOC "CONTACT-PERSON" (FORM-DATA) :TEST #'EQUAL)))
                                       (SETF (A-CONTACT-PHONE OBJ) (CDR (ASSOC "CONTACT-PHONE" (FORM-DATA) :TEST #'EQUAL)))
                                       (setf (a-status obj) :unfair)
                                       (hunchentoot:redirect (format nil "/supplier/~A" id))
                                       )))
                             ))))


    ;; Новости
    (:place                posts
     :url                  "/posts"
     :navpoint             "Новости"
     :actions
     '((:announce          "Анонсы"
        :entity            post-item
        :val               (cons-hash-list *POST-ITEM*)
        :fields            '((:fld title)
                             (:fld date)
                             (:fld announce-photo)
                             (:fld announce)))))

    ;; Новость
    (:place                post
     :url                  "/post/:id"
     :actions
     '((:post              "%|title|%"
        :entity            post-item
        :val               (gethash (cur-id) *POST-ITEM*)
        :fields            '((:fld title)
                             (:fld date)
                             (:fld text-photo)
                             (:fld text)))))


    ;; Аналитика
    (:place                anal
     :url                  "/analytics"
     :navpoint             "Аналитика"
     :actions
     '((:none              "Аналитика")))


    ;; Каталог материалов
    (:place                material
     :url                  "/material"
     :navpoint             "Каталог ресурсов"
     :actions
     '((:grid             "Группы"
        :perm              :all
        :entity            category
        :val               (cons-inner-objs *CATEGORY*
                            (a-child-categoryes
                             (cdr (car (remove-if-not #'(lambda (x)
                                                          (null (a-parent (cdr x))))
                                                      (cons-hash-list *CATEGORY*))))))
        :height            400
        :fields            '((:fld name :xref "category" :width 900)))))


    ;; ;; Каталог материалов
    (:place                machine
     :url                  "/machine"
     :navpoint             "Строительная техника"
     :actions
     '((:grid              "Группы"
        :perm              :all
        :entity            category
        :val               (cons-inner-objs *CATEGORY*
                            (a-child-categoryes
                             (cdr (cadr (remove-if-not #'(lambda (x)
                                                           (null (a-parent (cdr x))))
                                                       (cons-hash-list *CATEGORY*))))))
        :height            400
        :fields            '((:fld name :xref "category" :width 900)))))


    ;; Каталог ресурсов - содержимое категории
    (:place                category
     :url                  "/category/:id"
     :actions
     '((:linear             "Группа"
        :perm               :all
        :entity             category
        :val                :clear
        :fields             '((:grid              "Подгруппы"
                               :perm              :all
                               :entity            category
                               :val               (cons-inner-objs *CATEGORY* (a-child-categoryes (gethash (cur-id) *CATEGORY*)))
                               :fields            '((:fld name :xref "category" :width 900)))
                              (:grid              "Ресурсы группы"
                               :perm              :all
                               :entity            resource
                               :val               (remove-if-not #'(lambda (x)
                                                                     (equal (a-category (cdr x))
                                                                            (gethash (cur-id) *CATEGORY*)))
                                                   (cons-hash-list *RESOURCE*))
                               :fields            '((:fld name :xref "resource" :width 900)))))))

    ;; Страница ресурса (ресурсы редактированию не подвергаются)
    (:place                resource
     :url                  "/resource/:id"
     :actions
     '((:linear            "Ресурс"
        :perm              :all
        :entity            resource
        :val               (gethash (cur-id) *RESOURCE*)
        :fields            '((:fld name)
                             (:fld category)
                             (:fld resource-type)
                             (:fld unit)))))

    ;; ;; Личный кабинет Администратора
    ;; (:place                admin
    ;;  :url                  "/admin"
    ;;  ;; :navpoint             "Администратор"
    ;;  :actions
    ;;  '((:linear            "Изменить себе пароль"
    ;;     :perm              :admin
    ;;     :entity            admin
    ;;     :val               (cur-user)
    ;;     :fields            '((:fld login)
    ;;                          (:fld password)
    ;;                          (:btn "Изменить пароль"
    ;;                           :perm :all
    ;;                           :act (let ((obj (cur-user)))
    ;;                                  (with-obj-save obj
    ;;                                    LOGIN
    ;;                                    PASSWORD)))
    ;;                          (:btn   "Кнопка всплывающего окна"
    ;;                           :perm  :all
    ;;                           :popup '(:linear            "Заголовок всплывающего окна"
    ;;                                    :perm              :admin
    ;;                                    :entity            admin
    ;;                                    :val               (cur-user)
    ;;                                    :fields            '((:fld login)
    ;;                                                         (:fld password)
    ;;                                                         (:btn "Изменить пароль"
    ;;                                                         :perm :all
    ;;                                                          :act (let ((obj (cur-user)))
    ;;                                                                 (with-obj-save obj
    ;;                                                                   LOGIN
    ;;                                                                   PASSWORD))))))))
    ;;    (:linear            "Создать аккаунт эксперта"
    ;;     :perm              :admin
    ;;     :entity            expert
    ;;     :val               :clear
    ;;     :fields            '((:fld login)
    ;;                          (:fld password)
    ;;                          (:fld name)
    ;;                          (:btn "Создать новый аккаунт эксперта"
    ;;                           :perm :all
    ;;                           :act (progn
    ;;                                  (push-hash *USER* 'EXPERT
    ;;                                    :login (cdr (assoc "LOGIN" (form-data) :test #'equal))
    ;;                                    :password (cdr (assoc "PASSWORD" (form-data) :test #'equal))
    ;;                                    :name (cdr (assoc "NAME" (form-data) :test #'equal)))
    ;;                                  (hunchentoot:redirect (hunchentoot:request-uri*))))))
    ;;    (:grid             "Эксперты"
    ;;     :perm              :admin
    ;;     :entity            expert
    ;;     :val               (remove-if-not #'(lambda (x)
    ;;                                           (equal 'expert (type-of (cdr x))))
    ;;                         (cons-hash-list *USER*))
    ;;     :fields            '((:fld name)
    ;;                          (:fld login)
    ;;                          (:btn  "Удалить"
    ;;                           :perm :all
    ;;                           :act  (let ((key (get-btn-key (caar (form-data)))))
    ;;                                   (remhash key *USER*)
    ;;                                   (hunchentoot:redirect (hunchentoot:request-uri*))))
    ;;                          (:btn "Страница эксперта"
    ;;                           :perm :all
    ;;                           :act (to "/expert/~A" (caar (form-data))))))
    ;;    (:grid            "Заявки поставщиков на добросовестность"
    ;;     :perm              :admin
    ;;     :entity            supplier
    ;;     :val               (remove-if-not #'(lambda (x)
    ;;                                           (and (equal 'supplier (type-of (cdr x)))
    ;;                                                (equal (a-status (cdr x)) :request)
    ;;                                                ))
    ;;                         (cons-hash-list *USER*))
    ;;     :fields            '((:fld name)
    ;;                          (:fld login)
    ;;                          (:btn "Сделать добросовестным"
    ;;                           :perm :all
    ;;                           :act (let ((key (get-btn-key (caar (form-data)))))
    ;;                                  (setf (a-status (gethash key *USER*)) :fair)
    ;;                                  (hunchentoot:redirect (hunchentoot:request-uri*))))))))

    ;; ;; Список экспертов
    ;; (:place                experts
    ;;  :url                  "/expert"
    ;;  ;; :navpoint             "Эксперты"
    ;;  :actions
    ;;  '((:grid              "Эксперты"
    ;;     :perm              :all
    ;;     :entity            expert
    ;;     :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'EXPERT)) (cons-hash-list *USER*))
    ;;     :fields            '((:fld name)
    ;;                          (:fld login)
    ;;                          (:btn  "Страница эксперта"
    ;;                           :perm :all
    ;;                           :act  (to "/expert/~A" (caar (form-data))))))))
    ;; ;; Страница эксперта
    ;; (:place                expert
    ;;  :url                  "/expert/:id"
    ;;  :actions
    ;;  '((:linear            "Эксперт"
    ;;     :perm              :all
    ;;     :entity            expert
    ;;     :val               (gethash (cur-id) *USER*)
    ;;     :fields            '((:fld name)
    ;;                          (:fld login)))))

    ;; Список поставщиков
    (:place                suppliers
     :url                  "/supplier"
     :navpoint             "Поставщики"
     :actions
     '((:grid              "Каталог поставщиков"
        :perm              :all
        :entity            supplier
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'SUPPLIER))  (cons-hash-list *USER*))
        :fields            '((:fld name :xref "supplier" :width 300)
                             (:fld actual-address :width 600)))))

    ;; Страница поставщика
    (:place                supplier
     :url                  "/supplier/:id"
     :actions
     '((:linear            "Поставщик"
        :perm              :all
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '((:fld name)
                             (:fld status)
                             (:fld juridical-address)
                             (:fld actual-address)
                             (:fld contacts)
                             (:fld email)
                             (:fld site)
                             (:fld heads)
                             (:fld inn)
                             (:fld kpp)
                             (:fld ogrn)
                             (:fld bank-name)
                             (:fld bik)
                             (:fld corresp-account)
                             (:fld client-account)
                             (:fld addresses)
                             (:fld contact-person)
                             (:fld contact-phone)
                             (:fld contact-email)
                             (:btn               "Сохранить"
                              :perm              '(or :admin :self)
                              :act (let ((obj (gethash (cur-id) *USER*)))
                                     (with-obj-save obj
                                       NAME JURIDICAL-ADDRESS ACTUAL-ADDRESS CONTACTS EMAIL SITE HEADS INN KPP OGRN BANK-NAME
                                       BIK CORRESP-ACCOUNT CLIENT-ACCOUNT ADDRESSES CONTACT-PERSON contact-phone contact-email)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))
                             ;; affiliates
                             (:grid              "Адреса филиалов и магазинов"
                              :perm              :all
                              :entity            supplier-affiliate
                              :val               (cons-inner-objs *supplier-affiliate* (a-affiliates (gethash 5 *user*)))
                              :fields            '((:fld address :width 900)))
                             ;; pricelist
                             (:grid              "Прайс-лист"
                              :perm              :all
                              :entity            supplier-resource-price-elt
                              :val               (remove-if-not #'(lambda (x)
                                                                    (equal (a-owner (cdr x)) (gethash (cur-id) *user*)))
                                                  (cons-hash-list *supplier-resource-price-elt*))
                              :fields            '((:fld name  :width 500)
                                                   (:fld unit  :width 150)
                                                   (:fld price :width 150)
                                                   (:btn    "Удалить"
                                                    :perm   '(or :admin :owner)
                                                    :width  100
                                                    :act    (delete-supplier-resource-price-elt))))
                             ;; upload pricelist
                             (:popbtn             "Загрузить прайс-лист"
                              :top                1750
                              :left               280
                              :height             200
                              :width              700
                              :perm               :self
                              :action '(:linear             "Добавление прайс-листа"
                                        :perm               '(or :admin :self)
                                        :entity             supplier-resource-price-elt
                                        :val                :clear
                                        :fields             '(
                                                              (:file file
                                                               :perm :all
                                                               :name "Прайс")
                                                              (:btn "Загрузить"
                                                               :perm :all
                                                               :act
                                                               (progn
                                                                ;; (awhen (car (hunchentoot:post-parameter "FILE"))
                                                                ;;   (loop :for src :in (xls-processor it) :do
                                                                ;;      (let ((obj (push-hash *supplier-resource-price-elt* 'supplier-resource-price-elt
                                                                ;;                   :owner (cur-user)
                                                                ;;                   :name (nth 1 src)
                                                                ;;                   :unit (nth 2 src)
                                                                ;;                   :price (nth 3 src))))
                                                                ;;        (append-link (a-price-elts (cur-user)) obj))))
                                                                 (hunchentoot:redirect (hunchentoot:request-uri*)))))))

                             ;; resources
                             (:grid              "Ресурсы для конкурсов"
                              :perm              :all
                              :entity            supplier-resource
                              :val               (cons-inner-objs *SUPPLIER-RESOURCE* (a-resources (gethash (cur-id) *USER*)))
                              :fields            '((:fld resource :width 800)
                                                   (:btn   "Удалить"
                                                    :perm  :self
                                                    :width 100
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SUPPLIER-RESOURCE*
                                                          (a-resources (gethash (cur-id) *USER*))))))

                             (:popbtn            "Добавить ресурс"
                              :top                2000
                              :left               280
                              :height             400
                              :width              900
                              :perm              '(or :admin :self)
                              :action '(:grid               "Добавление ресурса"
                                        :perm               :all
                                        :entity             resource
                                        :val                (cons-hash-list *RESOURCE*)
                                        :height             250
                                        :fields             '((:fld name :width 700)
                                                              (:btn "Добавить ресурс"
                                                               :width 120
                                                               :perm :all
                                                               :act
                                                               (progn
                                                                 (push-hash *SUPPLIER-RESOURCE* 'SUPPLIER-RESOURCE
                                                                   :owner (gethash (cur-user) *USER*)
                                                                   :resource (gethash
                                                                              (cdr (assoc "res" (form-data) :test #'equal))
                                                                              *RESOURCE*)
                                                                   :price (cdr (assoc "PRICE" (form-data) :test #'equal)))
                                                                 (hunchentoot:redirect (hunchentoot:request-uri*)))))))

                             ;; sales
                             (:grid              "Акции"
                              :perm              :all
                              :entity            sale
                              :val               (cons-inner-objs *SALE* (a-sales (gethash (cur-id) *USER*)))
                              :fields            '((:fld name :width  700)
                                                   (:btn "Страница распродажи"
                                                    :perm :all
                                                    :width 100
                                                    :act (to "/sale/~A"  (caar (form-data))))
                                                   (:btn "Удалить распродажу"
                                                    :perm :owner
                                                    :width 100
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SALE*
                                                          (a-sales (gethash (cur-id) *USER*))))))

                             (:popbtn "Добавить распродажу"
                              :perm :nobody
                              :action '(:linear             "Добавление расподажи"
                                       :perm               :self
                                       :entity             sale
                                       :fields             '((:btn "Добавить распродажу"
                                                              :perm :all
                                                              :act (error "create-sale not implemented")))))

                             ;; offers
                             (:grid              "Список заявок на тендеры"
                              :perm              :logged
                              :entity            offer
                              :val               (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *USER*)))
                              :fields            '((:fld tender :xref "offer" :width 680)
                                                   (:btn "Страница заявки"
                                                    :perm :all
                                                    :width 115
                                                    :act (to "/offer/~A" (caar (form-data))))
                                                   (:btn "Удалить заявку"
                                                    :perm :all
                                                    :width 105
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *OFFER*
                                                          (a-offers (gethash (cur-id) *USER*))))))
                             ))

       (:linear            "Отправить заявку на добросовестность" ;; заявка на статус добросовестного поставщика (изменяет статус поставщика)
        :perm              '(and :self :unfair)
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '((:btn "Отправить заявку на добросовестность"
                              :perm :all
                              :act (progn
                                     (setf (a-status (gethash (cur-id) *USER*)) :request)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))

       (:yamap            "Адрес поставщика"
        :val               (let* ((supp (gethash (cur-id) *USER*))
                                  (name (a-name supp))
                                  (addr (a-actual-address supp))
                                  (affi (a-affiliates supp)))
                             (mapcar #'(lambda (x)
                                         (list name x (geo-coder x)))
                                     (remove-duplicates
                                      (append
                                       (mapcar #'(lambda (x)
                                                   (a-address x))
                                               affi)
                                       (list addr))))))
       ))

    ;; Технологии
    (:place                technologies
     :url                  "/technologies"
     :navpoint             "Технологии"
     :actions
     '((:none              "Технологии")))


    ;; Распродажи
    (:place                sales
     :url                  "/sale"
     :navpoint             "Aкции"
     :actions
     '((:grid              "Акции"
        :perm              :all
        :entity            sale
        :val               (cons-hash-list *SALE*)
        :fields            '((:fld name :xref "sale" :width 900)))))

    ;; Страница распродажи
    (:place                sale
     :url                  "/sale/:id"
     :actions
     '((:linear            "Распродажа"
        :perm              :all
        :entity            sale
        :val               (gethash (cur-id) *SALE*)
        :fields            '((:fld name)
                             (:fld owner)
                             (:fld procent)
                             (:fld price)
                             (:fld notes)
                             (:fld resource)
                             (:btn "Сохранить"
                              :perm :self
                              :act (let ((obj (gethash (cur-id) *SALE*)))
                                     (with-obj-save obj
                                       name price procent notes)))))))

    ;; Список застройщиков
    (:place                builders
     :url                  "/builder"
     :actions
     '((:grid            "Организации-застройщики"
        :perm              :all
        :entity            builder
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'BUILDER)) (cons-hash-list *USER*))
        :fields            '((:fld name :xref "builder" :width 750)
                             (:fld login :width 150)))))

    ;; Страница застройщика
    (:place                builder
     :url                  "/builder/:id"
     :actions
     '((:linear            "Застройщик"
        :perm              :all
        :entity            builder
        :val               (gethash (cur-id) *USER*)
        :fields            '((:fld name)
                             (:fld juridical-address)
                             (:fld inn)
                             (:fld kpp)
                             (:fld ogrn)
                             (:fld bank-name)
                             (:fld bik)
                             (:fld corresp-account)
                             (:fld client-account)
                             (:fld rating)
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *USER*)))
                                     (with-obj-save obj
                                       NAME JURIDICAL-ADDRESS INN KPP OGRN BANK-NAME BIK CORRESP-ACCOUNT CLIENT-ACCOUNT RATING)))
                             ;; tenders
                             (:grid             "Тендеры застройщика"
                              :perm             :all
                              :entity           tender
                              :val              (cons-inner-objs *TENDER* (a-tenders (gethash (cur-id) *USER*)))
                              :fields           '((:fld name :xref "tender" :width 550)
                                                  (:fld status :width 150)
                                                  (:fld all :width 200)))))

       (:linear            "Объявить новый тендер"
        :perm              :self
        :entity            tender
        :val               :clear
        :fields            '((:btn "Объявить тендер"
                              :perm :all
                              :act ;;(format nil "~A" (form-data))
                              (let* ((id     (hash-table-count *TENDER*))
                                     (owner  (cur-user))
                                     (tender (setf (gethash id *TENDER*)
                                                   (mi 'TENDER
                                                       :name      (cdr (ASSOC "NAME" (FORM-DATA) :test #'equal))
                                                       :status    :unactive
                                                       :owner     owner
                                                       :all       (cdr (ASSOC "ALL" (FORM-DATA) :test #'equal))
                                                       :claim     (cdr (ASSOC "CLAIM" (FORM-DATA) :test #'equal))
                                                       :analize   (cdr (ASSOC "ANALIZE" (FORM-DATA) :test #'equal))
                                                       :interview (cdr (ASSOC "INTERVIEW" (FORM-DATA) :test #'equal))
                                                       :result    (cdr (ASSOC "RESULT" (FORM-DATA) :test #'equal))
                                                       ))))
                                ;; Связываем с владельцем
                                (setf (a-tenders owner)
                                      (append (a-tenders owner)
                                              (list tender)))
                                ;; Редирект
                                (hunchentoot:redirect
                                 (format nil "/tender/~A" id)))
                              )))))


    ;; Список тендеров
    (:place                tenders
     :url                  "/tender"
     :navpoint             "Тендеры"
     :actions
     '((:grid              "Тендеры"
        :perm              :all
        :entity            tender
        :val               (cons-hash-list *TENDER*)
        :fields            '((:fld name :xref "tender")
                             (:fld status)
                             (:fld owner)))))

    ;; Страница тендера (поставщик может откликнуться)
    (:place                tender
     :url                  "/tender/:id"
     :actions
     '((:linear            "Тендер"
        :perm              :all
        :entity            tender
        :val               (gethash (cur-id) *TENDER*)
        :fields            '((:fld name)
                             (:fld status)
                             (:fld owner)
                             (:fld all)
                             (:fld claim)
                             (:fld analize)
                             (:fld interview)
                             (:fld result)
                             ;; winner price
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *TENDER*)))
                                     (with-obj-save obj
                                       name active-date all claim analize interview result)))
                             ;; resources
                             (:grid             "Ресурсы тендера"
                              :perm             :all
                              :entity           tender-resource
                              :val              (cons-inner-objs *TENDER-RESOURCE* (a-resources (gethash (cur-id) *TENDER*)))
                              :fields '((:fld resource)
                                        ;; (:calc  "Ед.изм."
                                        ;;  :perm :all
                                        ;;  :width 40
                                        ;;  :func (lambda (x) (a-unit (a-resource x))))
                                        (:fld quantity)
                                        (:fld price)
                                        (:fld delivery)
                                        (:fld basic)
                                        (:btn   "Удалить из тендера"
                                         :perm  :all
                                         :width 130
                                         :act   (let ((etalon (gethash (get-btn-key (caar (last (form-data)))) *TENDER-RESOURCE*)))
                                                  (setf (a-resources (gethash (cur-id) *TENDER*))
                                                        (remove-if #'(lambda (x)
                                                                       (equal x etalon))
                                                                   (a-resources (gethash (cur-id) *TENDER*))))
                                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
                                        (:btn   "Страница ресурса"
                                         :perm :all
                                         :width 130
                                         :act   (to "/tender-resource/~A" (caar (last (form-data)))))))


                             (:popbtn "Добавить ресурс"
                              :perm   :all
                              :action '(:grid              "Выберите ресурсы"
                                       :perm              :all ;;'(and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '((:fld name)
                                                            (:btn      "Добавить к тендеру"
                                                             :perm     :all
                                                             :width    140
                                                             :act      (let* ((key      (get-btn-key (caar (last (form-data)))))
                                                                              (resource (gethash key *RESOURCE*))
                                                                              (tender (gethash (cur-id) *TENDER*)))
                                                                         #| TODO: Надо находить пересечение с ресурсами поставщика, который видит это |#
                                                                         (multiple-value-bind (elt id)
                                                                             (push-hash *TENDER-RESOURCE* 'TENDER-RESOURCE
                                                                               :tender   tender
                                                                               :resource resource)
                                                                           (append-link (a-resources tender) elt)
                                                                           (hunchentoot:redirect (format nil "/tender-resource/~A" id))))))))

                             ;; documents
                             (:grid             "Документы тендера"
                              :perm             :all
                              :entity           document
                              :val              (cons-inner-objs *DOCUMENT* (a-documents (gethash (cur-id) *TENDER*)))
                              :fields '((:fld name)
                                        (:btn   "Удалить из тендера"
                                         :perm  :all
                                         :act   (let* ((key       (get-btn-key (caar (last (form-data)))))
                                                       (document  (gethash key *DOCUMENT*))
                                                       (tender    (a-tender document)))
                                                  ;; Удаляем этот документ из тендера
                                                  (setf (a-documents tender)
                                                        (remove-if #'(lambda (x)
                                                                       (equal x document))
                                                                   (a-documents tender)))
                                                  ;; Удаляем документ из документов
                                                  (remhash key *DOCUMENT*)
                                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
                                        (:btn   "Страница документа"
                                         :perm  :all
                                         :act   (to "/document/~A" (caar (last (form-data)))))))
                             (:popbtn "Добавить документ"
                              :perm   :all
                              :action '(:linear            "Загрузка документа"
                                       :perm              :all ;; '(and :active :fair)
                                       :entity            document
                                       :val               :clear
                                       :fields            '((:btn   "Загрузит документ (пока не активно)"
                                                             :perm  :all
                                                             :act   (upload-document)))))

                             ;; suppliers
                             (:grid             "Поставщики ресурсов"
                              :perm             :all
                              :entity           supplier
                              :val              (let ((tender-resources   (mapcar #'a-resource (a-resources (gethash (cur-id) *TENDER*))))
                                                      (all-suppliers      (remove-if-not #'(lambda (x)
                                                                                             (equal (type-of (cdr x)) 'SUPPLIER))
                                                                                         (cons-hash-list *USER*)))
                                                      (supplier-resources (mapcar #'(lambda (x)
                                                                                      (cons (a-resource (cdr x)) (a-owner (cdr x))))
                                                                                  (cons-hash-list *SUPPLIER-RESOURCE*)))
                                                      (result)
                                                      (rs))
                                                  (loop :for tr :in tender-resources :do
                                                     (loop :for sr :in supplier-resources :do
                                                        (when (equal tr (car sr))
                                                          (push (cdr sr) result))))
                                                  (setf result (remove-duplicates result))
                                                  (loop :for rd :in result :do
                                                     (loop :for as :in all-suppliers :do
                                                        (if (equal rd (cdr as))
                                                            (push as rs))))
                                                  rs)
                              :fields           '((:fld name)
                                                  (:fld email)
                                                  (:fld inn)
                                                  (:btn "Отправить приглашение"
                                                   :perm :all
                                                   :width 145
                                                   :act (send-offer-to-supplier-from-tender))
                                                  (:btn "Страница поставщика"
                                                   :perm :all
                                                   :width 145
                                                   :act (to "/supplier/~A"  (caar (last (form-data)))))))
                             (:btn "Добавить своего поставщика"
                              :perm :nobody
                              :act (add-supplier-to-tender))
                             ;; oferts
                             (:grid             "Заявки на тендер"
                              :perm             :all
                              :entity           offer
                              :val              (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *TENDER*)))
                              :fields '((:fld owner)
                                        (:fld status)
                                        (:btn "Просмотр заявки"
                                         :perm :all
                                         :width 140
                                         :act (to "/offer/~A"  (caar (last (form-data)))))))
                             ;; create offer
                             (:popbtn "Ответить заявкой на тендер"
                              :perm   :supplier
                              :action '(:linear            "Вы хотите участвовать в этом тендере?"
                                       :perm              :all ;; '(and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '((:btn  "Да, хочу!"
                                                             :perm :all
                                                             :act  (multiple-value-bind (offer id)
                                                                       (push-hash *OFFER* 'OFFER
                                                                         :owner (cur-user)
                                                                         :tender (gethash (cur-id) *TENDER*)
                                                                         :status :open)
                                                                     (append-link (a-offers (gethash (cur-id) *TENDER*)) offer)
                                                                     (hunchentoot:redirect (format nil "/offer/~A" id)))))))

                             (:popbtn "Отменить тендер"
                              :perm   :all ;;:owner
                              :action '(:linear            "Действительно отменить?"
                                       :perm               :all ;;:owner
                                       :entity             tender
                                       :fields             '((:btn "Подтверждаю отмену"
                                                              :perm :all
                                                              :act  (progn
                                                                      (setf (a-status (gethash (cur-id) *TENDER*)) :cancelled)
                                                                      (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                             ))))


    ;; Ресурс тендера
    (:place                tender-resource
     :url                  "/tender-resource/:id"
     :actions
     '((:linear            "Ресурс тендера"
        :perm              :all
        :entity            tender-resource
        :val               (gethash (cur-id) *TENDER-RESOURCE*)
        :fields            '((:fld tender)
                             (:fld resource)
                             (:fld quantity)
                             (:fld price)
                             (:fld price-date)
                             (:fld comment)
                             (:fld delivery)
                             (:fld basic)
                             (:btn "Сохранить"
                              :perm :all
                              :act  (let ((obj (gethash (cur-id) *TENDER-RESOURCE*)))
                                      (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
                                      (setf (a-basic obj) (not (null (cdr (assoc "BASIC" (form-data) :test #'equal)))))
                                      (with-obj-save obj
                                        quantity price price-date comment)))))
       (:linear            "Вернуться к тендеру"
        :perm              :all
        :entity            tender-resource
        :val               (gethash (cur-id) *TENDER-RESOURCE*)
        :fields            '((:btn "Вернутся к тендеру"
                              :perm :all
                              :act (let* ((tender    (a-tender (gethash (cur-id) *TENDER-RESOURCE*)))
                                          (tender-id (caar (remove-if-not #'(lambda (x) (equal tender (cdr x))) (cons-hash-list *TENDER*)))))
                                     (hunchentoot:redirect (format nil "/tender/~A" tender-id))))))))


    ;; Заявки на тендер
    (:place                offers
     :url                  "/offers"
     ;; :navpoint             "Заявки на участие в тендере"
     :actions
     '((:grid              "Заявки на участие в тендере"
        :perm              :all
        :entity            offer
        :val               (cons-hash-list *OFFER*)
        :fields            '((:fld owner)
                             (:fld tender)
                             (:btn "Страница заявки"
                              :perm :all
                              :width 120
                              :act (to "/offer/~A" (caar (form-data))))
                             (:btn "Страница тендера"
                              :perm :all
                              :width 120
                              :act (HUNCHENTOOT:REDIRECT
                                    (FORMAT NIL "/tender/~A"
                                             (CAAR
                                               (CONS-INNER-OBJS *TENDER*
                                                                (LIST
                                                                 (A-TENDER
                                                                  (GETHASH (GET-BTN-KEY (CAAR (FORM-DATA)))
                                                                           *OFFER*))))))))))))

    ;; Страница заявки на тендер
    (:place                offer
     :url                  "/offer/:id"
     :actions
     '((:linear            "Заявка на тендер"
        :entity            offer
        :perm              :all
        :val               (gethash (cur-id) *OFFER*)
        :fields            '((:fld owner)
                             (:fld tender)
                             (:fld status)
                             ;; resources
                             (:grid             "Ресурсы заявки"
                              :perm             :all
                              :entity           offer-resource
                              ;; (mapcar #'(lambda (x)
                              ;;             (cons (car x) (a-resources (cdr x))))
                              ;;  (cons-hash-list *OFFER*))
                              :val              (cons-inner-objs *OFFER-RESOURCE* (a-resources (gethash (cur-id) *OFFER*)))
                              :fields '((:fld tender-resource)
                                        (:fld quantity)
                                        (:fld price)
                                        (:fld price-result)
                                        #| comment delivery delivery-price market rank |#
                                        (:btn "Удалить из заявки"
                                         :perm :owner
                                         :width 110
                                         :act (del-inner-obj
                                                 (caar (last (form-data)))
                                                 *OFFER-RESOURCE*
                                                 (a-resources (gethash (cur-id) *OFFER*))))
                                        (:btn   "Ресурс заявки"
                                         :width 150
                                         :perm  :all
                                         :act   (to "/offer-resource/~A" (caar (last (form-data)))))
                                        ))
                             (:popbtn "Добавить ресурс к заявке"
                              :perm :owner
                              :action '(:grid              "Выберите ресурсы"
                                       :perm              :all ;; '(and :active :fair)
                                       :entity            tender-resource
                                       :val               (cons-inner-objs *TENDER-RESOURCE* (a-resources (a-tender (gethash (cur-id) *OFFER*))))
                                       :fields            '((:fld resource)
                                                            (:btn  "Добавить к заявке"
                                                             :perm :all
                                                             :act  (let* ((key             (get-btn-key (caar (last (form-data)))))
                                                                          (tender-resource (gethash key *TENDER-RESOURCE*))
                                                                          (offer           (gethash (cur-id) *OFFER*)))
                                                                     (multiple-value-bind (offer-resource id)
                                                                         (push-hash *OFFER-RESOURCE* 'OFFER-RESOURCE
                                                                           :offer offer
                                                                           :tender-resource tender-resource
                                                                           :quantity 0
                                                                           :price 0
                                                                           :price-result 0
                                                                           :comment ""
                                                                           :delivery nil
                                                                           :delivery-price 0
                                                                           :marked nil
                                                                           :rank 0)
                                                                       (append-link (a-resources offer) offer-resource)
                                                                       (hunchentoot:redirect (format nil "/offer-resource/~A" id))))
                                                             )
                                                            )))))))

    ;; Страница ресурса заявки
    (:place                offer-resource
     :url                  "/offer-resource/:id"
     :actions
     '((:linear            "Ресурс заявки"
        :perm              :all
        :entity            offer-resource
        :val               (gethash (cur-id) *OFFER-RESOURCE*)
        :fields            '(#|offer|#
                             (:fld tender-resource)
                             (:fld quantity)
                             (:fld price)
                             (:fld price-result)
                             (:fld comment)
                             (:fld delivery)
                             (:fld delivery-price)
                             #|marked rank|#
                             (:btn "Сохранить"
                              :perm :all
                              :act  (let ((obj (gethash (cur-id) *OFFER-RESOURCE*)))
                                      (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
                                      (setf (a-marked obj) (not (null (cdr (assoc "MARKED" (form-data) :test #'equal)))))
                                      (with-obj-save obj
                                        quantity price price-result comment delivery delivery-price rank)))))
       (:linear            "Вернуться к заявке"
        :perm              :all
        :entity            offer-resource
        :val               :clear
        :fields            '((:btn "Вернутся к заявке"
                              :perm :all
                              :act (let* ((offer    (a-offer (gethash (cur-id) *OFFER-RESOURCE*)))
                                          (offer-id (caar (remove-if-not #'(lambda (x) (equal offer (cdr x))) (cons-hash-list *OFFER*)))))
                                     (hunchentoot:redirect (format nil "/offer/~A" offer-id))))))))



    ;; Календарь событий
    (:place                calendar
     :url                  "/calender"
     :navpoint             "Календарь событий"
     :actions
     '((:none              "Календарь событий")))

    ;; О портале
    (:place                about
     :url                  "/about"
     :actions
     '((:tpl               "О портале"
        :val               (funcall (find-symbol "ABOUT" 'tpl)))))

    ;; Услуги портала
    (:place                services
     :url                  "/services"
     :navpoint             "Услуги портала"
     :actions
     '((:tpl               "Услуги портала"
        :val               (funcall (find-symbol "SERVICES" 'tpl)))))

    ;; Контакты
    (:place                contacts
     :url                  "/contacts"
     :navpoint             "Контакты"
     :actions
     '((:tpl               "Контакты"
        :val               (funcall (find-symbol "CONTACTS" 'tpl)))))
    ))
