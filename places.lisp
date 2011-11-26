(in-package #:wizard)

(defmacro def-fld (name &key (update nil update-p) (show nil show-p) (view nil view-p) (xref nil xref-p)  (width 200 width-p))
  (let ((rs `(:fld ,name)))
    (if update-p  (nconc rs `(:update ,update)))
    (if view-p    (nconc rs `(:view   ,view)))
    (if show-p    (nconc rs `(:show   ,show)))
    (if xref-p    (nconc rs `(:xref   ,xref)))
    (if width-p   (nconc rs `(:width  ,width)))
    `',rs))

;; (macroexpand-1 '
;;  (def-fld name2 :width 550 :update :all))


(defmacro def-btn ((title perm &key (width 200 width-p)) &body act)
  (let ((rs `(:btn ,title :perm ,perm)))
    (if width-p (nconc rs `(:width ,width)))
    (nconc rs `(:act ,@act))
    `',rs))

(defmacro def-pop ((title perm &key (top 100 top-p)  (left 100 left-p)  (height 100 height-p)  (width 200 width-p)) &body action)
  (let ((rs `(:popbtn ,title)))
    (if top-p    (nconc rs `(:top    ,top)))
    (if left-p   (nconc rs `(:left   ,left)))
    (if height-p (nconc rs `(:height ,height)))
    (if width-p  (nconc rs `(:width  ,width)))
    (nconc rs `(:action ,@action))
    `',rs))

;; (macroexpand-1 '
;; (def-pop (name2 :all :width 550)
;;   'aaa))



(defparameter *places*
  `(
    ;; Главная страница
    (:place         main
     :url           "/"
     :navpoint      "Главная"
     :actions
     '((:tpl        ""
        :val        (funcall (find-symbol "MAIN" 'tpl)
                     (list :postblocks (list (list* :xrefall "/posts"
                                                    :titleall "все новости"
                                                    :title "Новости строительной сферы"
                                                    :posts (posts-by-section "news" 3))
                                             (list* :xrefall "/technologies"
                                                    :titleall "все новости"
                                                    :title "Новые технологии"
                                                    :posts (posts-by-section "techno" 3))
                                             (list* :xrefall "/event"
                                                    :titleall "все новости"
                                                    :title "Календарь событий"
                                                    :posts (posts-by-section "ivent" 3))
                                             (list* :xrefall "/posts"
                                                    :titleall "все новости"
                                                    :title "Акции, скидки и предложения"
                                                    :posts (posts-by-sales 3))))))))

    ;; Страница регистрации
    (:place         register
     :url           "/register"
     :actions
     '((:linear     "Регистрация"
        :entity     supplier
        :perm       :all
        :val        :clear
        :fields     '(,(def-fld  login             :update '(or :nobody :all))
                      ,(def-fld  password          :update :all)
                      ,(def-fld  email             :update :all)
                      ,(def-fld  name              :update :all)
                      ,(def-fld  inn               :update :all)
                      ,(def-fld  ogrn              :update :all)
                      ,(def-fld  juridical-address :update :all)
                      ,(def-fld  actual-address    :update :all)
                      ,(def-fld  contact-person    :update :all)
                      ,(def-fld  contact-phone     :update :all)
                      ,(def-btn  ("Зарегистрироваться" :all :width 120)
                                 (with-obj-create (*USER* 'SUPPLIER (login password email name inn ogrn juridical-address actual-address
                                                                          contact-person contact-phone))
                                   (setf (a-status obj) :unfair)
                                   (hunchentoot:redirect (format nil "/supplier/~A" id))))))))

    ;; Новости
    (:place         posts
     :url           "/posts"
     :navpoint      "Новости"
     :actions
     '((:tpl        "Новости"
        :val        (funcall (find-symbol "POSTPAGE" 'tpl)
                     (list :postblocks (list (list* :xrefall "/posts"
                                                    :titleall "все новости"
                                                    :title "Новости строительной сферы"
                                                    :posts (posts-by-section "news" 3))
                                             (list* :xrefall "/technologies"
                                                    :titleall "все новости"
                                                    :title "Новые технологии"
                                                    :posts (posts-by-section "techno" 3))))))))

    ;; Новость
    (:place         post
     :url           "/post/:id"
     :actions
     '((:post       "%|title|%"
        :entity     post-item
        :val        (gethash (cur-page-id) *POST-ITEM*)
        :fields     '(,(def-fld title)
                      ,(def-fld date)
                      ,(def-fld text-photo)
                      ,(def-fld text)))))

    ;; Аналитика
    (:place         anal
     :url           "/analytics"
     :navpoint      "Аналитика"
     :actions
     '((:none       "Аналитика")))

    ;; Каталог материалов
    (:place         material
     :url           "/material"
     :navpoint      "Каталог ресурсов"
     :actions
     '((:grid       "Группы"
        :perm       :all
        :entity     category
        :val        (cons-inner-objs *CATEGORY*
                     (a-child-categoryes
                      (cdr (car (remove-if-not #'(lambda (x)
                                                   (null (a-parent (cdr x))))
                                               (cons-hash-list *CATEGORY*))))))
        :height     400
        :fields     '(,(def-fld name :xref "category" :width 900)))))


    ;; ;; Каталог материалов
    (:place         machine
     :url           "/machine"
     :navpoint      "Строительная техника"
     :actions
     '((:grid       "Группы"
        :perm       :all
        :entity     category
        :val        (cons-inner-objs *CATEGORY*
                     (a-child-categoryes
                      (cdr (cadr (remove-if-not #'(lambda (x)
                                                    (null (a-parent (cdr x))))
                                                (cons-hash-list *CATEGORY*))))))
        :height     400
        :fields     '(,(def-fld name :xref "category" :width 900)))))


    ;; Каталог ресурсов - содержимое категории
    (:place         category
     :url           "/category/:id"
     :actions
     '((:linear     "Группа"
        :perm       :all
        :entity     category
        :val        :clear
        :fields     '((:grid              "Подгруппы"
                       :perm              :all
                       :entity            category
                       :val               (cons-inner-objs *CATEGORY* (a-child-categoryes (gethash (cur-page-id) *CATEGORY*)))
                       :fields            '(,(def-fld name :xref "category" :width 900)))
                      (:grid              "Ресурсы группы"
                       :perm              :all
                       :entity            resource
                       :val               (remove-if-not #'(lambda (x)
                                                             (equal (a-category (cdr x))
                                                                    (gethash (cur-page-id) *CATEGORY*)))
                                           (cons-hash-list *RESOURCE*))
                       :fields            '(,(def-fld name :xref "resource" :width 900)))))))

    ;; Страница ресурса
    (:place                resource
     :url                  "/resource/:id"
     :actions
     '((:linear            "Ресурс"
        :perm              :all
        :entity            resource
        :val               (gethash (cur-page-id) *RESOURCE*)
        :fields            '(,(def-fld name)
                             ,(def-fld category)
                             ,(def-fld resource-type)
                             ,(def-fld unit)))))

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
    ;;                                    PASSWORD)
    ;;                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
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
    ;;                                                                   PASSWORD)
    ;; (hunchentoot:redirect (hunchentoot:request-uri*)))))))))
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
    ;;     :val               (gethash (cur-page-id) *USER*)
    ;;     :fields            '((:fld name)
    ;;                          (:fld login)))))

    ;; Список поставщиков
    (:place         suppliers
     :url           "/supplier"
     :navpoint      "Поставщики"
     :actions
     '((:grid       "Каталог поставщиков"
        :perm       :all
        :entity     supplier
        :val        (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'SUPPLIER))  (cons-hash-list *USER*))
        :fields     '(,(def-fld name :xref "supplier" :width 300)
                      ,(def-fld actual-address :width 600)))))

    ;; Страница поставщика
    (:place         supplier
     :url           "/supplier/:id"
     :actions
     '((:linear     "Поставщик"
        :perm       :all
        :entity     supplier
        :val        (gethash (cur-page-id) *USER*)
        :fields     '(,(def-fld  name)
                      ,(def-fld  status)
                      ,(def-fld  juridical-address)
                      ,(def-fld  actual-address)
                      ,(def-fld  contacts)
                      ,(def-fld  email)
                      ,(def-fld  site)
                      ,(def-fld  heads)
                      ,(def-fld  inn)
                      ,(def-fld  kpp)
                      ,(def-fld  ogrn)
                      ,(def-fld  bank-name)
                      ,(def-fld  bik)
                      ,(def-fld  corresp-account)
                      ,(def-fld  client-account)
                      ,(def-fld  addresses)
                      ,(def-fld  contact-person)
                      ,(def-fld  contact-phone)
                      ,(def-fld  contact-email)
                      ,(def-btn  ("Сохранить" :self)
                                 (let ((obj (gethash (cur-page-id) *USER*)))
                                   (with-obj-save obj
                                     NAME JURIDICAL-ADDRESS ACTUAL-ADDRESS CONTACTS EMAIL SITE HEADS INN KPP OGRN BANK-NAME
                                     BIK CORRESP-ACCOUNT CLIENT-ACCOUNT ADDRESSES CONTACT-PERSON contact-phone contact-email)
                                   (hunchentoot:redirect (hunchentoot:request-uri*))))
                      ,(def-btn  ("Отправить заявку на добросовестность" '(and :self :unfair))
                                 (progn
                                   (setf (a-status (gethash (cur-page-id) *USER*)) :request)
                                   (hunchentoot:redirect (hunchentoot:request-uri*))))
                      ;; affiliates
                      (:grid     "Адреса филиалов и магазинов"
                       :perm     :all
                       :entity   supplier-affiliate
                       :val      (cons-inner-objs *supplier-affiliate* (a-affiliates (gethash (cur-page-id) *user*)))
                       :fields   '(,(def-fld address :width 900)))
                      ;; pricelis
                      (:grid     "Прайс-лист"
                       :perm     :all
                       :entity   supplier-resource-price-elt
                       :val      (remove-if-not #'(lambda (x)
                                                    (equal (a-owner (cdr x)) (gethash (cur-page-id) *user*)))
                                  (cons-hash-list *supplier-resource-price-elt*))
                       :fields   '(,(def-fld  name  :width 350)
                                   ,(def-fld  unit  :width 150)
                                   ,(def-fld  price :width 150)
                                   ,(def-fld  date  :width 150)
                                   ,(def-btn  ("Удалить" :owner :width  100)
                                      (let* ((key (get-btn-key (caar (form-data))))
                                             (hobj (gethash key *supplier-resource-price-elt*)))
                                        (setf (a-price-elts (cur-user))
                                              (remove-if #'(lambda (x) (equal x hobj))
                                                         (a-price-elts (cur-user))))
                                        (remhash key *supplier-resource-price-elt*)
                                        (hunchentoot:redirect (hunchentoot:request-uri*))))))
                      ;; upload pricelist
                      (:popbtn  "Загрузить прайс-лист"
                       :top      1750
                       :left     280
                       :height   200
                       :width    700
                       :perm     :self
                       :action '(:linear     "Добавление прайс-листа"
                                 :perm       :self
                                 :entity     supplier-resource-price-elt
                                 :val        :clear
                                 :fields     '((:file file :perm :all :name "Прайс")
                                               ,(def-btn ("Загрузить" :all)
                                                  (progn
                                                    (awhen (car (hunchentoot:post-parameter "FILE"))
                                                      (loop :for src :in (xls-processor it) :do
                                                         (add-inner-obj *supplier-resource-price-elt* 'supplier-resource-price-elt
                                                             (a-price-elts (cur-user))
                                                           :owner (cur-user)
                                                           :name  (nth 1 src)
                                                           :unit  (nth 2 src)
                                                           :price (nth 3 src)
                                                           :date  (decode-date (get-universal-time)))))
                                                    (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                      ;; resources
                      (:grid     "Ресурсы для конкурсов"
                       :perm     :all
                       :entity   supplier-resource
                       :val      (cons-inner-objs *SUPPLIER-RESOURCE* (a-resources (gethash (cur-page-id) *USER*)))
                       :fields   '(,(def-fld resource :width 800)
                                   ,(def-btn ("Удалить" :owner :width 100)
                                      (del-inner-obj
                                       (caar (form-data))
                                       *SUPPLIER-RESOURCE*
                                       (a-resources (gethash (cur-page-id) *USER*))))))
                      ;; ;; Добавление ресурса
                      (:popbtn  "Добавить ресурс"
                       :top      2000
                       :left     280
                       :height   400
                       :width    900
                       :perm     :self
                       :action '(:grid      "Добавление ресурса"
                                 :perm       :all
                                 :entity     resource
                                 :val        (cons-hash-list *RESOURCE*)
                                 :height     240
                                 :fields     '(,(def-fld name :width 700)
                                               ,(def-btn ("Добавить ресурс" :all :width 120)
                                                  (let* ((owner    (cur-user))
                                                         (resource (gethash (get-btn-key (caar (form-data))) *RESOURCE*)))
                                                    (add-inner-obj *SUPPLIER-RESOURCE* 'SUPPLIER-RESOURCE (a-resources owner)
                                                      :owner     owner
                                                      :resource  resource
                                                      :price     0)
                                                    (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                      ;; sales
                      (:grid    "Акции"
                       :perm     :all
                       :entity   sale
                       :val      (cons-inner-objs *SALE* (a-sales (gethash (cur-page-id) *USER*)))
                       :fields  '(,(def-fld title :width 800 :xref "sale")
                                  ,(def-btn ("Удалить" :owner :width 100)
                                             (del-inner-obj
                                              (caar (form-data))
                                              *SALE*
                                              (a-sales (gethash (cur-page-id) *USER*))))))
                      ;; Добавление акции
                      (:popbtn  "Добавить акцию"
                       :top      2200
                       :left     280
                       :height   400
                       :width    900
                       :perm     :all
                       :action  '(:linear  "Добавление акции"
                                  :perm     :all
                                  :entity   sale
                                  :val      :clear
                                  :fields   '(,(def-fld title)
                                              ;; (:fld date)
                                              ;; (:fld announce-photo)
                                              ;; (:fld announce)
                                              ;; (:fld text-photo)
                                              ;; (:fld text)
                                              ;; (:fld owner)
                                              ;; (:fld resource)
                                              ,(def-btn ("Добавить акцию" :all)
                                                  (let* ((owner (cur-user)))
                                                    (add-inner-obj *SALE* 'SALE (a-sales owner)
                                                      :owner     owner
                                                      :title     (form-fld title))
                                                    (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                      ;; offers
                      (:grid     "Список заявок на тендеры"
                       :perm     :logged
                       :entity   offer
                       :val      (cons-inner-objs *OFFER* (a-offers (gethash (cur-page-id) *USER*)))
                       :fields   '(,(def-fld tender :xref "offer" :width 680)
                                   ,(def-btn ("Страница заявки" :all :width 115)
                                       (to "/offer/~A" (caar (form-data))))
                                   ,(def-btn ("Удалить заявку" :all :width 105)
                                       (del-inner-obj
                                        (caar (form-data))
                                        *OFFER*
                                        (a-offers (gethash (cur-page-id) *USER*))))))
                      ))
       (:yamap  "Адрес поставщика"
        :val     (let* ((supp (gethash (cur-page-id) *USER*))
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

    ;; ;; Распродажи
    ;; (:place                sales
    ;;  :url                  "/sale"
    ;;  :actions
    ;;  '((:tpl               "Акции"
    ;;     :val               (funcall (find-symbol "POSTPAGE" 'tpl)
    ;;                         (list :postblocks (list (list* :xrefall "/event"
    ;;                                                        :titleall ""
    ;;                                                        :title ""
    ;;                                                        :posts (posts-by-sales 100))
    ;;                                                 )))
    ;;     )))

    ;; ;; Страница распродажи
    ;; (:place                sale
    ;;  :url                  "/sale/:id"
    ;;  :actions
    ;;  '((:post              "%|title|%"
    ;;     :entity            sale
    ;;     :val               (gethash (cur-page-id) *SALE*)
    ;;     :fields            '((:fld title)
    ;;                           (:fld date)
    ;;                           (:fld text-photo)
    ;;                           (:fld text)))))

    ;; ;; Список застройщиков
    ;; (:place                builders
    ;;  :url                  "/builder"
    ;;  :actions
    ;;  '((:grid            "Организации-застройщики"
    ;;     :perm              :all
    ;;     :entity            builder
    ;;     :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'BUILDER)) (cons-hash-list *USER*))
    ;;     :fields            '((:fld name :xref "builder" :width 750)
    ;;                          (:fld login :width 150)))))

    ;; ;; ;; Страница застройщика
    ;; ;; (:place                builder
    ;; ;;  :url                  "/builder/:id"
    ;; ;;  :actions
    ;; ;;  '((:linear            "Застройщик"
    ;; ;;     :perm              :all
    ;; ;;     :entity            builder
    ;; ;;     :val               (gethash (cur-page-id) *USER*)
    ;; ;;     :fields            '(,(def-fld  name)
    ;; ;;                          ,(def-fld  juridical-address)
    ;; ;;                          ,(def-fld  inn)
    ;; ;;                          ,(def-fld  kpp)
    ;; ;;                          ,(def-fld  ogrn)
    ;; ;;                          ,(def-fld  bank-name)
    ;; ;;                          ,(def-fld  bik)
    ;; ;;                          ,(def-fld  corresp-account)
    ;; ;;                          ,(def-fld  client-account)
    ;; ;;                          ,(def-fld  rating)
    ;; ;;                          ,(def-btn  "Сохранить" :all
    ;; ;;                                     (progn
    ;; ;;                                       (with-obj-save (gethash (cur-page-id) *USER*)
    ;; ;;                                         NAME JURIDICAL-ADDRESS INN KPP OGRN BANK-NAME BIK CORRESP-ACCOUNT CLIENT-ACCOUNT RATING)
    ;; ;;                                       (hunchentoot:redirect (hunchentoot:request-uri*))))
    ;; ;;                          ;; tenders
    ;; ;;                          (:grid             "Тендеры застройщика"
    ;; ;;                           :perm             :all
    ;; ;;                           :entity           tender
    ;; ;;                           :val              (cons-inner-objs *TENDER* (a-tenders (gethash (cur-page-id) *USER*)))
    ;; ;;                           :fields           '(,(def-fld name :xref "tender" :width 550)
    ;; ;;                                               (:fld status :width 150)
    ;; ;;                                               (:fld all :width 200)))))

    ;;    (:linear            "Объявить новый тендер"
    ;;     :perm              :self
    ;;     :entity            tender
    ;;     :val               :clear
    ;;     :fields            '((:btn "Объявить тендер"
    ;;                           :perm :all
    ;;                           :act ;;(format nil "~A" (form-data))
    ;;                           (let* ((id     (hash-table-count *TENDER*))
    ;;                                  (owner  (cur-user))
    ;;                                  (tender (setf (gethash id *TENDER*)
    ;;                                                (mi 'TENDER
    ;;                                                    :name      (cdr (ASSOC "NAME" (FORM-DATA) :test #'equal))
    ;;                                                    :status    :unactive
    ;;                                                    :owner     owner
    ;;                                                    :all       (cdr (ASSOC "ALL" (FORM-DATA) :test #'equal))
    ;;                                                    :claim     (cdr (ASSOC "CLAIM" (FORM-DATA) :test #'equal))
    ;;                                                    :analize   (cdr (ASSOC "ANALIZE" (FORM-DATA) :test #'equal))
    ;;                                                    :interview (cdr (ASSOC "INTERVIEW" (FORM-DATA) :test #'equal))
    ;;                                                    :result    (cdr (ASSOC "RESULT" (FORM-DATA) :test #'equal))
    ;;                                                    ))))
    ;;                             ;; Связываем с владельцем
    ;;                             (setf (a-tenders owner)
    ;;                                   (append (a-tenders owner)
    ;;                                           (list tender)))
    ;;                             ;; Редирект
    ;;                             (hunchentoot:redirect
    ;;                              (format nil "/tender/~A" id)))
    ;;                           )))))


    ;; ;; Список тендеров
    ;; (:place                tenders
    ;;  :url                  "/tender"
    ;;  :navpoint             "Тендеры"
    ;;  :actions
    ;;  '((:grid              "Тендеры"
    ;;     :perm              :all
    ;;     :entity            tender
    ;;     :val               (cons-hash-list *TENDER*)
    ;;     :fields            '((:fld name :xref "tender")
    ;;                          (:fld status)
    ;;                          (:fld owner)))))

    ;; ;; Страница тендера (поставщик может откликнуться)
    ;; (:place                tender
    ;;  :url                  "/tender/:id"
    ;;  :actions
    ;;  '((:linear            "Тендер"
    ;;     :perm              :all
    ;;     :entity            tender
    ;;     :val               (gethash (cur-page-id) *TENDER*)
    ;;     :fields            '((:fld name)
    ;;                          (:fld status)
    ;;                          (:fld owner)
    ;;                          (:fld all)
    ;;                          (:fld claim)
    ;;                          (:fld analize)
    ;;                          (:fld interview)
    ;;                          (:fld result)
    ;;                          ;; winner price
    ;;                          (:btn "Сохранить"
    ;;                           :perm :owner
    ;;                           :act (let ((obj (gethash (cur-page-id) *TENDER*)))
    ;;                                  (with-obj-save obj
    ;;                                    name all claim analize interview result)
    ;;                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
    ;;                          ;; resources
    ;;                          (:grid             "Ресурсы тендера"
    ;;                           :perm             :all
    ;;                           :entity           tender-resource
    ;;                           :val              (cons-inner-objs *TENDER-RESOURCE* (a-resources (gethash (cur-page-id) *TENDER*)))
    ;;                           :fields '((:fld resource :xref "tender-resource" :width 400)
    ;;                                     ;; (:calc  "Ед.изм."
    ;;                                     ;;  :perm :all
    ;;                                     ;;  :width 40
    ;;                                     ;;  :func (lambda (x) (a-unit (a-resource x))))
    ;;                                     (:fld quantity :width 80)
    ;;                                     (:fld price :width 80)
    ;;                                     (:fld delivery :width 100)
    ;;                                     (:fld basic :width 100)
    ;;                                     (:btn   "Удалить из тендера"
    ;;                                      :perm  :owner
    ;;                                      :width 125
    ;;                                      :act   (let ((etalon (gethash (get-btn-key (caar (last (form-data)))) *TENDER-RESOURCE*)))
    ;;                                               (setf (a-resources (gethash (cur-page-id) *TENDER*))
    ;;                                                     (remove-if #'(lambda (x)
    ;;                                                                    (equal x etalon))
    ;;                                                                (a-resources (gethash (cur-page-id) *TENDER*))))
    ;;                                               (hunchentoot:redirect (hunchentoot:request-uri*))))))

    ;;                          (:popbtn  "Добавить ресурс"
    ;;                           :top     400
    ;;                           :left    280
    ;;                           :height  480
    ;;                           :width   800
    ;;                           :perm    :owner
    ;;                           :action  '(:grid              "Выберите ресурсы"
    ;;                                      :perm              :all ;;'(and :active :fair)
    ;;                                      :height            240
    ;;                                      :entity            resource
    ;;                                      :val               (cons-hash-list *RESOURCE*)
    ;;                                      :fields            '((:fld name :xref "resource" :width 650)
    ;;                                                           (:btn      "Добавить к тендеру"
    ;;                                                            :perm     :all
    ;;                                                            :width    140
    ;;                                                            :act      (let* ((key      (get-btn-key (caar (last (form-data)))))
    ;;                                                                             (resource (gethash key *RESOURCE*))
    ;;                                                                             (tender (gethash (cur-page-id) *TENDER*)))
    ;;                                                                        #| TODO: Надо находить пересечение с ресурсами поставщика, который видит это |#
    ;;                                                                        (multiple-value-bind (elt id)
    ;;                                                                            (push-hash *TENDER-RESOURCE* 'TENDER-RESOURCE
    ;;                                                                              :tender   tender
    ;;                                                                              :resource resource)
    ;;                                                                          (append-link (a-resources tender) elt)
    ;;                                                                          (hunchentoot:redirect (format nil "/tender-resource/~A" id))))))))

    ;;                          ;; documents
    ;;                          (:grid             "Документы тендера"
    ;;                           :perm             :all
    ;;                           :entity           document
    ;;                           :val              (cons-inner-objs *DOCUMENT* (a-documents (gethash (cur-page-id) *TENDER*)))
    ;;                           :fields '((:fld name)
    ;;                                     (:btn   "Удалить из тендера"
    ;;                                      :perm  :all
    ;;                                      :act   (let* ((key       (get-btn-key (caar (last (form-data)))))
    ;;                                                    (document  (gethash key *DOCUMENT*))
    ;;                                                    (tender    (a-tender document)))
    ;;                                               ;; Удаляем этот документ из тендера
    ;;                                               (setf (a-documents tender)
    ;;                                                     (remove-if #'(lambda (x)
    ;;                                                                    (equal x document))
    ;;                                                                (a-documents tender)))
    ;;                                               ;; Удаляем документ из документов
    ;;                                               (remhash key *DOCUMENT*)
    ;;                                               (hunchentoot:redirect (hunchentoot:request-uri*))))
    ;;                                     (:btn   "Страница документа"
    ;;                                      :perm  :all
    ;;                                      :act   (to "/document/~A" (caar (last (form-data)))))))
    ;;                          (:popbtn "Добавить документ"
    ;;                           :perm   :all
    ;;                           :action '(:linear            "Загрузка документа"
    ;;                                     :perm              :all ;; '(and :active :fair)
    ;;                                     :entity            document
    ;;                                     :val               :clear
    ;;                                     :fields            '((:btn   "Загрузить документ (пока не активно)"
    ;;                                                           :perm  :all
    ;;                                                           :act   (upload-document)))))

    ;;                          ;; suppliers
    ;;                          (:grid             "Поставщики ресурсов"
    ;;                           :perm             :all
    ;;                           :entity           supplier
    ;;                           :val              (let ((tender-resources   (mapcar #'a-resource (a-resources (gethash (cur-page-id) *TENDER*))))
    ;;                                                   (all-suppliers      (remove-if-not #'(lambda (x)
    ;;                                                                                          (equal (type-of (cdr x)) 'SUPPLIER))
    ;;                                                                                      (cons-hash-list *USER*)))
    ;;                                                   (supplier-resources (mapcar #'(lambda (x)
    ;;                                                                                   (cons (a-resource (cdr x)) (a-owner (cdr x))))
    ;;                                                                               (cons-hash-list *SUPPLIER-RESOURCE*)))
    ;;                                                   (result)
    ;;                                                   (rs))
    ;;                                               (loop :for tr :in tender-resources :do
    ;;                                                  (loop :for sr :in supplier-resources :do
    ;;                                                     (when (equal tr (car sr))
    ;;                                                       (push (cdr sr) result))))
    ;;                                               (setf result (remove-duplicates result))
    ;;                                               (loop :for rd :in result :do
    ;;                                                  (loop :for as :in all-suppliers :do
    ;;                                                     (if (equal rd (cdr as))
    ;;                                                         (push as rs))))
    ;;                                               rs)
    ;;                           :fields           '((:fld name)
    ;;                                               (:fld email)
    ;;                                               (:fld inn)
    ;;                                               (:btn "Отправить приглашение"
    ;;                                                :perm :all
    ;;                                                :width 145
    ;;                                                :act (send-offer-to-supplier-from-tender))
    ;;                                               (:btn "Страница поставщика"
    ;;                                                :perm :all
    ;;                                                :width 145
    ;;                                                :act (to "/supplier/~A"  (caar (last (form-data)))))))
    ;;                          (:btn "Добавить своего поставщика"
    ;;                           :perm :nobody
    ;;                           :act (add-supplier-to-tender))
    ;;                          ;; oferts
    ;;                          (:grid             "Заявки на тендер"
    ;;                           :perm             :all
    ;;                           :entity           offer
    ;;                           :val              (cons-inner-objs *OFFER* (a-offers (gethash (cur-page-id) *TENDER*)))
    ;;                           :fields '((:fld owner)
    ;;                                     (:fld status)
    ;;                                     (:btn "Просмотр заявки"
    ;;                                      :perm :all
    ;;                                      :width 140
    ;;                                      :act (to "/offer/~A"  (caar (last (form-data)))))))
    ;;                          ;; create offer
    ;;                          (:popbtn "Ответить заявкой на тендер"
    ;;                           :perm   :supplier
    ;;                           :action '(:linear            "Вы хотите участвовать в этом тендере?"
    ;;                                    :perm              :all ;; '(and :active :fair)
    ;;                                    :entity            resource
    ;;                                    :val               (cons-hash-list *RESOURCE*)
    ;;                                    :fields            '((:btn  "Да, хочу!"
    ;;                                                          :perm :all
    ;;                                                          :act  (multiple-value-bind (offer id)
    ;;                                                                    (push-hash *OFFER* 'OFFER
    ;;                                                                      :owner (cur-user)
    ;;                                                                      :tender (gethash (cur-page-id) *TENDER*)
    ;;                                                                      :status :open)
    ;;                                                                  (append-link (a-offers (gethash (cur-page-id) *TENDER*)) offer)
    ;;                                                                  (hunchentoot:redirect (format nil "/offer/~A" id)))))))

    ;;                          (:popbtn "Отменить тендер"
    ;;                           :perm   :all ;;:owner
    ;;                           :action '(:linear            "Действительно отменить?"
    ;;                                    :perm               :all ;;:owner
    ;;                                    :entity             tender
    ;;                                    :fields             '((:btn "Подтверждаю отмену"
    ;;                                                           :perm :all
    ;;                                                           :act  (progn
    ;;                                                                   (setf (a-status (gethash (cur-page-id) *TENDER*)) :cancelled)
    ;;                                                                   (hunchentoot:redirect (hunchentoot:request-uri*)))))))
    ;;                          ))))


    ;; ;; Ресурс тендера
    ;; (:place                tender-resource
    ;;  :url                  "/tender-resource/:id"
    ;;  :actions
    ;;  '((:linear            "Ресурс тендера"
    ;;     :perm              :all
    ;;     :entity            tender-resource
    ;;     :val               (gethash (cur-page-id) *TENDER-RESOURCE*)
    ;;     :fields            '((:fld tender)
    ;;                          (:fld resource)
    ;;                          (:fld quantity)
    ;;                          (:fld price)
    ;;                          (:fld price-date)
    ;;                          (:fld comment)
    ;;                          (:fld delivery)
    ;;                          (:fld basic)
    ;;                          (:btn "Сохранить"
    ;;                           :perm :all
    ;;                           :act  (let ((obj (gethash (cur-page-id) *TENDER-RESOURCE*)))
    ;;                                   (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
    ;;                                   (setf (a-basic obj) (not (null (cdr (assoc "BASIC" (form-data) :test #'equal)))))
    ;;                                   (with-obj-save obj
    ;;                                     quantity price price-date comment)
    ;;                                   (hunchentoot:redirect (hunchentoot:request-uri*))))))
    ;;    (:linear            "Вернуться к тендеру"
    ;;     :perm              :all
    ;;     :entity            tender-resource
    ;;     :val               (gethash (cur-page-id) *TENDER-RESOURCE*)
    ;;     :fields            '((:btn "Вернутся к тендеру"
    ;;                           :perm :all
    ;;                           :act (let* ((tender    (a-tender (gethash (cur-page-id) *TENDER-RESOURCE*)))
    ;;                                       (tender-id (caar (remove-if-not #'(lambda (x) (equal tender (cdr x))) (cons-hash-list *TENDER*)))))
    ;;                                  (hunchentoot:redirect (format nil "/tender/~A" tender-id))))))))


    ;; ;; Заявки на тендер
    ;; (:place                offers
    ;;  :url                  "/offers"
    ;;  ;; :navpoint             "Заявки на участие в тендере"
    ;;  :actions
    ;;  '((:grid              "Заявки на участие в тендере"
    ;;     :perm              :all
    ;;     :entity            offer
    ;;     :val               (cons-hash-list *OFFER*)
    ;;     :fields            '((:fld owner)
    ;;                          (:fld tender)
    ;;                          (:btn "Страница заявки"
    ;;                           :perm :all
    ;;                           :width 120
    ;;                           :act (to "/offer/~A" (caar (form-data))))
    ;;                          (:btn "Страница тендера"
    ;;                           :perm :all
    ;;                           :width 120
    ;;                           :act (HUNCHENTOOT:REDIRECT
    ;;                                 (FORMAT NIL "/tender/~A"
    ;;                                          (CAAR
    ;;                                            (CONS-INNER-OBJS *TENDER*
    ;;                                                             (LIST
    ;;                                                              (A-TENDER
    ;;                                                               (GETHASH (GET-BTN-KEY (CAAR (FORM-DATA)))
    ;;                                                                        *OFFER*))))))))))))

    ;; ;; Страница заявки на тендер
    ;; (:place                offer
    ;;  :url                  "/offer/:id"
    ;;  :actions
    ;;  '((:linear            "Заявка на тендер"
    ;;     :entity            offer
    ;;     :perm              :all
    ;;     :val               (gethash (cur-page-id) *OFFER*)
    ;;     :fields            '((:fld owner)
    ;;                          (:fld tender)
    ;;                          (:fld status)
    ;;                          ;; resources
    ;;                          (:grid             "Ресурсы заявки"
    ;;                           :perm             :all
    ;;                           :entity           offer-resource
    ;;                           ;; (mapcar #'(lambda (x)
    ;;                           ;;             (cons (car x) (a-resources (cdr x))))
    ;;                           ;;  (cons-hash-list *OFFER*))
    ;;                           :val              (cons-inner-objs *OFFER-RESOURCE* (a-resources (gethash (cur-page-id) *OFFER*)))
    ;;                           :fields '((:fld tender-resource)
    ;;                                     (:fld quantity)
    ;;                                     (:fld price)
    ;;                                     (:fld price-result)
    ;;                                     #| comment delivery delivery-price market rank |#
    ;;                                     (:btn "Удалить из заявки"
    ;;                                      :perm :owner
    ;;                                      :width 110
    ;;                                      :act (del-inner-obj
    ;;                                              (caar (last (form-data)))
    ;;                                              *OFFER-RESOURCE*
    ;;                                              (a-resources (gethash (cur-page-id) *OFFER*))))
    ;;                                     (:btn   "Ресурс заявки"
    ;;                                      :width 150
    ;;                                      :perm  :all
    ;;                                      :act   (to "/offer-resource/~A" (caar (last (form-data)))))
    ;;                                     ))
    ;;                          (:popbtn "Добавить ресурс к заявке"
    ;;                           :perm :owner
    ;;                           :action '(:grid              "Выберите ресурсы"
    ;;                                    :perm              :all ;; '(and :active :fair)
    ;;                                    :entity            tender-resource
    ;;                                    :val               (cons-inner-objs *TENDER-RESOURCE* (a-resources (a-tender (gethash (cur-page-id) *OFFER*))))
    ;;                                    :fields            '((:fld resource)
    ;;                                                         (:btn  "Добавить к заявке"
    ;;                                                          :perm :all
    ;;                                                          :act  (let* ((key             (get-btn-key (caar (last (form-data)))))
    ;;                                                                       (tender-resource (gethash key *TENDER-RESOURCE*))
    ;;                                                                       (offer           (gethash (cur-page-id) *OFFER*)))
    ;;                                                                  (multiple-value-bind (offer-resource id)
    ;;                                                                      (push-hash *OFFER-RESOURCE* 'OFFER-RESOURCE
    ;;                                                                        :offer offer
    ;;                                                                        :tender-resource tender-resource
    ;;                                                                        :quantity 0
    ;;                                                                        :price 0
    ;;                                                                        :price-result 0
    ;;                                                                        :comment ""
    ;;                                                                        :delivery nil
    ;;                                                                        :delivery-price 0
    ;;                                                                        :marked nil
    ;;                                                                        :rank 0)
    ;;                                                                    (append-link (a-resources offer) offer-resource)
    ;;                                                                    (hunchentoot:redirect (format nil "/offer-resource/~A" id))))
    ;;                                                          )
    ;;                                                         )))))))

    ;; ;; Страница ресурса заявки
    ;; (:place                offer-resource
    ;;  :url                  "/offer-resource/:id"
    ;;  :actions
    ;;  '((:linear            "Ресурс заявки"
    ;;     :perm              :all
    ;;     :entity            offer-resource
    ;;     :val               (gethash (cur-page-id) *OFFER-RESOURCE*)
    ;;     :fields            '(#|offer|#
    ;;                          (:fld tender-resource)
    ;;                          (:fld quantity)
    ;;                          (:fld price)
    ;;                          (:fld price-result)
    ;;                          (:fld comment)
    ;;                          (:fld delivery)
    ;;                          (:fld delivery-price)
    ;;                          #|marked rank|#
    ;;                          (:btn "Сохранить"
    ;;                           :perm :all
    ;;                           :act  (let ((obj (gethash (cur-page-id) *OFFER-RESOURCE*)))
    ;;                                   (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
    ;;                                   (setf (a-marked obj) (not (null (cdr (assoc "MARKED" (form-data) :test #'equal)))))
    ;;                                   (with-obj-save obj
    ;;                                     quantity price price-result comment delivery delivery-price rank)
    ;;                                   (hunchentoot:redirect (hunchentoot:request-uri*))))))
    ;;    (:linear            "Вернуться к заявке"
    ;;     :perm              :all
    ;;     :entity            offer-resource
    ;;     :val               :clear
    ;;     :fields            '((:btn "Вернутся к заявке"
    ;;                           :perm :all
    ;;                           :act (let* ((offer    (a-offer (gethash (cur-page-id) *OFFER-RESOURCE*)))
    ;;                                       (offer-id (caar (remove-if-not #'(lambda (x) (equal offer (cdr x))) (cons-hash-list *OFFER*)))))
    ;;                                  (hunchentoot:redirect (format nil "/offer/~A" offer-id))))))))


    ;; Календарь событий
    (:place                event
     :url                  "/event"
     :navpoint             "Календарь событий"
     :actions
     '((:announce          "Анонсы"
        :entity            post-item
        :val               (remove-if-not #'(lambda (x)
                                              (equal "ivent" (a-section (cdr x))))
                            (cons-hash-list *POST-ITEM*))
        :fields            '(,(def-fld title)
                             ,(def-fld date)
                             ,(def-fld announce-photo)
                             ,(def-fld announce)))))

    ;; Новые технологии
    (:place                technologies
     :url                  "/technologies"
     :navpoint             "Технологии"
     :actions
     '((:announce          "Новые технологии"
        :entity            post-item
        :val               (remove-if-not #'(lambda (x)
                                              (equal "techno" (a-section (cdr x))))
                            (cons-hash-list *POST-ITEM*))
        :fields            '(,(def-fld title)
                             ,(def-fld date)
                             ,(def-fld announce-photo)
                             ,(def-fld announce)))))


    ;; Новости законодальства
    (:place                laws
     :url                  "/laws"
     ;; :navpoint             "Новости законодательства"
     :actions
     '((:announce          "Новости законодательства"
        :entity            post-item
        :val               (remove-if-not #'(lambda (x)
                                              (equal "laws" (a-section (cdr x))))
                            (cons-hash-list *POST-ITEM*))
        :fields            '(,(def-fld title)
                             ,(def-fld date)
                             ,(def-fld announce-photo)
                             ,(def-fld announce)))))

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
