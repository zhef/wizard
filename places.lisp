(in-package #:wizard)

(defmacro def-fld (name &key (update nil update-p) (show nil show-p) (view nil view-p) (xref nil xref-p)  (width 200 width-p))
  (let ((rs `(:fld ,name)))
    (if update-p  (nconc rs `(:update ,update)))
    (if view-p    (nconc rs `(:view   ,view)))
    (if show-p    (nconc rs `(:show   ,show)))
    (if xref-p    (nconc rs `(:xref   ,xref)))
    (if width-p   (nconc rs `(:width  ,width)))
    `',rs))

(defmacro def-btn ((title perm &key (width 200 width-p)) &body act)
  (let ((rs `(:btn ,title :perm ,perm)))
    (if width-p (nconc rs `(:width ,width)))
    (nconc rs `(:act ,@act))
    `',rs))

(defmacro def-upl ((file perm name))
  (let ((rs `(:file ,file :perm ,perm :name ,name)))
    `',rs))

(defmacro def-pop ((title perm &key (height 100 height-p)  (width 200 width-p)) &body action)
  (let ((rs `(:popbtn ,title :perm ,perm)))
    (if height-p (nconc rs `(:height ,height)))
    (if width-p  (nconc rs `(:width  ,width)))
    (nconc rs `(:action ,@action))
    `',rs))

(defmacro def-grd ((title perm entity val &key (height 100 height-p)) &body fields)
  (let ((rs `(:grid ,title :perm ,perm :entity ,entity :val ,val)))
    (if height-p (nconc rs `(:height ,height)))
    (nconc rs `(:fields ',(loop :for item :in fields :collect
                             (eval (macroexpand-1 item)))))
    `',rs))

(defmacro def-lin ((title perm entity val) &body fields)
  (let ((rs `(:linear ,title :perm ,perm :entity ,entity :val ,val)))
    (nconc rs `(:fields ',(loop :for item :in fields :collect
                             (eval (macroexpand-1 item)))))
    `',rs))

(defmacro def-tpl ((tpl) &body val)
  (let ((rs `(:tpl ,tpl)))
    (nconc rs `(:val ,@val))
    `',rs))

(defmacro def-map ((yamap) &body val)
  (let ((rs `(:yamap ,yamap)))
    (nconc rs `(:val ,@val))
    `',rs))

(defmacro def-nop ((none))
  (let ((rs `(:none ,none)))
    `',rs))

(defmacro def-pst ((post entity val) &body fields)
  (let ((rs `(:post ,post :entity ,entity :val ,val)))
    (nconc rs `(:fields ',(loop :for item :in fields :collect
                             (eval (macroexpand-1 item)))))
    `',rs))

(defmacro def-ann ((announce entity val) &body fields)
  (let ((rs `(:announce ,announce :entity ,entity :val ,val)))
    (nconc rs `(:fields ',(loop :for item :in fields :collect
                             (eval (macroexpand-1 item)))))
    `',rs))

(defmacro def-plc ((name url &key (navpoint nil navpoint-p)) &body actions)
  "TODO: -controllers-"
  `(let ((rs (list :place ',name :url ,url)))
     ,(if navpoint-p
          `(progn
             (nconc rs (list :navpoint ,navpoint))
             (if (boundp '-navpoints-) ;; if exists special var -navpoints- — save navpoint!
                 (nconc -navpoints- (list (list :link ,url :title ,navpoint))))))
     (nconc rs (list :actions (list 'quote (list ,@actions))))
     rs))

(defmacro def-asm (&body places)
  "TODO: -ajax-data-set-"
  `(let* ((-navpoints- (list 'dymmy)))
     (declare (special -navpoints-)) ;; special for menu
     (let ((-places- (list ,@(loop :for item :in places :collect item))))
       (defparameter *places* (remove-if #'null -places-))
       (defparameter *navpoints* (cdr -navpoints-)))))

(def-asm
  ;; Главная страница
  (def-plc (main "/" :navpoint "Главная")
    (def-tpl ("")
      (funcall (find-symbol "MAIN" 'tpl)
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
                                              :posts (posts-by-sales 3)))))))

  ;; Страница регистрации
  (def-plc (register "/register")
    (def-lin ("Регистрация" :all supplier :clear)
      (def-fld  login             :update :all)
      (def-fld  password          :update :all)
      (def-fld  email             :update :all)
      (def-fld  name              :update :all)
      (def-fld  inn               :update :all)
      (def-fld  ogrn              :update :all)
      (def-fld  juridical-address :update :all)
      (def-fld  actual-address    :update :all)
      (def-fld  contact-person    :update :all)
      (def-fld  contact-phone     :update :all)
      (def-btn  ("Зарегистрироваться" :all :width 120)
        (with-obj-create (*USER* 'SUPPLIER (login password email name inn ogrn juridical-address actual-address contact-person contact-phone))
          (setf (a-status obj) :unfair)
          (hunchentoot:redirect (format nil "/supplier/~A" id))))))

  ;; Новости
  (def-plc (posts "/posts" :navpoint "Новости")
    (def-tpl ("Новости")
      (funcall (find-symbol "POSTPAGE" 'tpl)
               (list :postblocks (list (list* :xrefall "/posts"
                                              :titleall "все новости"
                                              :title "Новости строительной сферы"
                                              :posts (posts-by-section "news" 3))
                                       (list* :xrefall "/technologies"
                                              :titleall "все новости"
                                              :title "Новые технологии"
                                              :posts (posts-by-section "techno" 3)))))))

  ;; Новость
  (def-plc (post "/post/:id")
    (def-pst ("%|title|%" post-item (gethash (cur-page-id) *POST-ITEM*))
      (def-fld title)
      (def-fld date)
      (def-fld text-photo)
      (def-fld text)))

  ;; Аналитика
  (def-plc (anal "/analytics" :navpoint "Аналитика")
    (def-nop ("Аналитика")))

  ;; Каталог ресурсов
  (def-plc (material "/material" :navpoint "Каталог ресурсов")
    (def-grd ("Группы" :all category (cons-inner-objs *CATEGORY*
                                                      (a-child-categoryes
                                                       (cdr (car (remove-if-not #'(lambda (x)
                                                                                    (null (a-parent (cdr x))))
                                                                                (cons-hash-list *CATEGORY*)))))))
      (def-fld name :xref "category" :width 900)))

  ;; Строительная техника
  (def-plc (machine "/machine" :navpoint "Строительная техника")
    (def-grd ("Группы" :all category (cons-inner-objs *CATEGORY*
                                                      (a-child-categoryes
                                                       (cdr (cadr (remove-if-not #'(lambda (x)
                                                                                     (null (a-parent (cdr x))))
                                                                                 (cons-hash-list *CATEGORY*))))))
                       :height     400)
      (def-fld name :xref "category" :width 900)))


  ;; Каталог ресурсов - содержимое категории
  (def-plc (category "/category/:id")
    (def-lin ("Группа" :all category :clear)
      (def-grd ("Подгруппы" :all category (cons-inner-objs *CATEGORY* (a-child-categoryes (gethash (cur-page-id) *CATEGORY*))))
        (def-fld name :xref "category" :width 900))
      (def-grd ("Ресурсы группы" :all resource (cons-inner-objs *RESOURCE*
                                                                (a-resources (gethash (cur-page-id) *CATEGORY*))))
        (def-fld name :xref "resource" :width 900))))


  ;; Страница ресурса
  (def-plc (resource "/resource/:id")
    (def-lin ("Ресурс" :all resource (gethash (cur-page-id) *RESOURCE*))
      (def-fld name)
      (def-fld resource-type)
      (def-fld unit)))


  ;; Список поставщиков
  (def-plc (suppliers "/supplier" :navpoint "Поставщики")
    (def-grd ("Каталог поставщиков" :all supplier (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'SUPPLIER))  (cons-hash-list *USER*)))
      (def-fld name :xref "supplier" :width 300)
      (def-fld actual-address :width 600)))

  ;; Страница поставщика
  (def-plc (supplier "/supplier/:id")
    (def-lin ("Поставщик" :all supplier (gethash (cur-page-id) *USER*))
      (def-fld  name)
      (def-fld  status)
      (def-fld  juridical-address)
      (def-fld  actual-address)
      (def-fld  contacts)
      (def-fld  email)
      (def-fld  site)
      (def-fld  heads)
      (def-fld  inn)
      (def-fld  kpp)
      (def-fld  ogrn)
      (def-fld  bank-name)
      (def-fld  bik)
      (def-fld  corresp-account)
      (def-fld  client-account)
      (def-fld  addresses)
      (def-fld  contact-person)
      (def-fld  contact-phone)
      (def-fld  contact-email)
      (def-btn  ("Сохранить" :self)
        (let ((obj (gethash (cur-page-id) *USER*)))
          (with-obj-save obj
            NAME JURIDICAL-ADDRESS ACTUAL-ADDRESS CONTACTS EMAIL SITE HEADS INN KPP OGRN BANK-NAME
            BIK CORRESP-ACCOUNT CLIENT-ACCOUNT ADDRESSES CONTACT-PERSON contact-phone contact-email)
          (hunchentoot:redirect (hunchentoot:request-uri*))))
      (def-btn  ("Отправить заявку на добросовестность" '(and :self :unfair))
        (progn
          (setf (a-status (gethash (cur-page-id) *USER*)) :request)
          (hunchentoot:redirect (hunchentoot:request-uri*))))
      ;; affiliates
      (def-grd ("Адреса филиалов и магазинов" :all supplier-affiliate
                                              (cons-inner-objs *supplier-affiliate* (a-affiliates (gethash (cur-page-id) *user*))))
        (def-fld address :width 900))
      ;; pricelist
      (def-grd ("Прайс-лист" :all supplier-resource-price-elt (remove-if-not #'(lambda (x)
                                                                                 (equal (a-owner (cdr x)) (gethash (cur-page-id) *user*)))
                                                                             (cons-hash-list *supplier-resource-price-elt*)))
        (def-fld  name  :width 350)
        (def-fld  unit  :width 150)
        (def-fld  price :width 150)
        (def-fld  date  :width 150)
        (def-btn  ("Удалить" :owner :width  100)
          (let* ((key (get-btn-key (caar (form-data))))
                 (hobj (gethash key *supplier-resource-price-elt*)))
            (setf (a-price-elts (cur-user))
                  (remove-if #'(lambda (x) (equal x hobj))
                             (a-price-elts (cur-user))))
            (remhash key *supplier-resource-price-elt*)
            (hunchentoot:redirect (hunchentoot:request-uri*)))))
      ;; upload pricelist
      (def-pop ("Загрузить прайс-лист" :self :height 200 :width 700)
        (def-lin ("Добавление прайс-листа" :self supplier-resource-price-elt :clear)
          (def-upl (file :all "Прайс"))
          (def-btn ("Загрузить" :all)
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
              (hunchentoot:redirect (hunchentoot:request-uri*))))))
      ;; resources
      (def-grd ("Ресурсы для конкурсов" :all supplier-resource
                                        (cons-inner-objs *SUPPLIER-RESOURCE* (a-resources (gethash (cur-page-id) *USER*))))
        (def-fld resource :width 800)
        (def-btn ("Удалить" :owner :width 100)
          (del-inner-obj (caar (form-data)) *SUPPLIER-RESOURCE* (a-resources (gethash (cur-page-id) *USER*)))))
      ;; Добавление ресурса
      (def-pop ("Добавить ресурс" :self :height 400 :width 900)
        (def-grd ("Добавление ресурса" :all resource (cons-hash-list *RESOURCE*) :height 240)
          (def-fld name :width 700)
          (def-btn ("Добавить ресурс" :all :width 120)
            (let* ((owner    (cur-user))
                   (resource (gethash (get-btn-key (caar (form-data))) *RESOURCE*)))
              (add-inner-obj *SUPPLIER-RESOURCE* 'SUPPLIER-RESOURCE (a-resources owner)
                :owner     owner
                :resource  resource
                :price     0)
              (hunchentoot:redirect (hunchentoot:request-uri*))))))
      ;; sales
      (def-grd ("Акции" :all sale (cons-inner-objs *SALE* (a-sales (gethash (cur-page-id) *USER*))))
        (def-fld title :width 800 :xref "sale")
        (def-btn ("Удалить" :owner :width 100)
          (del-inner-obj (caar (form-data)) *SALE* (a-sales (gethash (cur-page-id) *USER*)))))
      ;; Добавление акции
      (def-pop ("Добавить акцию" :self :height 400 :width 900)
        (def-lin ("Добавление акции" :all sale :clear)
          (def-fld title)
          ;; (:fld date)
          ;; (:fld announce-photo)
          ;; (:fld announce)
          ;; (:fld text-photo)
          ;; (:fld text)
          ;; (:fld owner)
          ;; (:fld resource)
          (def-btn ("Добавить акцию" :all)
            (let* ((owner (cur-user)))
              (add-inner-obj *SALE* 'SALE (a-sales owner)
                :owner     owner
                :title     (form-fld title))
              (hunchentoot:redirect (hunchentoot:request-uri*))))))
                                        ; offers
      (def-grd ("Список заявок на тендеры" :logged offer (cons-inner-objs *OFFER* (a-offers (gethash (cur-page-id) *USER*))))
        (def-fld tender :xref "offer" :width 680)
        (def-btn ("Страница заявки" :all :width 115)
          (to "/offer/~A" (caar (form-data))))
        (def-btn ("Удалить заявку" :all :width 105)
          (del-inner-obj (caar (form-data)) *OFFER* (a-offers (gethash (cur-page-id) *USER*))))))
    (def-map ("Адрес поставщика")
      (let* ((supp (gethash (cur-page-id) *USER*))
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
                  (list addr)))))))

  ;; Распродажи
  (def-plc (sales "/sale")
    (def-tpl ("Акции")
      (funcall (find-symbol "POSTPAGE" 'tpl)
               (list :postblocks (list (list* :xrefall "/event"
                                              :titleall ""
                                              :title ""
                                              :posts (posts-by-sales 100)))))))

  ;; Страница распродажи
  (def-plc (sale "/sale/:id")
    (def-pst ("%|title|%" sale (gethash (cur-page-id) *SALE*))
      (def-fld title)
      (def-fld date)
      (def-fld text-photo)
      (def-fld text)))

  ;; Список застройщиков
  (def-plc (builders "/builder")
    (def-grd ("Организации-застройщики" :all builder (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'BUILDER)) (cons-hash-list *USER*)))
      (def-fld name :xref "builder" :width 750)
      (def-fld login :width 150)))

  ;; Страница застройщика
  (def-plc (builder "/builder/:id")
    (def-lin ("Застройщик" :all builder (gethash (cur-page-id) *USER*))
      (def-fld  name)
      (def-fld  juridical-address)
      (def-fld  inn)
      (def-fld  kpp)
      (def-fld  ogrn)
      (def-fld  bank-name)
      (def-fld  bik)
      (def-fld  corresp-account)
      (def-fld  client-account)
      (def-fld  rating)
      (def-btn  ("Сохранить" :all)
                (progn
                  (with-obj-save (gethash (cur-page-id) *USER*)
                    NAME JURIDICAL-ADDRESS INN KPP OGRN BANK-NAME BIK CORRESP-ACCOUNT CLIENT-ACCOUNT RATING)
                  (hunchentoot:redirect (hunchentoot:request-uri*))))
      ;; tenders
      (def-grd ("Тендеры застройщика" :all tender (cons-inner-objs *TENDER* (a-tenders (gethash (cur-page-id) *USER*))))
        (def-fld name :xref "tender" :width 550)
        (def-fld status :width 150)
        (def-fld all :width 200)))
    (def-lin ("Объявить новый тендер" :self tender :clear)
      (def-pop ("Объявить тендер" :self :height 200 :width 600)
        (def-lin ("Задайте название новому тендеру" :self tender :clear)
          (def-fld name :update :all)
          (def-btn ("Продолжить создание тендера" :all)
            (let ((owner (cur-user)))
              (with-obj-create (*TENDER* 'TENDER (name))
                (setf (a-status obj) :unactive)
                (setf (a-owner obj) owner)
                ;; Связываем с владельцем
                (append-link (a-tenders owner) obj)
                ;; Редирект
                (hunchentoot:redirect (format nil "/tender/~A" id)))))))))

  ;; Список тендеров
  (def-plc (tenders "/tender" :navpoint "Тендеры")
    ;; (def-grd ("Тендеры" :all tender (cons-hash-list *TENDER*))
    ;;   (def-fld name :xref "tender")
    ;;   (def-fld status)
    ;;   (def-fld owner))
    (def-ann ("Тендеры" post-item (remove-if-not #'(lambda (x)
                                                     (equal "techno" (a-section (cdr x))))
                                                 (cons-hash-list *POST-ITEM*)))
      (def-fld title)
      (def-fld date)
      (def-fld announce-photo)
      (def-fld announce))
    )

  ;; Страница тендера (поставщик может откликнуться)
  (def-plc (tender "/tender/:id")
    (def-lin ("Тендер" :all tender (gethash (cur-page-id) *TENDER*))
      (def-fld name)
      (def-fld status)
      (def-fld owner)
      (def-fld all)
      (def-fld claim)
      (def-fld analize)
      (def-fld interview)
      (def-fld result)
      ;; winner price
      (def-btn ("Сохранить" :owner)
        (let ((obj (gethash (cur-page-id) *TENDER*)))
          (with-obj-save obj
            name all claim analize interview result)
          (hunchentoot:redirect (hunchentoot:request-uri*))))
      ;; resources
      (def-grd ("Ресурсы тендера" :all tender-resource (cons-inner-objs *TENDER-RESOURCE* (a-resources (gethash (cur-page-id) *TENDER*))))
        (def-fld resource :xref "tender-resource" :width 400)
        (def-fld quantity :width 80)
        (def-fld price :width 80)
        (def-fld delivery :width 100)
        (def-fld basic :width 100)
        (def-btn ("Удалить из тендера" :owner :width 125)
          (let ((etalon (gethash (get-btn-key (caar (last (form-data)))) *TENDER-RESOURCE*)))
            (setf (a-resources (gethash (cur-page-id) *TENDER*))
                  (remove-if #'(lambda (x)
                                 (equal x etalon))
                             (a-resources (gethash (cur-page-id) *TENDER*))))
            (hunchentoot:redirect (hunchentoot:request-uri*)))))
      ;; Добавление ресурса
      (def-pop ("Добавить ресурс" :owner :height 480 :width 800)
        (def-grd ("Выберите ресурсы" :all #|'(and :active :fair)|#  resource (cons-hash-list *RESOURCE*) :height 240)
          (def-fld name :xref "resource" :width 650)
          (def-btn ("Добавить к тендеру" :all :width 140)
            (let* ((key      (get-btn-key (caar (last (form-data)))))
                   (resource (gethash key *RESOURCE*))
                   (tender (gethash (cur-page-id) *TENDER*)))
              #| TODO: Надо находить пересечение с ресурсами поставщика, который видит это |#
              (with-obj-create (*TENDER-RESOURCE* 'TENDER-RESOURCE nil)
                (setf (a-tender obj) tender)
                (setf (a-resource obj) resource)
                (append-link (a-resources tender) elt)
                (hunchentoot:redirect (format nil "/tender-resource/~A" id)))))))
      ;; documents
      (def-grd ("Документы тендера" :all document (cons-inner-objs *DOCUMENT* (a-documents (gethash (cur-page-id) *TENDER*))))
        (def-fld name)
        (def-btn ("Удалить из тендера" :all)
          (let* ((tender    (a-tender document)))
            ;; Удаляем этот документ из тендера и из документов
            (del-inner-obj (caar (last (form-data))) *DOCUMENT* (a-documents tender))))
        (def-btn ("Страница документа" :all)
          (to "/document/~A" (caar (last (form-data)))))
        (def-pop ("Добавить документ" :all)
          (def-lin ("Загрузка документа" :all #|'(and :active :fair)|# document :clear)
            (def-btn ("Загрузить документ (пока не активно)" :all)
              (err "upload-document")))))
      ;; suppliers
      (def-grd ("Поставщики ресурсов" :all supplier
                                      (let ((tender-resources   (mapcar #'a-resource (a-resources (gethash (cur-page-id) *TENDER*))))
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
                                        rs))
        (def-fld name)
        (def-fld email)
        (def-fld inn)
        (def-btn ("Отправить приглашение" :all :width 145)
          (err "send-offer-to-supplier-from-tender"))
        (def-btn ("Страница поставщика" :all :width 145)
          (to "/supplier/~A"  (caar (last (form-data)))))
        (def-btn ("Добавить своего поставщика" :all)
          (err "add-supplier-to-tender")))
      ;; oferts
      (def-grd ("Заявки на тендер" :all offer (cons-inner-objs *OFFER* (a-offers (gethash (cur-page-id) *TENDER*))))
        (def-fld owner)
        (def-fld status)
        (def-btn ("Просмотр заявки" :all :width 140)
          (to "/offer/~A"  (caar (last (form-data))))))
      ;; create offer
      (def-pop ("Ответить заявкой на тендер" :supplier)
        (def-lin ("Вы хотите участвовать в этом тендере?" :all #|'(and :active :fair)|# resource (cons-hash-list *RESOURCE*))
          (def-btn ("Да, хочу!"  :all)
            (with-obj-create (*OFFER* 'OFFER nil)
              (setf (a-owner obj) (cur-user))
              (setf (a-tender obj) (gethash (cur-page-id) *TENDER*))
              (setf (a-status obj) :open)
              (append-link (a-offers (gethash (cur-page-id) *TENDER*)) offer)
              (hunchentoot:redirect (format nil "/offer/~A" id))))))
      (def-pop ("Отменить тендер" :all #|:owner|#)
        (def-lin ("Действительно отменить?" :all #|:owner|# tender :clear)
          (def-btn ("Подтверждаю отмену" :all)
            (progn
              (setf (a-status (gethash (cur-page-id) *TENDER*)) :cancelled)
              (hunchentoot:redirect (hunchentoot:request-uri*))))))
      ))

  ;; Ресурс тендера
  (def-plc (tender-resource "/tender-resource/:id")
    (def-lin ("Ресурс тендера" :all tender-resource (gethash (cur-page-id) *TENDER-RESOURCE*))
      (def-fld tender)
      (def-fld resource)
      (def-fld quantity)
      (def-fld price)
      (def-fld price-date)
      (def-fld comment)
      (def-fld delivery)
      (def-fld basic)
      (def-btn ("Сохранить" :all)
        (let ((obj (gethash (cur-page-id) *TENDER-RESOURCE*)))
          (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
          (setf (a-basic obj) (not (null (cdr (assoc "BASIC" (form-data) :test #'equal)))))
          (with-obj-save obj
            quantity price price-date comment)
          (hunchentoot:redirect (hunchentoot:request-uri*)))))
    (def-lin ("Вернуться к тендеру" :all tender-resource (gethash (cur-page-id) *TENDER-RESOURCE*))
      (def-btn ("Вернутся к тендеру" :all)
        (let* ((tender    (a-tender (gethash (cur-page-id) *TENDER-RESOURCE*)))
               (tender-id (caar (remove-if-not #'(lambda (x) (equal tender (cdr x))) (cons-hash-list *TENDER*)))))
          (hunchentoot:redirect (format nil "/tender/~A" tender-id))))))

  ;; Заявки на тендер
  (def-plc (offers "/offers")
    (def-grd ("Заявки на участие в тендере" :all offer (cons-hash-list *OFFER*))
      (def-fld owner)
      (def-fld tender)
      (def-btn ("Страница заявки" :all :width 120)
        (to "/offer/~A" (caar (form-data))))
      (def-btn ("Страница тендера" :all :width 120)
        (HUNCHENTOOT:REDIRECT
         (FORMAT NIL "/tender/~A" (caar (cons-inner-objs *tender* (list (a-tender (gethash (get-btn-key (caar (form-data))) *offer*))))))))))

  ;; Страница заявки на тендер
  (def-plc (offer "/offer/:id")
    (def-lin ("Заявка на тендер" :all offer (gethash (cur-page-id) *OFFER*))
      (def-fld owner)
      (def-fld tender)
      (def-fld status)
      ;; resources
      (def-grd ("Ресурсы заявки" :all offer-resource (cons-inner-objs *OFFER-RESOURCE* (a-resources (gethash (cur-page-id) *OFFER*))))
        (def-fld tender-resource)
        (def-fld quantity)
        (def-fld price)
        (def-fld price-result)
        #| comment delivery delivery-price market rank |#
        (def-btn ("Удалить из заявки" :owner :width 110)
          (del-inner-obj (caar (last (form-data))) *OFFER-RESOURCE* (a-resources (gethash (cur-page-id) *OFFER*))))
        (def-btn ("Ресурс заявки" :all :width 150)
          (to "/offer-resource/~A" (caar (last (form-data)))))
        (def-pop ("Добавить ресурс к заявке" :owner)
          (def-grd ("Выберите ресурсы" :all #|'(and :active :fair)|# tender-resource
                                       (cons-inner-objs *TENDER-RESOURCE* (a-resources (a-tender (gethash (cur-page-id) *OFFER*)))))
            (def-fld resource)
            (def-btn ("Добавить к заявке" :all)
              (let* ((key             (get-btn-key (caar (last (form-data)))))
                     (tender-resource (gethash key *TENDER-RESOURCE*))
                     (offer           (gethash (cur-page-id) *OFFER*)))
                (with-obj-create (*OFFER-RESOURCE* 'OFFER-RESOURCE nil)
                  (setf (a-offer obj) offer)
                  (setf (a-tender-resource obj) tender-resource)
                  (setf (a-quantity obj) 0)
                  (setf (a-price obj) 0)
                  (setf (a-price-result obj) 0)
                  (setf (a-comment obj) "")
                  (setf (a-delivery obj) nil)
                  (setf (a-delivery-price obj) 0)
                  (setf (a-marked obj) nil)
                  (setf (a-rank obj) 0)
                  (append-link (a-resources offer) obj)
                  (hunchentoot:redirect (format nil "/offer-resource/~A" id))))))))))

  ;; Страница ресурса заявки
  (def-plc (offer-resource "/offer-resource/:id")
    (def-lin ("Ресурс заявки" :all offer-resource (gethash (cur-page-id) *OFFER-RESOURCE*))
      (def-fld tender-resource)
      (def-fld quantity)
      (def-fld price)
      (def-fld price-result)
      (def-fld comment)
      (def-fld delivery)
      (def-fld delivery-price)
      #|marked rank|#
      (def-btn ("Сохранить" :all)
        (let ((obj (gethash (cur-page-id) *OFFER-RESOURCE*)))
          (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
          (setf (a-marked obj) (not (null (cdr (assoc "MARKED" (form-data) :test #'equal)))))
          (with-obj-save obj
            quantity price price-result comment delivery delivery-price rank)
          (hunchentoot:redirect (hunchentoot:request-uri*)))))
    (def-lin ("Вернуться к заявке" :all offer-resource :clear)
      (def-btn ("Вернутся к заявке" :all)
        (let* ((offer    (a-offer (gethash (cur-page-id) *OFFER-RESOURCE*)))
               (offer-id (caar (remove-if-not #'(lambda (x) (equal offer (cdr x))) (cons-hash-list *OFFER*)))))
          (hunchentoot:redirect (format nil "/offer/~A" offer-id))))))

  ;; Календарь событий
  (def-plc (event "/event" :navpoint "Календарь событий")
    (def-ann ("Анонсы" post-item (remove-if-not #'(lambda (x)
                                                    (equal "ivent" (a-section (cdr x))))
                                                (cons-hash-list *POST-ITEM*)))
      (def-fld title)
      (def-fld date)
      (def-fld announce-photo)
      (def-fld announce)))

  ;; Новые технологии
  (def-plc (technologies "/technologies" :navpoint "Технологии")
    (def-ann ("Новые технологии" post-item (remove-if-not #'(lambda (x)
                                                              (equal "techno" (a-section (cdr x))))
                                                          (cons-hash-list *POST-ITEM*)))
      (def-fld title)
      (def-fld date)
      (def-fld announce-photo)
      (def-fld announce)))


  ;; Новости законодальства
  (def-plc (laws "/laws" #|:navpoint"Новости законодательства"|#)
    (def-ann ("Новости законодательства" post-item (remove-if-not #'(lambda (x)
                                                                      (equal "laws" (a-section (cdr x))))
                                                                  (cons-hash-list *POST-ITEM*)))
      (def-fld title)
      (def-fld date)
      (def-fld announce-photo)
      (def-fld announce)))

  ;; О портале
  (def-plc (about "/about")
    (def-tpl ("О портале")
      (funcall (find-symbol "ABOUT" 'tpl))))

  ;; Услуги портала
  (def-plc (services "/services" :navpoint "Услуги портала")
    (def-tpl ("Услуги портала")
      (funcall (find-symbol "SERVICES" 'tpl))))

  ;; Контакты
  (def-plc (contacts "/contacts" :navpoint "Контакты")
    (def-tpl ("Контакты")
      (funcall (find-symbol "CONTACTS" 'tpl))))
  )

