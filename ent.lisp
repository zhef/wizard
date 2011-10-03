(in-package #:WIZARD)

(closure-template:compile-template :common-lisp-backend #P"templates.soy")

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (path "static/")))

(defclass entity () ())

;; Возможные типы ресурсов: машины, материалы etc
(defparameter *resource-types*
  '(:machine "машина" :material "материал"))

;; Возможные статусы тендеров
(defparameter *tender-status*
  '(:active "активный" :unactive "неактивный" :finished "завершенный" :cancelled "отмененный"))

;; Возможные статусы поставщиков
(defparameter *supplier-status*
  '(:fair "добросовестный" :unfair "недобросовестный" :request "подана заявка"))
;; Пока нет схемы перехода поставщика в добросовестного будем переводить через заявку

;; Допустимые типы полей, составляющих сущности (TODO: при создании экземпляра entity написать
;; проверку, чтобы тип входил в этот список)
(defparameter *types*
  '(:bool                 ;; T или NIL (checkbox)
    :num                  ;; число
    :str                  ;; строка
    :pswd                 ;; пароль
    :list-of-str          ;; список строк (модификатор: возможность ввода строк пользователем)
    :link                 ;; связанная сущность (модификатор: тип сущности)
    :list-of-links        ;; список связанных сущностей
    :list-of-keys         ;; выпадающий список ключей, с выбором одного из них
    :text-box             ;; текстовое поле
    :date                 ;; дата и время
    :interval             ;; диапазоны дат, относящиеся к тендеру
    :img                  ;; изображения, картинки
    ))

;; Сущности, используемые в программе, по ним строятся объекты и контейнеры, в которых они хранятся.
;; Также названия полей используются для построения интерфейсов CRUD
;; Для полей наследуются разрешения объекта, если иное явно не указано в поле
(defparameter *entityes*
  '(

    ;; Администратор
    (:entity               admin
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd)))
     :perm
     (:view                :self
      :update              :self))

    ;; Эксперт - имеет доступ не ко всем тендерам (в будущем!)
    (:entity               expert
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "ФИО"                        (:str)))
     :perm
     (:create              :admin
      :delete              :admin
      :view                (or :admin :self)
      :show                (or :admin :self)
      :update              (or :admin :self)))

  ;; Новости
    (:entity               post
     :container            post
     :fields
     ((title               "Название новости"           (:str))
      (date                "Дата и время"               (:date))
      (photo-announce      "Фото в анонсе"              (:img))
      (announce            "Анонс"                      (:str))
      (photo-text          "Фото в тексте"              (:img))
      (text                "Текст"                      (:str)))
     :perm
     (:create              :admin
      :delete              :admin
      :view                :all
      :show                :all
      :update              :admin))

    ;; Поставщик
    (:entity               supplier
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "Название организации"       (:str))
      (referal             "Реферал"                    (:link user)
                           '(:create :system             ;; Если застройщик привел этого поставщика
                             :view   (or :admin :expert) ;; то здесь ссылка на застройщика
                             :update :nobody))
      (status              "Статус"                     (:list-of-keys supplier-status)
                           '(:view   :all
                             :update :admin))
      (juridical-address   "Юридический адрес"          (:str)
                           '(:view   :logged))          ;; Гость не видит
      (actual-address      "Фактический адрес"          (:str))
      (contacts            "Контактные телефоны"        (:list-of-str)     ;; cписок телефонов с возможностью ввода
                           '(:view   (or :logged :fair)))                  ;; незалогиненные могут видеть только тел. добросовестных
      (email               "Email"                      (:str)             ;; отображение как ссылка mailto://....
                           '(:view   (or :logged :fair)))
      (site                "Сайт организации"           (:str)             ;; отображение как ссылка http://....
                           '(:view   (or :logged :fair)))
      (heads               "Руководство"                (:list-of-str)
                           '(:view   :logged))                              ;; Гости не видят руководство фирм-поставщиков
      (inn                 "Инн"                        (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (kpp                 "КПП"                        (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (ogrn                "ОГРН"                       (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (bank-name           "Название банка"             (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (bik                 "Банковский идентификационный код" (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (corresp-account     "Корреспондентский счет"    (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (client-account      "Расчетный счет"            (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (addresses           "Адреса офисов и магазинов" (:list-of-str)
                           '(:view   (or :logged :fair)))                   ;; Гость не видит у недобросовестных
      (contact-person      "Контактное лицо"           (:str)
                           '(:view   (or :logged :fair)))                   ;; Гость не видит у недобросовестных
      (resources           "Поставляемые ресурсы"      (:list-of-links supplier-resource-price)
                           '(:add-resource :self   ;; создается связующий объект supplier-resource-price содержащий установленную поставщиком цену
                             :del-resource :self   ;; удаляется связующий объект
                             :change-price :self))
      (offers              "Посланные заявки на тендеры"  (:list-of-links offer)
                           '(:view :self
                             :update :self))  ;; offer - связующий объект
      (sales               "Распродажи"                (:list-of-links sale)))    ;; sale - связующий объект
     :perm
     (:create             (or :admin :not-logged)
      :delete             :admin
      :view               :all
      :show               :all
      :update             (or :admin :self)))


    ;; Связующий объект: Заявка на участие в тендере. Связывает поставщика, тендер и ресурсы заявки
    ;; Создается поставщиком, когда он отвечает своим предложением на тендер застройщика
    (:entity               offer
     :container            offer
     :fields
     ((owner               "Поставщик ресурсов"         (:link supplier)
                           '(:update :nobody))
      (tender              "Тендер"                     (:link tender)
                           '(:update :nobody))
      (resources           "Ресурсы заявки"             (:list-of-links offer-resource)))
     :perm
     (:create (and :active :supplier) ;; создается связанный объект offer-resource, содержащие ресурсы заявки
      :delete (and :owner  :active)   ;; удаляются связанный объект offer-resource
      :view   :all
      :show   :all
      :update (and :active :owner)    ;; Заявка модет быть отредактирвана пока срок приема заявок не истек.
      ))


    ;; Связующий объект: Ресурсы и цены для заявки на участие в тендере
    (:entity               offer-resource
     :container            offer-resource
     :fields
     ((owner               "Поставщик"                  (:link supplier)
                           '(:update :nobody))
      (offer               "Заявка"                     (:link offer)
                           '(:update :nobody))
      (resource            "Ресурс"                     (:link resource)
                           '(:update :nobody))
      (price               "Цена поставщика"            (:num)))
     :perm
     (:create :owner
      :delete :owner
      :view   :all
      :update (and :active :owner)))


    ;; Связующий объект: Распродажи - связывает поставщика, объявленный им ресурс и хранит условия скидки
    (:entity               sale
     :container            sale
     :fields
     ((name                "Распродажа"                 (:str)
                           '(:update :owner))
      (owner               "Поставщик"                  (:link supplier)
                           '(:update :admin))
      (resource            "Ресурс"                     (:link supplier-resource-price))
      (procent             "Процент скидки"             (:num))
      (price               "Цена со скидкой"            (:num))
      (notes               "Дополнительные условия"     (:list-of-str)))
     :perm
     (:create :supplier
      :delete :owner
      :view   :all
      :show   :all
      :update :owner))


    ;; Связующий объект - ресурсы, заявленные поставщиком
    (:entity               supplier-resource-price
     :container            supplier-resource-price
     :fields
     ((owner               "Поставщик"                  (:link supplier)
                           '(:update :nobody))
      (resource            "Ресурс"                     (:link resource))
      (price               "Цена поставщика"            (:num)))
     :perm
     (:create :owner
      :delete :owner
      :view   :all
      :update :owner))


    ;; Застройщик - набор полей не утвержден (берем с чужого сайта)
    (:entity               builder
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "Организация-застройщик"     (:str))
      (juridical-address   "Юридический адрес"          (:str))
      (inn                 "Инн"                        (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (kpp                 "КПП"                        (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (ogrn                "ОГРН"                       (:str)
                           '(:view   (or :logged :fair)))                   ;; Незалогиненные видят только добросовестных
      (bank-name           "Название банка"             (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (bik                 "Банковский идентификационный код" (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (corresp-account     "Корреспондентский счет"     (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (client-account      "Рассчетный счет"            (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (tenders             "Тендеры"                    (:list-of-links tender)
                           '(:view   :all))
      (rating              "Рейтинг"                    (:num)
                           '(:update :system)))
     :perm
     (:create :admin
      :delete :admin
      :view   :all
      :show   :all
      :update (or :admin :self)))


    ;; Иерархический каталог ресурсов


    ;; Категория - группа ресурсов, не содержащая в себе ресурсы, а ссылающаяся на них
    (:entity               category
     :container            category
     :fields
     ((name                "Имя"                        (:str))
      (parent              "Родительская категория"     (:link category))
      (child-categoryes    "Дочерние категории"         (:list-of-links category))
      (resources           "Ресурсы"                    (:list-of-links resource)))
     :perm
     (:create :system
      :delete :system
      :view   :all
      :show   :all
      :update :system))


    ;; Ресурс
    (:entity               resource
     :container            resource
     :fields
     ((name                "Наименование"               (:str))
      (category            "Категория"                  (:link category))
      (resource-type       "Тип"                        (:list-of-keys resource-types))
      (unit                "Единица измерения"          (:str))
      (suppliers           "Поставляющие организации"   (:list-box supplier)))
     :perm
     (:create :system
      :delete :system
      :view   :all
      :show   :all
      :update :system))


    ;; Тендеры
    ;; Незалогиненный видит Номер, название, срок проведения, статус
    ;; Недобросовестный поставщик видит то же что и незалогиненный
    (:entity               tender
     :container            tender
     :fields
     ((name                "Название"                   (:str)
                           '(:view   :all))
      (status              "Статус"                     (:list-of-keys tender-status)
                           '(:view   :all))
      (owner               "Заказчик"                   (:link builder)
                           '(:update :admin))
      ;; Дата, когда тендер стал активным (первые сутки новые тендеры видят только добростовестные поставщики)
      (active-date         "Дата активации"             (:date)
                           '(:update :system))
      (all                 "Срок проведения"            (:interval)
                           '(:view   :all
                             :update (or :admin  (and :owner :unactive))))
      (claim               "Срок подачи заявок"         (:interval)
                           '(:update (or :admin  (and :owner :unactive))))
      (analize             "Срок рассмотрения заявок"   (:interval)
                           '(:update (or :admin  (and :owner :unactive))))
      (interview           "Срок проведения интервью"   (:interval)
                           '(:update (or :admin  (and :owner :unactive))))
      (result              "Срок подведения итогов"     (:interval)
                           '(:update (or :admin (and :owner :unactive))))
      (winner              "Победитель тендера"         (:link supplier)
                           '(:view   :finished))
      (price               "Рекомендуемая стоимость"    (:num) ;; вычисляется автоматически на основании заявленных ресурсов
                           '(:update :system))
      (resources           "Ресурсы"                    (:list-of-links resource)
                           '(:update (and :owner :unactive)))
      (documents           "Документы"                  (:list-of-links document) ;; закачка и удаление файлов
                           '(:update (and :owner :unactive)))
      (suppliers           "Поставщики"                 (:list-of-links supplier) ;; строится по ресурсам автоматически
                           '(:update :system))
      (offers              "Заявки"                     (:list-of-links offer)
                           '(:update :system)))
     :perm
     (:create :builder
      :delete :admin
      :view   (and :logged (or :stale (and :fresh :fair)))
      :show   :all
      :update (or :admin :owner)))


    ;; Связанные с тендерами документы
    (:entity               document
     :container            document
     :fields
     ((name                "Название"                   (:str))
      (filename            "Имя файла"                  (:str))
      (tender              "Тендер"                     (:link tender)))
     :perm
     (:create :owner
      :delete (and :owner :unactive)
      :view   :all
      :update :owner))))


;; Мы считаем, что если у пользователя есть права на редактирование
;; всего объекта или части его полей - то эти поля показываются как
;; доступные для редактирования.


(defparameter *places*
  '(
    ;; Главная страница
    (:place                main
     :url                  "/"
     :navpoint             "Главная страница"
     :actions
     '((:action            "Главная страница"
        :showtype          :none
        :perm              :all)))

    ;; Новости
    (:place                posts
     :url                  "/posts"
     :navpoint             "Новости"
     :actions
     '((:action            "Анонсы"
        :showtype          :grid
        :perm              :all
        :entity            post
        :val               (cons-hash-list *POST*)
        :fields            '(title date photo-announce announce
                             (:btn  "Страница новости"
                              :perm :all
                              :act  (to "/post/~A" (caar (form-data))))))))

    ;; Новость
    (:place                post
     :url                  "/post/:id"
     :actions
     '((:action            "Тут будет подставляться заголовок новости или просто оставить пустую строку"
        :showtype          :linear
        :perm              :all
        :entity            post
        :val               (gethash (cur-id) *POST*)
        :fields            '(title date photo-text text))))

    ;; Каталог ресурсов - категории
    (:place                catalog
     :url                  "/catalog"
     :navpoint             "Каталог ресурсов"
     :actions
     '((:action            "Категории"
        :showtype          :grid
        :perm              :all
        :entity            category
        :val               (cons-hash-list *CATEGORY*)
        :fields            '(name ;; parent child-categoryes
                             (:btn "Показать ресурсы"
                              :perm :all
                              :act (to "/category/~A" (caar (form-data))))))))

    ;; Каталог ресурсов - содержимое категории
    (:place                category
     :url                  "/category/:id"
     :actions
     '((:action            "Категории"
        :showtype          :grid
        :perm              :all
        :entity            category
        :val               (cons-hash-list *CATEGORY*)
        :fields            '(name
                             ;; parent child-categoryes
                             (:btn "Показать ресурсы"
                              :perm :all
                              :act (to "/category/~A" (caar (form-data))))))
       ;; Тут ошибка в гриде, так как надо передавать параметр-группу, но скоро там будет дерево, так что можно не исправлять
       (:action            "Ресурсы категории"
        :showtype          :grid
        :perm              :all
        :entity            resource
        :val               (remove-if-not #'(lambda (x)
                                              (equal (a-category (cdr x))
                                                     (gethash (cur-id) *CATEGORY*)))
                            (cons-hash-list *RESOURCE*))
        :fields            '(name resource-type unit
                             (:btn "Страница ресурса"
                              :perm :all
                              :act (to "/resource/~A" (caar (form-data))))))))

    ;; Линейный список ресурсов
    (:place                resources
     :url                  "/resource"
     :navpoint             "Список ресурсов"
     :actions
     '((:action            "Ресурсы"
        :perm              :all
        :showtype          :grid
        :entity            resource
        :val               (cons-hash-list *RESOURCE*)
        :fields            '(name resource-type unit
                             (:btn "Страница категории"
                              :perm :all
                              :act (HUNCHENTOOT:REDIRECT
                                    (FORMAT NIL "/category/~A"
                                            (let ((etalon (a-category (gethash (GET-BTN-KEY (CAAR (form-data))) *RESOURCE*))))
                                              (car (find-if #'(lambda (category-cons)
                                                                (equal (cdr category-cons) etalon))
                                                            (cons-hash-list *CATEGORY*)))))))
                             (:btn "Страница ресурса"
                              :perm :all
                              :act (to "/resource/~A" (caar (form-data))))))))
    ;; Страница ресурса (ресурсы редактированию не подвергаются)
    (:place                resource
     :url                  "/resource/:id"
     :actions
     '((:action            "Ресурс"
        :perm              :all
        :showtype          :linear
        :entity            resource
        :val               (gethash (cur-id) *RESOURCE*)
        :fields            '(name category resource-type unit))))

    ;; Личный кабинет Администратора
    (:place                admin
     :url                  "/admin"
     :navpoint             "Администратор"
     :actions
     '((:action            "Изменить себе пароль"
        :perm              :admin
        :entity            admin
        :val               (cur-user)
        :fields            '(login password
                             (:btn "Изменить пароль"
                              :perm :all
                              :act (let ((obj (cur-user)))
                                     (with-obj-save obj
                                       LOGIN
                                       PASSWORD)))))
       (:action            "Создать аккаунт эксперта"
        :perm              :admin
        :entity            expert
        :val               :clear
        :fields            '(login password name
                             (:btn "Создать новый аккаунт эксперта"
                              :perm :all
                              :act (progn
                                     (push-hash *USER* 'EXPERT
                                       :login (cdr (assoc "LOGIN" (form-data) :test #'equal))
                                       :password (cdr (assoc "PASSWORD" (form-data) :test #'equal))
                                       :name (cdr (assoc "NAME" (form-data) :test #'equal)))
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))
       (:action            "Эксперты"
        :perm              :admin
        :entity            expert
        :val               (remove-if-not #'(lambda (x)
                                              (equal 'expert (type-of (cdr x))))
                            (cons-hash-list *USER*))
        :showtype          :grid
        :fields            '(name login
                             (:btn "Удалить"
                              :perm :all
                              :popup '(:action             "Действительно удалить?"
                                       :perm               :admin
                                       :entity             expert
                                       :showtype           :linear
                                       :fields             '((:btn  "Подтверждаю удаление"
                                                              :perm :all
                                                              :act  (let ((key (get-btn-key (caar (form-data)))))
                                                                      (remhash key *USER*)
                                                                      (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                             (:btn "Сменить пароль"
                              :perm :all
                              :popup '(:action            "Смена пароля эксперта"
                                       :entity            expert
                                       :perm              :admin
                                       :fields            '(password
                                                            (:btn "Изменить пароль эксперта"
                                                             :perm :all
                                                             :act (let ((obj (gethash (get-btn-key (caar (last (form-data)))) *USER*)))
                                                                    (with-obj-save obj
                                                                      PASSWORD))))))
                             (:btn "Страница эксперта"
                              :perm :all
                              :act (to "/expert/~A" (caar (form-data))))
                             ))
       (:action            "Заявки поставщиков на добросовестность"
        :perm              :admin
        :entity            supplier
        :showtype          :grid
        :val               (remove-if-not #'(lambda (x)
                                              (and (equal 'supplier (type-of (cdr x)))
                                                   (equal (a-status (cdr x)) :request)))
                            (cons-hash-list *USER*))
        :fields            '(name login
                             (:btn "Подтвердить заявку"
                              :perm :all
                              :popup '(:action            "Подтвердить заявку поставщика"
                                       :perm               :admin
                                       :entity             supplier
                                       :fields             '((:btn "Сделать добросовестным"
                                                              :perm :all
                                                              :act (let ((key (get-btn-key (caar (form-data)))))
                                                                     (setf (a-status (gethash key *USER*)) :fair)
                                                                     (hunchentoot:redirect (hunchentoot:request-uri*)))))))))))

    ;; Список экспертов
    (:place                experts
     :url                  "/expert"
     :navpoint             "Эксперты"
     :actions
     '((:action            "Эксперты"
        :showtype          :grid
        :perm              :all
        :entity            expert
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'EXPERT)) (cons-hash-list *USER*))
        :fields            '(name login
                             (:btn  "Страница эксперта"
                              :perm (or :admin :self)
                              :act  (to "/expert/~A" (caar (form-data))))
                             (:btn  "Доп кнопка"
                              :perm (or :admin :self)
                              :act  (to "/expert/~A" (caar (form-data))))))))
    ;; Страница эксперта
    (:place                expert
     :url                  "/expert/:id"
     :actions
     '((:action            "Эксперт"
        :showtype          :linear
        :perm              :all
        :entity            expert
        :val               (gethash (cur-id) *USER*)
        :fields            '(name))))

    ;; Список поставщиков
    (:place                suppliers
     :url                  "/supplier"
     :navpoint             "Поставщики"
     :actions
     '((:action            "Организации-поставщики"
        :showtype          :grid
        :perm              :all
        :entity            supplier
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'SUPPLIER))  (cons-hash-list *USER*))
        :fields            '(name login
                             (:btn "Страница поставщика"
                              :perm :all
                              :act (to "/supplier/~A" (caar (form-data))))))))
    ;; Страница поставщика
    (:place                supplier
     :url                  "/supplier/:id"
     :actions
     '((:action            "Изменить себе пароль"
        :showtype          :linear
        :perm              (or :admin :self)
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '(login password
                             (:btn "Изменить пароль"
                              :perm :all
                              :act (let ((obj (cur-user)))
                                     (with-obj-save obj
                                       LOGIN
                                       PASSWORD)))))
       (:action            "Поставщик"
        :showtype          :linear
        :perm              (or :admin :self)
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '(name status juridical-address actual-address contacts email site heads inn kpp ogrn
                             bank-name bik corresp-account client-account addresses contact-person
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *USER*)))
                                     (with-obj-save obj
                                       NAME JURIDICAL-ADDRESS ACTUAL-ADDRESS CONTACTS EMAIL SITE HEADS INN KPP OGRN BANK-NAME
                                       BIK CORRESP-ACCOUNT CLIENT-ACCOUNT ADDRESSES CONTACT-PERSON)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))
                             (:action            "Эксперты"
                              :showtype          :grid
                              :perm              :all
                              :entity            expert
                              :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'EXPERT)) (cons-hash-list *USER*))
                              :fields            '(name login
                                                   (:btn  "Страница эксперта"
                                                    :perm (or :admin :self)
                                                    :act  (to "/expert/~A" (caar (form-data))))
                                                   (:btn  "Доп кнопка"
                                                    :perm (or :admin :self)
                                                    :act  (to "/expert/~A" (caar (form-data))))))
                             ;; resources
                             (:col               "Список поставляемых ресурсов"
                              :perm              222
                              :entity            supplier-resource-price
                              :val               (cons-inner-objs *SUPPLIER-RESOURCE-PRICE* (a-resources (gethash (cur-id) *USER*)))
                              :fields            '(resource price
                                                   (:btn "Удалить"
                                                    :perm :all
                                                    :popup '(:action            "Удаление ресурса"
                                                             :perm              :admin
                                                             :entity            supplier-resource-price
                                                             :fields            '((:btn "Удалить ресурс"
                                                                                   :perm :all
                                                                                   :act (del-inner-obj
                                                                                         (caar (form-data))
                                                                                         *SUPPLIER-RESOURCE-PRICE*
                                                                                         (a-resources (gethash (cur-id) *USER*)))))))))
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action             "Добавление ресурса"
                                       :perm               111
                                       :entity             supplier-resource-price
                                       :fields             '((:btn "Добавить ресурс"
                                                              :perm :all
                                                              :act
                                                              (progn
                                                                (push-hash *SUPPLIER-RESOURCE-PRICE* 'SUPPLIER-RESOURCE-PRICE
                                                                  :owner (gethash (cur-user) *USER*)
                                                                  :resource (gethash
                                                                             (cdr (assoc "res" (form-data) :test #'equal))
                                                                             *RESOURCE*)
                                                                  :price (cdr (assoc "PRICE" (form-data) :test #'equal)))
                                                                (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                              ;; offers
                             (:col               "Список заявок на тендеры"
                              :perm              222
                              :entity            offer
                              :val               (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *USER*)))
                              :fields            '(tender
                                                   (:btn "Страница заявки"
                                                    :perm :all
                                                    :act (to "/offer/~A" (caar (form-data))))
                                                   (:btn "Удалить заявку"
                                                    :perm :all
                                                    :popup '(:action            "Удаление заявки"
                                                             :perm              :admin
                                                             :entity            supplier-resource-price
                                                             :fields            '((:btn "Удалить заявку"
                                                                                   :perm :all
                                                                                   :act (del-inner-obj
                                                                                         (caar (form-data))
                                                                                         *OFFER*
                                                                                         (a-offers (gethash (cur-id) *USER*)))))))))
                             ;; sale
                             (:col               "Список распродаж"
                              :perm              222
                              :entity            sale
                              :val               (cons-inner-objs *SALE* (a-sales (gethash (cur-id) *USER*)))
                              :fields            '(name
                                                   (:btn "Страница распродажи"
                                                    :perm :all
                                                    :act (to "/sale/~A"  (caar (form-data))))
                                                   (:btn "Удалить распродажу"
                                                    :perm :all
                                                    :popup '(:action            "Удаление распродажи"
                                                             :perm              :admin
                                                             :entity            supplier-resource-price
                                                             :fields            '((:btn "Удалить распродажу"
                                                                                   :perm :all
                                                                                   :act (del-inner-obj
                                                                                         (caar (form-data))
                                                                                         *SALE*
                                                                                         (a-sales (gethash (cur-id) *USER*)))))))))
                             (:btn "Добавить распродажу"
                              :perm :all
                              :popup '(:action             "Добавление расподажи"
                                       :perm               222
                                       :entity             sale
                                       :fields             '((:btn "Добавить распродажу"
                                                              :perm :all
                                                              :act (create-sale)))))))
       (:action            "Отправить заявку на добросовестность" ;; заявка на статус добросовестного поставщика (изменяет статус поставщика)
        :perm              (and :self :unfair)
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '((:btn "Отправить заявку на добросовестность"
                              :perm :all
                              :act (progn
                                     (setf (a-status (gethash (cur-id) *USER*)) :request)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))))

    ;; Распродажи
    (:place                sales
     :url                  "/sale"
     :navpoint             "Распродажи"
     :actions
     '((:action            "Распродажи"
        :showtype          :grid
        :perm              :all
        :entity            sale
        :val               (cons-hash-list *SALE*)
        :fields            '(name
                             (:btn "Страница распродажи"
                              :perm :all
                              :act (to "/sale/~A" (caar (form-data))))))))

    ;; Страница распродажи
    (:place                sale
     :url                  "/sale/:id"
     :actions
     '((:action            "Распродажа"
        :perm              :all
        :entity            sale
        :val               (gethash (cur-id) *SALE*)
        :fields            '(name owner procent price notes
                             ;; resource
                             ;; (:col               "Список ресурсов распродажи"
                             ;;  :perm              111
                             ;;  :entity            supplier-resource-price
                             ;;  :val               (cons-inner-objs *SUPPLIER-RESOURCE-PRICE* (a-resource (gethash (cur-id) *SALE*)))
                             ;;  :fields            '(resource price
                             ;;                       (:btn "Удалить"
                             ;;                        :perm :all
                             ;;                        :popup '(:action            "Удаление ресурса"
                             ;;                                 :perm              :admin
                             ;;                                 :entity            supplier-resource-price
                             ;;                                 :fields            '((:btn "Удалить ресурс"
                             ;;                                                       :perm :all
                             ;;                                                       :act
                             ;;                                                       (del-inner-obj
                             ;;                                                        (caar (form-data))
                             ;;                                                        *SUPPLIER-RESOURCE-PRICE*
                             ;;                                                        (a-resource (gethash (cur-id) *SALE*)))
                             ;;                                                       ))))))
                             ;; (:btn "Добавить ресурс"
                             ;;  :perm :all
                             ;;  :popup '(:action             "Добавление ресурса"
                             ;;           :perm               111
                             ;;           :entity             supplier-resource-price
                             ;;           :fields             '((:btn "Добавить ресурс"
                             ;;                                  :perm :all
                             ;;                                  :act
                             ;;                                  (progn
                             ;;                                    (setf (gethash (hash-table-count *SUPPLIER-RESOURCE-PRICE*) *SUPPLIER-RESOURCE-PRICE*)
                             ;;                                          (make-instance 'SUPPLIER-RESOURCE-PRICE
                             ;;                                                         :owner (gethash 3 *USER*)
                             ;;                                                         :resource (gethash
                             ;;                                                                    (cdr (assoc "res" (form-data) :test #'equal))
                             ;;                                                                    *RESOURCE*)
                             ;;                                                         :price (cdr (assoc "PRICE" (form-data) :test #'equal))))
                             ;;                                    (hunchentoot:redirect (hunchentoot:request-uri*)))
                             ;;                                  ))))
                              (:btn "Сохранить"
                               :perm :all
                               :act (save-sale))
                              (:btn "Удалить распродажу"
                               :perm :all
                               :act (delete-sale))))))

    ;; Список застройщиков
    (:place                builders
     :url                  "/builder"
     :navpoint             "Застройщики"
     :actions
     '((:action            "Организации-застройщики"
        :showtype          :grid
        :perm              :all
        :entity            builder
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'BUILDER)) (cons-hash-list *USER*))
        :fields            '(name login
                             (:btn  "Страница застройщика"
                              :perm :all
                              :act  (to "/builder/~A" (caar (form-data))))))))
    ;; Страница застройщика
    (:place                builder
     :url                  "/builder/:id"
     :actions
     '((:action            "Застройщик"
        :perm              :all
        :entity            builder
        :val               (gethash (cur-id) *USER*)
        :fields            '(name juridical-address inn kpp ogrn bank-name bik corresp-account client-account rating
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *USER*)))
                                     (with-obj-save obj
                                       NAME JURIDICAL-ADDRESS INN KPP OGRN BANK-NAME BIK CORRESP-ACCOUNT CLIENT-ACCOUNT RATING)))
                             ;; tenders
                             (:col              "Тендеры застройщика"
                              :perm             222
                              :entity           tender
                              :val              (cons-inner-objs *TENDER* (a-tenders (gethash 11 *USER*)))
                              :fields           '(name (:btn "Страница тендера"
                                                        :perm :all
                                                        :act (to "/tender/~A" (caar (last (form-data)))))))))
       (:action            "Объявить новый тендер"
        :perm              :self
        :entity            tender
        :val               :clear
        :fields            '(name all claim analize interview result
                             (:btn "Объявить тендер (+)"
                              :perm :all
                              :act
                              ;; (format nil "~A" (form-data))
                              (let ((id (hash-table-count *TENDER*)))
                                     (setf (gethash id *TENDER*)
                                           (make-instance 'TENDER
                                                          :name      (cdr (ASSOC "NAME" (FORM-DATA) :test #'equal))
                                                          :status    :unactive
                                                          :owner     (gethash (cur-id) *USER*)
                                                          :all       (cdr (ASSOC "ALL" (FORM-DATA) :test #'equal))
                                                          :claim     (cdr (ASSOC "CLAIM" (FORM-DATA) :test #'equal))
                                                          :analize   (cdr (ASSOC "ANALIZE" (FORM-DATA) :test #'equal))
                                                          :interview (cdr (ASSOC "INTERVIEW" (FORM-DATA) :test #'equal))
                                                          :result    (cdr (ASSOC "RESULT" (FORM-DATA) :test #'equal))
                                                          ))
                                     (hunchentoot:redirect
                                      (format nil "/tender/~A" id)))
                              )))))

    ;; Список тендеров
    (:place                tenders
     :url                  "/tender"
     :navpoint             "Тендеры"
     :actions
     '((:action            "Тендеры"
        :showtype          :grid
        :perm              :all
        :entity            tender
        :val               (cons-hash-list *TENDER*)
        :fields            '(name status owner
                             (:btn "Страница тендера"
                              :perm :all
                              :act (to "/tender/~A" (caar (form-data))))
                             ))))
    ;; Страница тендера (поставщик может откликнуться)
    (:place                tender
     :url                  "/tender/:id"
     :actions
     '((:action            "Тендер"
        :perm              :all
        :entity            tender
        :val               (gethash (cur-id) *TENDER*)
        :fields            '(name status owner active-date all claim analize interview result ;; winner price
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *TENDER*)))
                                     (with-obj-save obj
                                       name active-date all claim analize interview result)))
                             ;; resources
                             (:col              "Ресурсы тендера"
                              :perm             222
                              :entity           tender
                              :val              (cons-inner-objs *RESOURCE* (a-resources (gethash (cur-id) *TENDER*)))
                              :fields '(name
                                        (:btn   "Удалить из тендера"
                                         :perm  :all
                                         :act   (let ((etalon (gethash (get-btn-key (caar (last (form-data)))) *RESOURCE*)))
                                                  (setf (a-resources (gethash (cur-id) *TENDER*))
                                                        (remove-if #'(lambda (x)
                                                                       (equal x etalon))
                                                                   (a-resources (gethash (cur-id) *TENDER*))))
                                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
                                        (:btn   "Страница ресурса"
                                         :perm :all
                                         :act   (to "/resource/~A" (caar (last (form-data)))))))
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :perm              (and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '(
                                                            (:col "Выберите ресурс"
                                                             :perm 222
                                                             :entity resource
                                                             :val (cons-hash-list *RESOURCE*)
                                                             :fields '(name
                                                                       (:btn "Добавить ресурс"
                                                                        :perm :all
                                                                        :act
                                                                        ;; (format nil "~A" (get-btn-key (caar (last (form-data)))))
                                                                        (progn
                                                                          (push
                                                                           (gethash (get-btn-key (caar (last (form-data)))) *RESOURCE*)
                                                                           (a-resources (gethash (cur-id) *TENDER*)))
                                                                          (hunchentoot:redirect (hunchentoot:request-uri*)))))))))

                             ;; documents
                             (:col              "Документы тендера"
                              :perm             111
                              :entity           tender
                              :val              (cons-inner-objs *DOCUMENT* (a-documents (gethash (cur-id) *TENDER*)))
                              :fields '(name
                                        (:btn   "Удалить из тендера"
                                         :perm  :all
                                         :act   (delete-doc-from-tender))
                                        (:btn   "Страница документа"
                                         :perm  :all
                                         :act   (to "/document/~A" (caar (last (form-data)))))))
                             (:btn "Добавить документ"
                              :perm :all
                              :popup '(:action            "Загрузите документ"
                                       :perm              (and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '(
                                                            (:col "Выберите ресурс"
                                                             :perm 222
                                                             :entity resource
                                                             :val (cons-hash-list *RESOURCE*)
                                                             :fields '(name
                                                                       (:btn "Добавить ресурс"
                                                                        :perm :all
                                                                        :act
                                                                        ;; (format nil "~A" (get-btn-key (caar (last (form-data)))))
                                                                        (progn
                                                                          (push
                                                                           (gethash (get-btn-key (caar (last (form-data)))) *RESOURCE*)
                                                                           (a-resources (gethash (cur-id) *TENDER*)))
                                                                          (hunchentoot:redirect (hunchentoot:request-uri*)))
                                                                        ))))))



                             ;; suppliers
                             (:col              "Поставщики ресурсов"
                              :perm             111
                              :entity           tender
                              :val
                              ;; (remove-if-not #'(lambda (x)
                              ;;                                      (equal (type-of (cdr x))
                              ;;                                             'SUPPLIER))
                              ;;                    (cons-hash-list *USER*))
                              (let ((tender-resources   (a-resources (gethash (cur-id) *TENDER*)))
                                                      (all-suppliers      (remove-if-not #'(lambda (x)
                                                                                             (equal (type-of (cdr x))
                                                                                                    'SUPPLIER))
                                                                                         (cons-hash-list *USER*)))
                                                      (supplier-resource  (mapcar #'(lambda (x)
                                                                                      (cons (a-resource (cdr x))
                                                                                            (a-owner (cdr x))))
                                                                                  (cons-hash-list *SUPPLIER-RESOURCE-PRICE*)))
                                                      (result)
                                                      (rs))
                                                  (loop :for tr :in tender-resources :do
                                                     (loop :for sr :in supplier-resource :do
                                                        (when (equal tr (car sr))
                                                          (push (cdr sr) result))))
                                                  (setf result (remove-duplicates result))
                                                  (loop :for rd :in result :do
                                                     (loop :for as :in all-suppliers :do
                                                        (if (equal rd (cdr as))
                                                            (push as rs))
                                                        ))
                                                  rs)
                              :fields '(name
                                        (:btn "Отправить приглашение"
                                         :perm :all
                                         :act (delete-from-tender))
                                        (:btn "Страница поставщика"
                                         :perm :all
                                         :act (to "/supplier/~A"  (caar (last (form-data)))))))
                             (:btn "Добавить своего поставщика"
                              :perm :all
                              :act (add-document-to-tender))
                             ;; oferts
                             (:col              "Заявки на тендер"
                              :perm             111
                              :entity           offer
                              :val              (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *TENDER*)))
                              :fields '((:btn "Страница заявки"
                                         :perm :all
                                         :act (to "/offer/~A"  (caar (last (form-data)))))))
                             ;;
                             (:btn "Ответить заявкой на тендер"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :perm              (and :active :fair)
                                       :entity            resource
                                       :fields            '((:btn "Участвовать в тендере"
                                                             :perm :all
                                                             :act
                                                             (let* ((id    (hash-table-count *OFFER*))
                                                                    (offer (setf (gethash id *OFFER*)
                                                                                 (make-instance 'OFFER
                                                                                                :owner (cur-user)
                                                                                                :tender (gethash (cur-id) *TENDER*)))))
                                                               (push offer (a-offers (gethash (cur-id) *TENDER*)))
                                                               (hunchentoot:redirect (format nil "/offer/~A" id)))))))
                             ;;
                             (:btn "Отменить тендер"
                              :perm :all
                              :popup '(:action            "Действительно отменить?"
                                       :perm               :owner
                                       :entity             tender
                                       :fields             '((:btn "Подтверждаю отмену"
                                                              :perm :all
                                                              :act (hunchentoot:redirect (format nil "/tender"))))))))))

    ;; Заявки на тендер
    (:place                offers
     :url                  "/offers"
     :navpoint             "Заявки на участие в тендере"
     :actions
     '((:action            "Заявки на участие в тендере"
        :showtype          :grid
        :perm              :all
        :entity            offer
        :val               (cons-hash-list *OFFER*)
        :fields            '(owner tender
                             (:btn "Страница заявки"
                              :perm :all
                              :act (to "/offer/~A" (caar (form-data))))))))

    ;; Страница заявки на тендер
    (:place                offer
     :url                  "/offer/:id"
     :actions
     '((:action            "Заявка на тендер"
        :entity            offer
        :perm              :all
        :val               (gethash (cur-id) *OFFER*)
        :fields            '(tender
                             ;; resources
                             (:col              "Ресурсы оферты"
                              :perm             111
                              :entity           offer-resource
                              :val              (cons-inner-objs *OFFER-RESOURCE* (a-resources (gethash (cur-id) *OFFER*)))
                              :fields '(resource price
                                        (:btn   "Удалить из оферты"
                                         :perm  :all
                                         :act   (del-inner-obj
                                                 (caar (last (form-data)))
                                                 *OFFER-RESOURCE*
                                                 (a-resources (gethash (cur-id) *OFFER*))))
                                         ;; (let* ((id (get-btn-key (caar (last (form-data)))))
                                         ;;               (etalon (gethash id *OFFER-RESOURCE*)))
                                         ;;          (setf (a-resources (gethash (cur-id) *OFFER*))
                                         ;;                (remove-if #'(lambda (x)
                                         ;;                               (equal etalon x))
                                         ;;                           (a-resources (gethash (cur-id) *OFFER*)))

                                        (:btn   "Страница ресурса"
                                         :perm  :all
                                         :act   (to "/resource/~A" (caar (last (form-data)))))))
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :perm              (and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '(
                                                            (:col "Выберите ресурс"
                                                             :perm 222
                                                             :entity resource
                                                             :val (cons-hash-list *RESOURCE*)
                                                             :fields '(name
                                                                       (:btn "Добавить ресурс"
                                                                        :perm :all
                                                                        :popup '(:action  "Укажите цену"
                                                                                 :perm    1111
                                                                                 :entity  resource
                                                                                 :val     :clear
                                                                                 :fields  '((:calc "<input type=\"text\" name=\"INPRICE\" />")
                                                                                            (:btn "Задать цену"
                                                                                             :perm :all
                                                                                             :act
                                                                                             (let ((res-id (get-btn-key(caar (last (form-data)))))
                                                                                                   (in (cdr (assoc "INPRICE" (form-data) :test #'equal)))
                                                                                                   (id (hash-table-count *OFFER-RESOURCE*)))
                                                                                               (push
                                                                                                (setf (gethash id *OFFER-RESOURCE*)
                                                                                                      (make-instance 'OFFER-RESOURCE
                                                                                                                     :owner (cur-user)
                                                                                                                     :offer (gethash (cur-id) *OFFER*)
                                                                                                                     :resource (gethash res-id *RESOURCE*)
                                                                                                                     :price in))
                                                                                                (a-resources (gethash (cur-id) *OFFER*)))
                                                                                               (hunchentoot:redirect (hunchentoot:request-uri*)))
                                                                                             )))))))))))))

    ;; Рейтинг компаний
    (:place                rating
     :url                  "/rating"
     :navpoint             "Рейтинг компаний"
     :actions
     '((:action            "Рейтинг компаний"
        :perm              :all)))


    ;; Календарь событий
    (:place                calendar
     :url                  "/calender"
     :navpoint             "Календарь событий"
     :actions
     '((:action            "Календарь событий"
        :perm              :all)))

    ;; Ссылки
    (:place                links
     :url                  "/links"
     :navpoint             "Ссылки"
     :actions
     '((:action            "Ссылки"
        :perm              :all)))

    ;; О портале
    (:place                about
     :url                  "/about"
     :navpoint             "О портале"
     :actions
     '((:action            "О портале"
        :perm              :all)))

    ;; Контакты
    (:place                contacts
     :url                  "/contacts"
     :navpoint             "Контакты"
     :actions
     '((:action            "Контакты"
        :perm              :all)))

    ))

(print "ent.list finished")
