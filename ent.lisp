(in-package #:WIZARD)

(closure-template:compile-template :common-lisp-backend #P"templates.soy")

(restas:mount-submodule -static- (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (path "static/")))

(defclass entity () ())

;; Тип данных для хранения интервалов времени
(defstruct interval
  (begin 0 :type integer)
  (end 0 :type integer))

;; Возможные типы ресурсов: машины, материалы etc
(defparameter *resource-types*
  '(:machine "машина" :material "материал"))

;; Возможные статусы тендеров
(defparameter *tender-status*
  '(:active "активный" :unactive "неактивный" :finished "завершенный" :cancelled "отмененный"))

;; Возможные статусы поставщиков
(defparameter *supplier-status* ;; Пока нет схемы перехода поставщика в добросовестного будем переводить через заявку
  '(:fair "добросовестный" :unfair "недобросовестный" :request "подана заявка"))

;; Возможные статусы заявки
(defparameter *offer-status*
  '(:open      "заявка открыта, но не заполнена"
    :sended    "заявка открыта и отправлена поставщику"
    :readed    "заявка прочтена поставщиком"
    :replyed   "поставщик принял решение участвовать в тендере"
    :cancelled "поставщик отменил участие в тендере"
    :invited   "застройщик пригласил поставщика на собеседование"
    :closed    "тендер завершен, заявка закрыта"))

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
    :text                 ;; текстовое поле
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
      (resources           "Поставляемые ресурсы"      (:list-of-links supplier-resource)
                           '(:add-resource :self   ;; создается связующий объект supplier-resource содержащий установленную поставщиком цену
                             :del-resource :self   ;; удаляется связующий объект
                             :change-price :self))
      (offers              "Посланные приглашения на тендеры"  (:list-of-links offer)
                           '(:view :self
                             :update :self))  ;; offer - связующий объект
      (sales               "Распродажи"                (:list-of-links sale)))    ;; sale - связующий объект
     :perm
     (:create             (or :admin :not-logged)
      :delete             :admin
      :view               :all
      :show               :all
      :update             (or :admin :self)))


    ;; Связующий объект: Распродажи - связывает поставщика, объявленный им ресурс и хранит условия скидки
    (:entity               sale
     :container            sale
     :fields
     ((name                "Распродажа"                 (:str)
                           '(:update :owner))
      (owner               "Поставщик"                  (:link supplier)
                           '(:update :admin))
      (resource            "Ресурс"                     (:link supplier-resource))
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
    (:entity               supplier-resource
     :container            supplier-resource
     :fields
     ((owner               "Поставщик"                  (:link supplier)
                           '(:update :nobody))
      (resource            "Ресурс"                     (:link resource))
      (price               "Цена поставщика"            (:num)))
     :perm
     (:create :owner
      :delete :owner
      :view   :all
      :show   :all
      :update :owner))


    ;; Застройщик - набор полей не утвержден (берем с чужого сайта)
    (:entity               builder
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "Организация-застройщик"     (:str))
      (juridical-address   "Юридический адрес"          (:str))
      (actual-address      "Фактический адрес"          (:str))
      (contacts            "Контактные телефоны"        (:list-of-str))    ;; cписок телефонов с возможностью ввода
      (email               "Email"                      (:str))            ;; отображение как ссылка mailto://....
      (site                "Сайт организации"           (:str))            ;; отображение как ссылка http://....
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
      (resource-type       "Тип ресурса"                (:list-of-keys resource-types))
      (unit                "Единица измерения"          (:str))
      (suppliers           "Поставляющие организации"   (:list-box supplier))
      (resource-prices     "Цены ресурса"               (:list-of-links resource-price)))
     :perm
     (:create :system
      :delete :system
      :view   :all
      :show   :all
      :update :system))


    ;; Цены на ресурс
    (:entity               resource-price
     :container            resource-price
     :fields
     ((estimate            "Сметная цена"               (:num))
      (wholesale           "Оптовая цена"               (:num))
      (price-reference     "Справочник цен"             (:link price-reference))
      (resource            "Ресурс"                     (:link resource)))
     :perm
     (:create :system
      :delete :system
      :view   :all
      :show   :all
      :update :system))


    ;; Справочники цен
    (:entity               price-reference
     :container            price-reference
     :fields
     ((name                "Наименование"               (:str))
      (date                "Дата"                       (:str))
      (resource-prices     "Цены"                       (:list-of-links resource-price)))
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
      (resources           "Ресурсы"                    (:list-of-links tender-resource)
                           '(:update (and :owner :unactive)))
      (documents           "Документы"                  (:list-of-links document) ;; закачка и удаление файлов
                           '(:update (and :owner :unactive)))
      (suppliers           "Поставщики"                 (:list-of-links supplier) ;; строится по ресурсам автоматически при создании тендера
                           '(:update :system))                                    ;; по ресурсам тендера
      (offers              "Заявки"                     (:list-of-links offer)
                           '(:update :system)
                           ))
     :perm
     (:create :builder
      :delete :admin
      :view   (and :logged (or :stale (and :fresh :fair)))
      :show   :all
      :update (or :admin :owner)))


    ;; Ресурс, заявленный в тендере
    (:entity               tender-resource
     :container            tender-resource
     :fields
     ((tender             "Тендер"                      (:link tender))
      (resource           "Ресурс"                      (:link resource))
      (quantity           "Кол-во"                      (:num))
      (price              "Цена"                        (:num)) ;; Первоначально цена заполняется из справочника
      (price-date         "Дата справочника цен"        (:str))
      (comment            "Комментарий"                 (:text))
      (delivery           "Доставка"                    (:bool))
      (basic              "Базовый ресурс"              (:bool)))
     :perm
     (:create :builder
      :delete :admin
      :view   (and :logged (or :stale (and :fresh :fair)))
      :show   :all
      :update (or :admin :owner)))


    ;; Приглашение-заявка на участие в тендере.
    ;; Создается системой при создании тендера для каждого поставщика, который поставляет ресурс, привязанный к тендеру.
    ;; Ресурсы, которые требуются заявлены в тендере.
    ;; Поставщики привязывают к своему приглашению те ресурсы, которые готовы поставить
    ;; После этого застройщик, управляющий тендером может пригласить на собеседование, разрешить поменять заявку
    ;; и выбрать среди приглашений первое и второе место (но это лучше хранить в тендере)
    (:entity               offer
     :container            offer
     :fields
     ((owner               "Поставщик ресурсов"         (:link supplier))
      (tender              "Тендер"                     (:link tender))
      (status              "Состояние"                  (:list-of-keys  offer-status))
      (resources           "Ресурсы"                    (:list-of-links offer-resource))
      (allow-modify        "Разрешено изменять"         (:bool)))
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
     ((offer              "Заявка"                      (:link offer))
      (tender-resource    "Ресурс тендера"              (:link tender-resource) 380)
      (quantity           "Кол-во"                      (:num) 55)
      (price              "Цена до собеседования"       (:num) 150) ;; Первоначально цена заполняется из справочника
      (price-result       "Цена после собеседования"    (:num))
      (comment            "Комментарий"                 (:str))
      (delivery           "Доставка"                    (:bool))
      (delivery-price     "Стоимость доставки"          (:num))
      (marked             "Отметка застройщика"         (:bool))
      (rank               "Занятое место"               (:num)))
     :perm
     (:create :owner
      :delete :owner
      :view   :all
      :show   :all
      :update (and :active :owner)))


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
      :show   :all
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
        :val               (remove-if-not #'(lambda (x)
                                              (null (a-parent (cdr x))))
                            (cons-hash-list *CATEGORY*))
        :fields            '(name ;; parent child-categoryes
                             (:btn "Показать ресурсы"
                              :perm :all
                              :act (to "/category/~A" (caar (form-data))))))))

    ;; Каталог ресурсов - содержимое категории
    (:place                category
     :url                  "/category/:id"
     :actions
     '((:action            "Подкатегории"
        :showtype          :grid
        :perm              :all
        :entity            category
        :val               (cons-inner-objs *CATEGORY* (a-child-categoryes (gethash (cur-id) *CATEGORY*)))
        :fields            '(name
                             ;; parent child-categoryes
                             (:btn "Показать ресурсы"
                              :perm :all
                              :act (to "/category/~A" (caar (form-data))))))
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
        :fields            '(name unit
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
        :showtype          :linear
        :perm              :admin
        :entity            admin
        :val               (cur-user)
        :fields            '(login password
                             (:btn "Изменить пароль"
                              :perm :all
                              :act (let ((obj (cur-user)))
                                     (with-obj-save obj
                                       LOGIN
                                       PASSWORD)))
                             (:btn   "Кнопка всплывающего окна"
                              :perm  :all
                              :popup '(:action            "Заголовок всплывающего окна"
                                       :showtype          :linear
                                       :perm              :admin
                                       :entity            admin
                                       :val               (cur-user)
                                       :fields            '(login password
                                                            (:btn "Изменить пароль"
                                                            :perm :all
                                                             :act (let ((obj (cur-user)))
                                                                    (with-obj-save obj
                                                                      LOGIN
                                                                      PASSWORD))))))))
       (:action            "Создать аккаунт эксперта"
        :showtype          :linear
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
                             (:btn  "Удалить"
                              :perm :all
                              :act  (let ((key (get-btn-key (caar (form-data)))))
                                      (remhash key *USER*)
                                      (hunchentoot:redirect (hunchentoot:request-uri*))))
                             (:btn "Страница эксперта"
                              :perm :all
                              :act (to "/expert/~A" (caar (form-data))))))
       (:action            "Заявки поставщиков на добросовестность"
        :perm              :admin
        :entity            supplier
        :showtype          :grid
        :val               (remove-if-not #'(lambda (x)
                                              (and (equal 'supplier (type-of (cdr x)))
                                                   (equal (a-status (cdr x)) :request)
                                                   ))
                            (cons-hash-list *USER*))
        :fields            '(name login
                             (:btn "Сделать добросовестным"
                              :perm :all
                              :act (let ((key (get-btn-key (caar (form-data)))))
                                     (setf (a-status (gethash key *USER*)) :fair)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))))

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
                              :perm :all
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
        :fields            '(name login))))

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
                             ;; resources
                             (:action            "Список поставляемых ресурсов"
                              :showtype          :grid
                              :perm              :all
                              :entity            supplier-resource
                              :val               (cons-inner-objs *SUPPLIER-RESOURCE* (a-resources (gethash (cur-id) *USER*)))
                              :fields            '(resource price
                                                   (:btn   "Удалить"
                                                    :perm  :all
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SUPPLIER-RESOURCE*
                                                          (a-resources (gethash (cur-id) *USER*))))))
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action             "Добавление ресурса"
                                       :showtype           :grid
                                       :perm               :all
                                       :entity             resource
                                       :val                (cons-hash-list *RESOURCE*)
                                       :fields             '(name
                                                             (:btn "Добавить ресурс" ;; <-------------------- TODO: добавить поля
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
                              ;; offers
                             (:action            "Список заявок на тендеры"
                              :showtype          :grid
                              :perm              :all
                              :entity            offer
                              :val               (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *USER*)))
                              :fields            '(tender
                                                   (:btn "Страница заявки"
                                                    :perm :all
                                                    :act (to "/offer/~A" (caar (form-data))))
                                                   (:btn "Удалить заявку"
                                                    :perm :all
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *OFFER*
                                                          (a-offers (gethash (cur-id) *USER*))))))
                             ;; sale
                             (:action            "Список распродаж"
                              :showtype          :grid
                              :perm              :all
                              :entity            sale
                              :val               (cons-inner-objs *SALE* (a-sales (gethash (cur-id) *USER*)))
                              :fields            '(name
                                                   (:btn "Страница распродажи"
                                                    :perm :all
                                                    :act (to "/sale/~A"  (caar (form-data))))
                                                   (:btn "Удалить распродажу"
                                                    :perm :all
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SALE*
                                                          (a-sales (gethash (cur-id) *USER*))))))
                             (:btn "Добавить распродажу"
                              :perm :all
                              :popup '(:action             "Добавление расподажи"
                                       :showtype           :linear
                                       :perm               :all
                                       :entity             sale
                                       :fields             '((:btn "Добавить распродажу"
                                                              :perm :all
                                                              :act (error "create-sale not implemented")))))))

       (:action            "Отправить заявку на добросовестность" ;; заявка на статус добросовестного поставщика (изменяет статус поставщика)
        :showtype          :linear
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
        :showtype          :linear
        :perm              :all
        :entity            sale
        :val               (gethash (cur-id) *SALE*)
        :fields            '(name owner procent price notes
                             resource
                              (:btn "Сохранить"
                               :perm :all
                               :act (let ((obj (gethash (cur-id) *SALE*)))
                                      (with-obj-save obj
                                        name price procent notes)))))))

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
        :showtype          :linear
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
                             (:action           "Тендеры застройщика"
                              :showtype         :grid
                              :perm             :all
                              :entity           tender
                              :val              (cons-inner-objs *TENDER* (a-tenders (gethash (cur-id) *USER*)))
                              :fields           '(name (:btn "Страница тендера"
                                                        :perm :all
                                                        :act (to "/tender/~A" (caar (last (form-data)))))))
                             ))
       (:action            "Объявить новый тендер"
        :showtype          :linear
        :perm              :self
        :entity            tender
        :val               :clear
        :fields            '(name all claim analize interview result
                             (:btn "Объявить тендер"
                              :perm :all
                              :act ;;(format nil "~A" (form-data))
                              (let* ((id     (hash-table-count *TENDER*))
                                     (owner  (gethash (cur-id) *USER*))
                                     (tender (setf (gethash id *TENDER*)
                                                   (make-instance 'TENDER
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
        :showtype          :linear
        :perm              :all
        :entity            tender
        :val               (gethash (cur-id) *TENDER*)
        :fields            '(name status owner all claim analize interview result ;; winner price
                             (:btn "Сохранить"
                              :perm :all
                              :act (let ((obj (gethash (cur-id) *TENDER*)))
                                     (with-obj-save obj
                                       name active-date all claim analize interview result)))
                             ;; resources
                             (:action           "Ресурсы тендера"
                              :showtype         :grid
                              :perm             :all
                              :entity           tender-resource
                              :val              (cons-inner-objs *TENDER-RESOURCE* (a-resources (gethash (cur-id) *TENDER*)))
                              :fields '(resource
                                        (:btn   "Удалить из тендера"
                                         :perm  :all
                                         :act   (let ((etalon (gethash (get-btn-key (caar (last (form-data)))) *TENDER-RESOURCE*)))
                                                  (setf (a-resources (gethash (cur-id) *TENDER*))
                                                        (remove-if #'(lambda (x)
                                                                       (equal x etalon))
                                                                   (a-resources (gethash (cur-id) *TENDER*))))
                                                  (hunchentoot:redirect (hunchentoot:request-uri*))))
                                        (:btn   "Страница ресурса"
                                         :perm :all
                                         :act   (to "/tender-resource/~A" (caar (last (form-data)))))))
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :showtype          :grid
                                       :perm              :all ;;(and :active :fair)
                                       :entity            resource
                                       :val               (cons-hash-list *RESOURCE*)
                                       :fields            '(name
                                                            (:btn      "Добавить к тендеру"
                                                             :perm     :all
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
                             (:action           "Документы тендера"
                              :showtype         :grid
                              :perm             :all
                              :entity           document
                              :val              (cons-inner-objs *DOCUMENT* (a-documents (gethash (cur-id) *TENDER*)))
                              :fields '(name
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
                             (:btn "Добавить документ"
                              :perm :all
                              :popup '(:action            "Загрузка документа"
                                       :showtype          :linear
                                       :perm              :all ;; (and :active :fair)
                                       :entity            document
                                       :val               :clear
                                       :fields            '((:btn   "Загрузит документ (пока не активно)"
                                                             :perm  :all
                                                             :act   (upload-document)))))
                             ;; suppliers
                             (:action           "Поставщики ресурсов"
                              :showtype         :grid
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
                              :fields           '(name
                                                  (:btn "Отправить приглашение"
                                                   :perm :all
                                                   :act (send-offer-to-supplier-from-tender))
                                                  (:btn "Страница поставщика"
                                                   :perm :all
                                                   :act (to "/supplier/~A"  (caar (last (form-data)))))))
                             (:btn "Добавить своего поставщика"
                              :perm :all
                              :act (add-supplier-to-tender))
                             ;; oferts
                             (:action           "Заявки на тендер"
                              :showtype         :grid
                              :perm             :all
                              :entity           offer
                              :val              (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *TENDER*)))
                              :fields '(owner
                                        (:btn "Страница заявки"
                                         :perm :all
                                         :act (to "/offer/~A"  (caar (last (form-data)))))))
                             ;; create offer
                             (:btn "Ответить заявкой на тендер"
                              :perm :all
                              :popup '(:action            "Вы хотите участвовать в этом тендере?"
                                       :showtype          :linear
                                       :perm              :all ;; (and :active :fair)
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

                             (:btn "Отменить тендер"
                              :perm :all
                              :popup '(:action            "Действительно отменить?"
                                       :showtype           :linear
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
     '((:action            "Ресурс тендера"
        :showtype          :linear
        :perm              :all
        :entity            tender-resource
        :val               (gethash (cur-id) *TENDER-RESOURCE*)
        :fields            '(tender resource quantity price price-date comment delivery basic
                             (:btn "Сохранить"
                              :perm :all
                              :act  (let ((obj (gethash (cur-id) *TENDER-RESOURCE*)))
                                      (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
                                      (setf (a-basic obj) (not (null (cdr (assoc "BASIC" (form-data) :test #'equal)))))
                                      (with-obj-save obj
                                        quantity price price-date comment)))))
       (:action            "Вернуться к тендеру"
        :showtype          :linear
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
                              :act (to "/offer/~A" (caar (form-data))))
                             (:btn "Страница тендера"
                              :perm :all
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
     '((:action            "Заявка на тендер"
        :showtype          :linear
        :entity            offer
        :perm              :all
        :val               (gethash (cur-id) *OFFER*)
        :fields            '(owner tender status
                             ;; resources
                             (:action           "Ресурсы заявки"
                              :showtype         :grid
                              :perm             :all
                              :entity           offer-resource
                              ;; (mapcar #'(lambda (x)
                              ;;             (cons (car x) (a-resources (cdr x))))
                              ;;  (cons-hash-list *OFFER*))
                              :val              (cons-inner-objs *OFFER-RESOURCE* (a-resources (gethash (cur-id) *OFFER*)))
                              :fields '(tender-resource quantity price #|price-result comment delivery delivery-price market rank |#
                                        (:btn "Удалить из заявки"
                                         :perm :all
                                         :width 110
                                         :act (del-inner-obj
                                                 (caar (last (form-data)))
                                                 *OFFER-RESOURCE*
                                                 (a-resources (gethash (cur-id) *OFFER*))))
                                        (:btn   "Страница ресурса заявки"
                                         :width 150
                                         :perm  :all
                                         :act   (to "/offer-resource/~A" (caar (last (form-data)))))))
                             (:btn "Добавить ресурс к заявке"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :showtype          :grid
                                       :perm              :all ;; (and :active :fair)
                                       :entity            tender-resource
                                       :val               (cons-inner-objs *TENDER-RESOURCE* (a-resources (a-tender (gethash (cur-id) *OFFER*))))
                                       :fields            '(resource
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
                             ;;                                ;; (:col "Выберите ресурс"
                             ;;                                ;;  :perm 222
                             ;;                                ;;  :entity resource
                             ;;                                ;;  :val (cons-hash-list *RESOURCE*)
                             ;;                                ;;  :fields '(name
                             ;;                                ;;            (:btn "Добавить ресурс"
                             ;;                                ;;             :perm :all
                             ;;                                ;;             :popup '(:action  "Укажите цену"
                             ;;                                ;;                      :perm    1111
                             ;;                                ;;                      :entity  resource
                             ;;                                ;;                      :val     :clear
                             ;;                                ;;                      :fields  '((:calc "<input type=\"text\" name=\"INPRICE\" />")
                             ;;                                ;;                                 (:btn "Задать цену"
                             ;;                                ;;                                  :perm :all
                             ;;                                ;;                                  :act
                             ;;                                ;;                                  (let ((res-id (get-btn-key(caar (last (form-data)))))
                             ;;                                ;;                                        (in (cdr (assoc "INPRICE" (form-data) :test #'equal)))
                             ;;                                ;;                                        (id (hash-table-count *OFFER-RESOURCE*)))
                             ;;                                ;;                                    (push
                             ;;                                ;;                                     (setf (gethash id *OFFER-RESOURCE*)
                             ;;                                ;;                                           (make-instance 'OFFER-RESOURCE
                             ;;                                ;;                                                          :owner (cur-user)
                             ;;                                ;;                                                          :offer (gethash (cur-id) *OFFER*)
                             ;;                                ;;                                                          :resource (gethash res-id *RESOURCE*)
                             ;;                                ;;                                                          :price in))
                             ;;                                ;;                                     (a-resources (gethash (cur-id) *OFFER*)))
                             ;;                                ;;                                    (hunchentoot:redirect (hunchentoot:request-uri*)))
                             ;;                                ;;                                  ))))))
                             ;;                                )))
                             ;; ))))


    ;; Страница заявки на тендер
    (:place                offer-resource
     :url                  "/offer-resource/:id"
     :actions
     '((:action            "Ресурс заявки"
        :showtype          :linear
        :perm              :all
        :entity            offer-resource
        :val               (gethash (cur-id) *OFFER-RESOURCE*)
        :fields            '(#|offer tender-resource|# quantity price price-result comment delivery delivery-price marked rank
                             (:btn "Сохранить"
                              :perm :all
                              :act  (let ((obj (gethash (cur-id) *OFFER-RESOURCE*)))
                                      (setf (a-delivery obj) (not (null (cdr (assoc "DELIVERY" (form-data) :test #'equal)))))
                                      (setf (a-marked obj) (not (null (cdr (assoc "MARKED" (form-data) :test #'equal)))))
                                      (with-obj-save obj
                                        quantity price price-result comment delivery delivery-price rank)))))
       (:action            "Вернуться к заявке"
        :showtype          :linear
        :perm              :all
        :entity            offer-resource
        :val               :clear
        :fields            '((:btn "Вернутся к заявке"
                              :perm :all
                              :act (let* ((offer    (a-offer (gethash (cur-id) *OFFER-RESOURCE*)))
                                          (offer-id (caar (remove-if-not #'(lambda (x) (equal offer (cdr x))) (cons-hash-list *OFFER*)))))
                                     (hunchentoot:redirect (format nil "/offer/~A" offer-id))))))))


    ;; Рейтинг компаний
    (:place                rating
     :url                  "/rating"
     :navpoint             "Рейтинг компаний"
     :actions
     '((:action            "Рейтинг компаний"
        :showtype          :none
        :perm              :all)))


    ;; Календарь событий
    (:place                calendar
     :url                  "/calender"
     :navpoint             "Календарь событий"
     :actions
     '((:action            "Календарь событий"
        :showtype          :none
        :perm              :all)))

    ;; Ссылки
    (:place                links
     :url                  "/links"
     :navpoint             "Ссылки"
     :actions
     '((:action            "Ссылки"
        :showtype          :none
        :perm              :all)))

    ;; О портале
    (:place                about
     :url                  "/about"
     :navpoint             "О портале"
     :actions
     '((:action            "О портале"
        :showtype          :none
        :perm              :all)))

    ;; Контакты
    (:place                contacts
     :url                  "/contacts"
     :navpoint             "Контакты"
     :actions
     '((:action            "Контакты"
        :showtype          :none
        :perm              :all)))

   ))



(restas:define-route test ("/test")
  (tpl:test))

(restas:define-route test/post ("/test" :method :post)
  (if (null (car (hunchentoot:post-parameter "file")))
      (tpl:test)
      ;; else
      (format nil "<pre>~A</pre>" (xls-processor (car (hunchentoot:post-parameter "file"))))))

