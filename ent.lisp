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
      (inn                 "ИНН"                        (:str)
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
      (contact-phone       "Контактный телефон"        (:str)
                           '(:view   (or :logged :fair)))                   ;; Гость не видит у недобросовестных
      (contact-email       "Контактный email"          (:str)
                           '(:view   (or :logged :fair)))                   ;; Гость не видит у недобросовестных
      (resources           "Поставляемые ресурсы"      (:list-of-links supplier-resource))
      (price-elts          "Ресурсы из прайсa"         (:list-of-links supplier-resource-price-elt))
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
      (resource            "Ресурс"                     (:link supplier-resource) 600)
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
      (resource            "Ресурс"                     (:link resource) 485)
      (price               "Цена поставщика"            (:num) 55))
     :perm
     (:create :owner
      :delete :owner
      :view   :all
      :show   :all
      :update :owner))


    ;; Ресурсы, которые поставщик загружает на свою страницу (из xls-файлов)
    (:entity               supplier-resource-price-elt
     :container            supplier-resource-price-elt
     :fields
     ((owner               "Поставщик"                  (:link supplier))
      (name                "Наименование"               (:str))
      (unit                "Единица измерения"          (:str))
      (price               "Цена"                       (:str)))
     :perm
     (:create :supplier
      :delete :owner
      :view   :all
      :show   :all
      :update :owner))


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
     ((name                "Имя"                        (:str) 900)
      (parent              "Родительская группа"        (:link category))
      (child-categoryes    "Дочерние группы"            (:list-of-links category))
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
     ((name                "Наименование"               (:str) 900)
      (category            "Группа"                     (:link category))
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
     ((name                "Название"                   (:str) 450
                           '(:view   :all))
      (status              "Статус"                     (:list-of-keys tender-status) 120
                           '(:view   :all))
      (owner               "Заказчик"                   (:link builder)
                           '(:update :admin))
      ;; Дата, когда тендер стал активным (первые сутки новые тендеры видят только добростовестные поставщики)
      (all                 "Срок проведения"            (:interval) 150
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
      (resource           "Название ресурса"            (:link resource) 400)
      (quantity           "Кол-во"                      (:num) 50)
      (price              "Цена"                        (:num) 50) ;; Первоначально цена заполняется из справочника
      (price-date         "Дата справочника цен"        (:str))
      (comment            "Комментарий"                 (:text))
      (delivery           "Доставка"                    (:bool) 55)
      (basic              "Основной"                    (:bool) 50))
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
     ((owner               "Название организации"       (:link supplier))
      (tender              "Тендер"                     (:link tender) 400)
      (status              "Состояние"                  (:list-of-keys  offer-status) 300)
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
      (tender-resource    "Ресурс тендера"              (:link tender-resource) 345)
      (quantity           "Кол-во"                      (:num) 55)
      (price              "Цена до собеседования"       (:num) 150) ;; Первоначально цена заполняется из справочника
      (price-result       "Цена после собеседования"    (:num) 155)
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
     :navpoint             "Главная"
     :actions
     '((:action            "Главная"
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
        :fields            '((:fld title)
                             (:fld date)
                             (:fld photo-announce)
                             (:fld announce)
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
        :fields            '((:fld title)
                             (:fld date)
                             (:fld photo-text)
                             (:fld text)))))


    ;; Аналитика
    (:place                anal
     :url                  "/anal"
     :navpoint             "Аналитика"
     :actions
     '((:action            "Аналитика"
        :showtype          :none
        :perm              :all)))


    ;; Каталог материалов
    (:place                material
     :url                  "/material"
     :navpoint             "Каталог ресурсов"
     :actions
     '((:action            "Группы"
        :showtype          :grid
        :perm              :all
        :entity            category
        :val               (cons-inner-objs *CATEGORY*
                            (a-child-categoryes
                             (cdr (car (remove-if-not #'(lambda (x)
                                                          (null (a-parent (cdr x))))
                                                      (cons-hash-list *CATEGORY*))))))
        :height            400
        :fields            '((:fld name :xref "category")
                             ;; (:btn "Показать ресурсы"
                             ;;  :perm :all
                             ;;  :width 120
                             ;;  :act (to "/category/~A" (caar (form-data))))
                             ))))


    ;; Каталог материалов
    (:place                machine
     :url                  "/machine"
     :navpoint             "Строительная техника"
     :actions
     '((:action            "Группы"
        :showtype          :grid
        :perm              :all
        :entity            category
        :val               (cons-inner-objs *CATEGORY*
                            (a-child-categoryes
                             (cdr (cadr (remove-if-not #'(lambda (x)
                                                          (null (a-parent (cdr x))))
                                                      (cons-hash-list *CATEGORY*))))))
        :height            400
        :fields            '((:fld name :xref "category")))))


    ;; Каталог ресурсов - содержимое категории
    (:place                category
     :url                  "/category/:id"
     :actions
     '((:action             "Группа"
        :showtype           :linear
        :perm               :all
        :entity             category
        :val                :clear
        :fields             '((:action            "Подгруппы"
                               :showtype          :grid
                               :perm              :all
                               :entity            category
                               :val               (cons-inner-objs *CATEGORY* (a-child-categoryes (gethash (cur-id) *CATEGORY*)))
                               :fields            '((:fld name :xref "category")))
                              (:action            "Ресурсы группы"
                               :showtype          :grid
                               :perm              :all
                               :entity            resource
                               :val               (remove-if-not #'(lambda (x)
                                                                     (equal (a-category (cdr x))
                                                                            (gethash (cur-id) *CATEGORY*)))
                                                   (cons-hash-list *RESOURCE*))
                               :fields            '((:fld name :xref "resource")))))))

    ;; Страница ресурса (ресурсы редактированию не подвергаются)
    (:place                resource
     :url                  "/resource/:id"
     :actions
     '((:action            "Ресурс"
        :perm              :all
        :showtype          :linear
        :entity            resource
        :val               (gethash (cur-id) *RESOURCE*)
        :fields            '((:fld name)
                             (:fld category)
                             (:fld resource-type)
                             (:fld unit)))))

    ;; Личный кабинет Администратора
    (:place                admin
     :url                  "/admin"
     ;; :navpoint             "Администратор"
     :actions
     '((:action            "Изменить себе пароль"
        :showtype          :linear
        :perm              :admin
        :entity            admin
        :val               (cur-user)
        :fields            '((:fld login)
                             (:fld password)
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
                                       :fields            '((:fld login)
                                                            (:fld password)
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
        :fields            '((:fld login)
                             (:fld password)
                             (:fld name)
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
        :fields            '((:fld name)
                             (:fld login)
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
        :fields            '((:fld name)
                             (:fld login)
                             (:btn "Сделать добросовестным"
                              :perm :all
                              :act (let ((key (get-btn-key (caar (form-data)))))
                                     (setf (a-status (gethash key *USER*)) :fair)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))))

    ;; Список экспертов
    (:place                experts
     :url                  "/expert"
     ;; :navpoint             "Эксперты"
     :actions
     '((:action            "Эксперты"
        :showtype          :grid
        :perm              :all
        :entity            expert
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'EXPERT)) (cons-hash-list *USER*))
        :fields            '((:fld name)
                             (:fld login)
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
        :fields            '((:fld name)
                             (:fld login)))))

    ;; Список поставщиков
    (:place                suppliers
     :url                  "/supplier"
     :navpoint             "Поставщики"
     :actions
     '((:action            "Каталог поставщиков"
        :showtype          :grid
        :perm              :all
        :entity            supplier
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'SUPPLIER))  (cons-hash-list *USER*))
        :fields            '((:fld name)
                             #|login|#
                             (:fld actual-address)
                             (:btn "Страница поставщика"
                              :perm :all
                              :width 130
                              :act (to "/supplier/~A" (caar (form-data))))))))
    ;; Страница поставщика
    (:place                supplier
     :url                  "/supplier/:id"
     :actions
     '((:action            "Поставщик"
        :showtype          :linear
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
                             ;; pricelist
                             (:action            "Прайс-лист"
                              :showtype          :grid
                              :perm              :all
                              :entity            supplier-resource-price-elt
                              :val               (remove-if-not #'(lambda (x)
                                                                    (equal (a-owner (cdr x)) (gethash (cur-id) *user*)))
                                                  (cons-hash-list *supplier-resource-price-elt*))
                              :fields            '((:fld name)
                                                   (:fld unit)
                                                   (:fld price)
                                                   (:btn    "Удалить"
                                                    :perm   '(or :admin :owner)
                                                    :width  100
                                                    :act    (delete-supplier-resource-price-elt))))

                             (:btn               "Загрузить прайс-лист"
                              :perm              '(or :admin :self)
                              :popup '(:action             "Добавление ресурса"
                                       :showtype           :linear
                                       :perm               '(or :admin :self)
                                       :entity             supplier-resource-price-elt
                                       :val                :clear
                                       :fields             '((:file file
                                                              :perm :all
                                                              :name "Прайс")
                                                             (:btn "Загрузить"
                                                              :perm :all
                                                              :act
                                                              (progn
                                                                (awhen (car (hunchentoot:post-parameter "FILE"))
                                                                  (loop :for src :in (xls-processor it) :do
                                                                     (let ((obj (push-hash *supplier-resource-price-elt* 'supplier-resource-price-elt
                                                                                  :owner (cur-user)
                                                                                  :name (nth 1 src)
                                                                                  :unit (nth 2 src)
                                                                                  :price (nth 3 src))))
                                                                       (append-link (a-price-elts (cur-user)) obj))
                                                                     )
                                                                  )
                                                                (hunchentoot:redirect (hunchentoot:request-uri*)))))))
                             ;; resources
                             (:action            "Ресурсы для конкурсов"
                              :showtype          :grid
                              :perm              :all
                              :entity            supplier-resource
                              :val               (cons-inner-objs *SUPPLIER-RESOURCE* (a-resources (gethash (cur-id) *USER*)))
                              :fields            '((:fld resource)
                                                   (:btn   "Удалить"
                                                    :perm  :all
                                                    :width 60
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SUPPLIER-RESOURCE*
                                                          (a-resources (gethash (cur-id) *USER*))))))
                             (:btn               "Добавить ресурс"
                              :perm              '(or :admin :self)
                              :popup '(:action             "Добавление ресурса"
                                       :showtype           :grid
                                       :perm               :all
                                       :entity             resource
                                       :val                (cons-hash-list *RESOURCE*)
                                       :fields             '((:fld name)
                                                             (:btn "Добавить ресурс"
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
                             (:action            "Акции"
                              :showtype          :grid
                              :perm              :all
                              :entity            sale
                              :val               (cons-inner-objs *SALE* (a-sales (gethash (cur-id) *USER*)))
                              :fields            '((:fld name)
                                                   (:btn "Страница распродажи"
                                                    :perm :all
                                                    :act (to "/sale/~A"  (caar (form-data))))
                                                   (:btn "Удалить распродажу"
                                                    :perm :owner
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *SALE*
                                                          (a-sales (gethash (cur-id) *USER*))))))
                             (:btn "Добавить распродажу"
                              :perm :nobody
                              :popup '(:action             "Добавление расподажи"
                                       :showtype           :linear
                                       :perm               :self
                                       :entity             sale
                                       :fields             '((:btn "Добавить распродажу"
                                                              :perm :all
                                                              :act (error "create-sale not implemented")))))
                             ;; offers
                             (:action            "Список заявок на тендеры"
                              :showtype          :grid
                              :perm              :admin
                              :entity            offer
                              :val               (cons-inner-objs *OFFER* (a-offers (gethash (cur-id) *USER*)))
                              :fields            '((:fld tender)
                                                   (:btn "Страница заявки"
                                                    :perm :all
                                                    :width 105
                                                    :act (to "/offer/~A" (caar (form-data))))
                                                   (:btn "Удалить заявку"
                                                    :perm :all
                                                    :width 100
                                                    :act (del-inner-obj
                                                          (caar (form-data))
                                                          *OFFER*
                                                          (a-offers (gethash (cur-id) *USER*))))))
                             ))
       (:action            "Адрес поставщика"
        :showtype          :map
        :perm              :all
        :val               (let ((addr (a-actual-address (gethash 2 *USER*))))
                             (list (list (geo-coder addr) addr (a-name (gethash 2 *USER*))))))

       (:action            "Отправить заявку на добросовестность" ;; заявка на статус добросовестного поставщика (изменяет статус поставщика)
        :showtype          :linear
        :perm              '(and :self :unfair)
        :entity            supplier
        :val               (gethash (cur-id) *USER*)
        :fields            '((:btn "Отправить заявку на добросовестность"
                              :perm :all
                              :act (progn
                                     (setf (a-status (gethash (cur-id) *USER*)) :request)
                                     (hunchentoot:redirect (hunchentoot:request-uri*))))))))

    ;; Технологии
    (:place                technologies
     :url                  "/technologies"
     :navpoint             "Технологии"
     :actions
     '((:action            "Технологии"
        :showtype          :none
        :perm              :all)))


    ;; Распродажи
    (:place                sales
     :url                  "/sale"
     ;; :navpoint             "Распродажи"
     :actions
     '((:action            "Распродажи"
        :showtype          :grid
        :perm              :all
        :entity            sale
        :val               (cons-hash-list *SALE*)
        :fields            '((:fld name)
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
        :fields            '((:fld name)
                             (:fld owner)
                             (:fld procent)
                             (:fld price)
                             (:fld notes)
                             (:fld resource)
                              (:btn "Сохранить"
                               :perm :all
                               :act (let ((obj (gethash (cur-id) *SALE*)))
                                      (with-obj-save obj
                                        name price procent notes)))))))

    ;; Список застройщиков
    (:place                builders
     :url                  "/builder"
     ;; :navpoint             "Застройщики"
     :actions
     '((:action            "Организации-застройщики"
        :showtype          :grid
        :perm              :all
        :entity            builder
        :val               (remove-if-not #'(lambda (x) (equal (type-of (cdr x)) 'BUILDER)) (cons-hash-list *USER*))
        :fields            '((:fld name)
                             (:fld login)
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
                             (:action           "Тендеры застройщика"
                              :showtype         :grid
                              :perm             :all
                              :entity           tender
                              :val              (cons-inner-objs *TENDER* (a-tenders (gethash (cur-id) *USER*)))
                              :fields           '((:fld name)
                                                  (:fld status)
                                                  (:fld all)
                                                  (:btn "Страница тендера"
                                                   :perm :all
                                                   :width 120
                                                   :act (to "/tender/~A" (caar (last (form-data)))))))
                             ))
       (:action            "Объявить новый тендер"
        :showtype          :linear
        :perm              :self
        :entity            tender
        :val               :clear
        :fields            '((:fld name)
                             (:btn "Объявить тендер"
                              :perm :all
                              :act ;;(format nil "~A" (form-data))
                              (let* ((id     (hash-table-count *TENDER*))
                                     (owner  (gethash (cur-id) *USER*))
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
     '((:action            "Тендеры"
        :showtype          :grid
        :perm              :all
        :entity            tender
        :val               (cons-hash-list *TENDER*)
        :fields            '((:fld name)
                             (:fld status)
                             (:fld owner)
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
                             (:action           "Ресурсы тендера"
                              :showtype         :grid
                              :perm             :all
                              :entity           tender-resource
                              :val              (cons-inner-objs *TENDER-RESOURCE* (a-resources (gethash (cur-id) *TENDER*)))
                              :fields '((:fld resource)
                                        (:calc  "Ед.изм."
                                         :perm :all
                                         :width 40
                                         :func (lambda (x) (a-unit (a-resource x))))
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
                             (:btn "Добавить ресурс"
                              :perm :all
                              :popup '(:action            "Выберите ресурсы"
                                       :showtype          :grid
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
                             (:action           "Документы тендера"
                              :showtype         :grid
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
                             (:btn "Добавить документ"
                              :perm :all
                              :popup '(:action            "Загрузка документа"
                                       :showtype          :linear
                                       :perm              :all ;; '(and :active :fair)
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
                              :fields           '((:fld name)
                                                  (:fld email)
                                                  (:fld inn)
                                                  (:btn "Отправить приглашение"
                                                   :perm :all
                                                   :width 140
                                                   :act (send-offer-to-supplier-from-tender))
                                                  (:btn "Страница поставщика"
                                                   :perm :all
                                                   :width 140
                                                   :act (to "/supplier/~A"  (caar (last (form-data)))))))
                             (:btn "Добавить своего поставщика"
                              :perm :nobody
                              :act (add-supplier-to-tender))
                             ;; oferts
                             (:action           "Заявки на тендер"
                              :showtype         :grid
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
                             (:btn "Ответить заявкой на тендер"
                              :perm :supplier
                              :popup '(:action            "Вы хотите участвовать в этом тендере?"
                                       :showtype          :linear
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

                             (:btn "Отменить тендер"
                              :perm :owner
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
     ;; :navpoint             "Заявки на участие в тендере"
     :actions
     '((:action            "Заявки на участие в тендере"
        :showtype          :grid
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
     '((:action            "Заявка на тендер"
        :showtype          :linear
        :entity            offer
        :perm              :all
        :val               (gethash (cur-id) *OFFER*)
        :fields            '((:fld owner)
                             (:fld tender)
                             (:fld status)
                             ;; resources
                             (:action           "Ресурсы заявки"
                              :showtype         :grid
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
                             (:btn "Добавить ресурс к заявке"
                              :perm :owner
                              :popup '(:action            "Выберите ресурсы"
                                       :showtype          :grid
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
     '((:action            "Ресурс заявки"
        :showtype          :linear
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

    ;; Календарь событий
    (:place                calendar
     :url                  "/calender"
     :navpoint             "Календарь событий"
     :actions
     '((:action            "Календарь событий"
        :showtype          :none
        :perm              :all)))

    ;; О портале
    (:place                about
     :url                  "/about"
     :navpoint             "О портале"
     :actions
     '((:action            "О портале"
        :showtype          :tpl
        :perm              :all
        :val               #'tpl:about)))

    ;; Услуги портала
    (:place                services
     :url                  "/services"
     :navpoint             "Услуги портала"
     :actions
     '((:action            "Услуги портала"
        :showtype          :tpl
        :perm              :all
        :val               #'tpl:services)))

    ;; Контакты
    (:place                contacts
     :url                  "/contacts"
     :navpoint             "Контакты"
     :actions
     '((:action            "Контакты"
        :showtype          :tpl
        :perm              :all
        :val               #'tpl:contacts)))
   ))
