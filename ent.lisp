(in-package #:WIZARD)

(closure-template:compile-template :common-lisp-backend #P"tpl/root.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/right.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/templates.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/main.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/about.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/contacts.htm")
(closure-template:compile-template :common-lisp-backend #P"tpl/services.htm")

;; (restas:mount-submodule -static- (#:restas.directory-publisher)
;;   (restas.directory-publisher:*directory* (path "static/")))

(restas:mount-submodule -css- (#:restas.directory-publisher)
                        (restas.directory-publisher:*baseurl* '("css"))
                        (restas.directory-publisher:*directory* "css/"))

(restas:mount-submodule -js- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("js"))
  (restas.directory-publisher:*directory* "js/"))

(restas:mount-submodule -img- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img"))
  (restas.directory-publisher:*directory* "img/"))

(restas:mount-submodule -img-popups- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img/popups"))
  (restas.directory-publisher:*directory* "img/popups/"))

(restas:mount-submodule -img-jqgrid- (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("img/jqgrid"))
  (restas.directory-publisher:*directory* "img/jqgrid/"))


(defclass entity () ())


;; Структура для хранения интервалов времени
(defstruct interval
  (begin 0 :type integer)
  (end 0 :type integer))


;; Определение типов и их расшифровок
(defmacro define-user-type ((typename varname) &body typelist)
  `(progn
     (deftype ,typename ()
       (cons 'member ,(cons 'list (loop :for key :in typelist :by #'cddr :collect key))))
     (defparameter ,varname ',typelist)))

;; Возможные типы ресурсов: машины, материалы etc
(define-user-type (resource-type *resource-types*)
  :machine "машина"
  :material "материал")
;; (typep :machine2 'resource-type)

;; Возможные статусы тендеров
(define-user-type (tender-status *tender-status*)
  :active "активный"
  :unactive "неактивный"
  :finished "завершенный"
  :cancelled "отмененный")

;; Возможные статусы поставщиков
(define-user-type (supplier-status *supplier-status*) ;; Пока нет схемы перехода поставщика в добросовестного будем переводить через заявку
  :fair "добросовестный"
  :unfair "недобросовестный"
  :request "подана заявка")

;; Возможные статусы заявки
(define-user-type (offer-status *offer-status*)
  :open      "заявка открыта, но не заполнена"
  :sended    "заявка открыта и отправлена поставщику"
  :readed    "заявка прочтена поставщиком"
  :replyed   "поставщик принял решение участвовать в тендере"
  :cancelled "поставщик отменил участие в тендере"
  :invited   "застройщик пригласил поставщика на собеседование"
  :closed    "тендер завершен, заявка закрыта")


;; Типы полей, составляющих сущности - в дальнейшем будут использоваться при диспетчеризации
(define-user-type (fld-type *fld-types*)
  :bool                 "T или NIL (checkbox)"
  :num                  "число"
  :str                  "строка"
  :pswd                 "пароль"
  :list-of-str          "список строк (модификатор: возможность ввода строк пользователем)"
  :link                 "связанная сущность (модификатор: тип сущности)"
  :list-of-links        "список связанных сущностей"
  :list-of-keys         "выпадающий список ключей, с выбором одного из них"
  :text                 "текстовое поле"
  :date                 "дата и время"
  :interval             "диапазоны дат, относящиеся к тендеру"
  :img                  "изображения, картинки")

;; TODO: при создании экземпляра entity проверять тип поля
(defparameter *types* (loop :for ftype :in *fld-types* :by #'cddr :collect ftype))



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
      (name                "Название организации"       (:str) 300)
      (referal             "Реферал"                    (:link user)
                           '(:create :system             ;; Если застройщик привел этого поставщика
                             :view   (or :admin :expert) ;; то здесь ссылка на застройщика
                             :update :nobody))
      (status              "Статус"                     (:list-of-keys supplier-status)
                           '(:view   :all
                             :update :admin))
      (juridical-address   "Юридический адрес"          (:str)
                           '(:view   :logged))          ;; Гость не видит
      (actual-address      "Фактический адрес"          (:str) 600)
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
      (resource            "Ресурс"                     (:link resource) 800)
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
      (name                "Наименование"               (:str) 460)
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
     ((name                "Название"                   (:str) 550
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
      (tender              "Тендер"                     (:link tender) 640)
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
      :update :owner))
    ))


;; Мы считаем, что если у пользователя есть права на редактирование
;; всего объекта или части его полей - то эти поля показываются как
;; доступные для редактирования.

