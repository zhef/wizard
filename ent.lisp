(in-package #:WIZARD)


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
     (:view                (or :admin :self)
      :show                (or :admin :self)
      :update              (or :admin :self)))

  ;; Новости
    (:entity               post-item
     :container            post-item
     :fields
     ((title               "Название новости"           (:str))
      (date                "Дата и время"               (:date))
      (announce-photo      "Фото в анонсе"              (:img))
      (announce            "Анонс"                      (:str))
      (text-photo          "Фото в тексте"              (:img))
      (text                "Текст"                      (:str))
      (section             "Раздел"                     (:str)))
     :perm
     (:view                :all
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
                           '(:view   (or :admin :expert) ;; Если застройщик привел этого поставщика то здесь ссылка на застройщика
                             :update :nobody))
      (status              "Статус"                     (:list-of-keys supplier-status)
                           '(:view   :all
                             :update :admin))
      (juridical-address   "Юридический адрес"          (:str)
                           '(:view   :logged))          ;; Гость не видит
      (actual-address      "Фактический адрес"          (:str))
      (contacts            "Контактные телефоны"        (:txt)     ;; cписок телефонов с возможностью ввода
                           '(:view   (or :logged :fair)))                  ;; незалогиненные могут видеть только тел. добросовестных
      (email               "Email"                      (:str)             ;; отображение как ссылка mailto://....
                           '(:view   (or :logged :fair)))
      (site                "Сайт организации"           (:str)             ;; отображение как ссылка http://....
                           '(:view   (or :logged :fair)))
      (heads               "Руководство"                (:txt)
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
      (addresses           "Адреса офисов и магазинов" (:txt)
                           '(:view   (or :logged :fair)))                   ;; Гость не видит у недобросовестных
      (affiliates          "Адреса офисов и магазинов" (:list-of-links supplier-affiliate))
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
     (:view               :all
      :show               :all
      :update             :self))


    ;; Филиалы поставщика
    (:entity               supplier-affiliate
     :container            supplier-affiliate
     :fields
     ((owner               "Поставщик"                  (:link supplier))
      (address             "Адрес"                      (:str)))
     :perm
     (:view               :all ;; (or :logged :fair))
      :show               :all
      :update             (or :admin :self)))


    ;; Связующий объект: Распродажи - связывает поставщика, объявленный им ресурс и хранит условия скидки
    (:entity               sale
     :container            sale
     :fields
     ((title               "Название акции"             (:str))
      (date                "Дата и время"               (:interval))
      (announce-photo      "Фото в анонсе"              (:img))
      (announce            "Анонс"                      (:str))
      (text-photo          "Фото в тексте"              (:img))
      (text                "Текст"                      (:str))
      (owner               "Поставщик"                  (:link supplier))
      (resource            "Ресурс"                     (:link supplier-resource)))
     :perm
     (:view   :all
      :show   :all
      :update :all))


    ;; Связующий объект - ресурсы, заявленные поставщиком
    (:entity               supplier-resource
     :container            supplier-resource
     :fields
     ((owner               "Поставщик"                  (:link supplier)
                           '(:update :nobody))
      (resource            "Ресурс"                     (:link resource))
      (price               "Цена поставщика"            (:num)))
     :perm
     (:view   :all
      :show   :all
      :update :owner))


    ;; Ресурсы, которые поставщик загружает на свою страницу (из xls-файлов)
    (:entity               supplier-resource-price-elt
     :container            supplier-resource-price-elt
     :fields
     ((owner               "Поставщик"                  (:link supplier))
      (name                "Наименование"               (:str))
      (unit                "Единица измерения"          (:str))
      (price               "Цена"                       (:str))
      (date                "Дата загрузки"              (:str)))
     :perm
     (:view   :all
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
      (contacts            "Контактные телефоны"        (:txt))    ;; cписок телефонов с возможностью ввода
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
      (client-account      "Расчетный счет"            (:str)
                           '(:view   :logged))                              ;; Гость не видит банковские реквизиты
      (tenders             "Тендеры"                    (:list-of-links tender)
                           '(:view   :all))
      (rating              "Рейтинг"                    (:num)
                           '(:update :system)))
     :perm
     (:view   :all
      :show   :all
      :update :self))


    ;; Иерархический каталог ресурсов


    ;; Категория - группа ресурсов, не содержащая в себе ресурсы, а ссылающаяся на них
    (:entity               category
     :container            category
     :fields
     ((name                "Имя"                        (:str))
      (parent              "Родительская группа"        (:link category))
      (child-categoryes    "Дочерние группы"            (:list-of-links category))
      (resources           "Ресурсы"                    (:list-of-links resource)))
     :perm
     (:view   :all
      :show   :all
      :update :system))


    ;; Ресурс
    (:entity               resource
     :container            resource
     :fields
     ((name                "Наименование"               (:str))
      (category            "Группа"                     (:link category))
      (resource-type       "Тип ресурса"                (:list-of-keys resource-types))
      (unit                "Единица измерения"          (:str))
      (suppliers           "Поставляющие организации"   (:list-box supplier))
      (resource-prices     "Цены ресурса"               (:list-of-links resource-price)))
     :perm
     (:view   :all
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
     (:view   :all
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
     (:view   :all
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
     (:view   :all ;; (and :logged (or :stale (and :fresh :fair)))
      :show   :all
      :update :owner))


    ;; Ресурс, заявленный в тендере
    (:entity               tender-resource
     :container            tender-resource
     :fields
     ((tender             "Тендер"                      (:link tender))
      (resource           "Название ресурса"            (:link resource))
      (quantity           "Кол-во"                      (:num))
      (price              "Цена"                        (:num)) ;; Первоначально цена заполняется из справочника
      (price-date         "Дата справочника цен"        (:str))
      (comment            "Комментарий"                 (:text))
      (delivery           "Доставка"                    (:bool))
      (basic              "Основной"                    (:bool)))
     :perm
     (:view   (and :logged (or :stale (and :fresh :fair)))
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
      (tender              "Тендер"                     (:link tender))
      (status              "Состояние"                  (:list-of-keys  offer-status))
      (resources           "Ресурсы"                    (:list-of-links offer-resource))
      (allow-modify        "Разрешено изменять"         (:bool)))
      :perm
     (:view   :all
      :show   :all
      :update (and :active :owner)    ;; Заявка модет быть отредактирвана пока срок приема заявок не истек.
      ))


    ;; Связующий объект: Ресурсы и цены для заявки на участие в тендере
    (:entity               offer-resource
     :container            offer-resource
     :fields
     ((offer              "Заявка"                      (:link offer))
      (tender-resource    "Ресурс тендера"              (:link tender-resource))
      (quantity           "Кол-во"                      (:num))
      (price              "Цена до собеседования"       (:num)) ;; Первоначально цена заполняется из справочника
      (price-result       "Цена после собеседования"    (:num))
      (comment            "Комментарий"                 (:str))
      (delivery           "Доставка"                    (:bool))
      (delivery-price     "Стоимость доставки"          (:num))
      (marked             "Отметка застройщика"         (:bool))
      (rank               "Занятое место"               (:num)))
     :perm
     (:view   :all
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
     (:view   :all
      :show   :all
      :update :owner))
    ))


;; Мы считаем, что если у пользователя есть права на редактирование
;; всего объекта или части его полей - то эти поля показываются как
;; доступные для редактирования.
