(in-package #:WIZARD)


(defparameter *entityes*
  '(
    ;; Администратор
    (:entity               admin
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "ФИО"                        (:str)))
     :perm
     (:view                :self
      :update              :self))

    ;; Эксперт
    (:entity               expert
     :container            user
     :fields
     ((login               "Логин"                      (:str))
      (password            "Пароль"                     (:pswd))
      (name                "ФИО"                        (:str)))
     :perm
     (:view                :all
      :show                :all
      :update              :all))

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
      :update              :nobody))

    ;; Поставщик
    (:entity               supplier
     :container            user
     :fields
     ((login               "Логин"                             (:str))
      (password            "Пароль"                            (:pswd))
      (name                "Название организации"              (:str))
      (referal             "Реферал"                           (:link user))
      (status              "Статус"                            (:list-of-keys supplier-status))
      (juridical-address   "Юридический адрес"                 (:str))
      (actual-address      "Фактический адрес"                 (:str))
      (contacts            "Контактные телефоны"               (:txt))
      (email               "Email"                             (:str))
      (site                "Сайт организации"                  (:str))
      (heads               "Руководство"                       (:txt))
      (inn                 "ИНН"                               (:str))
      (kpp                 "КПП"                               (:str))
      (ogrn                "ОГРН"                              (:str))
      (bank-name           "Название банка"                    (:str))
      (bik                 "Банковский идентификационный код"  (:str))
      (corresp-account     "Корреспондентский счет"            (:str))
      (client-account      "Расчетный счет"                    (:str))
      (affiliates          "Адреса офисов и магазинов"         (:list-of-links supplier-affiliate))
      (contact-person      "Контактное лицо"                   (:str))
      (contact-phone       "Контактный телефон"                (:str))
      (contact-email       "Контактный email"                  (:str))
      (resources           "Поставляемые ресурсы"              (:list-of-links supplier-resource))
      (price-elts          "Ресурсы из прайсa"                 (:list-of-links supplier-resource-price-elt))
      (offers              "Посланные приглашения на тендеры"  (:list-of-links offer))
      (sales               "Распродажи"                        (:list-of-links sale)))
     :perm
     (:view                '(or :all :self)
      :show                :all
      :update              :self))

    ;; Филиалы поставщика
    (:entity               supplier-affiliate
     :container            supplier-affiliate
     :fields
     ((owner               "Поставщик"                  (:link supplier))
      (address             "Адрес"                      (:str)))
     :perm
     (:view                :all ;; (or :logged :fair))
      :show                :all
      :update              :self))

    ;; Распродажи - связывает поставщика, объявленный им ресурс и хранит условия скидки
    (:entity               sale
     :container            sale
     :fields
     ((title               "Название акции"             (:str))
      (date                "Дата и время"               (:interval))
      (announce-photo      "Фото в анонсе"              (:img))
      (announce            "Анонс"                      (:str))
      (text-photo          "Фото в тексте"              (:img))
      (text                "Текст"                      (:txt))
      (owner               "Поставщик"                  (:link supplier))
      (resource            "Ресурс"                     (:link supplier-resource)))
     :perm
     (:view                :all
      :show                :all
      :update              :all))

    ;; Ресурсы, заявленные поставщиком для конкурсов
    (:entity               supplier-resource
     :container            supplier-resource
     :fields
     ((owner               "Поставщик"                  (:link supplier))
      (resource            "Ресурс"                     (:link resource))
      (price               "Цена поставщика"            (:num)))
     :perm
     (:view                :all
      :show                :all
      :update              :owner))

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
     (:view                :all
      :show                :all
      :update              :owner))

    ;; Застройщик
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
      (inn                 "Инн"                        (:str))
      (kpp                 "КПП"                        (:str))
      (ogrn                "ОГРН"                       (:str))
      (bank-name           "Название банка"             (:str))
      (bik                 "Банковский идентификационный код" (:str))
      (corresp-account     "Корреспондентский счет"     (:str))
      (client-account      "Расчетный счет"            (:str))
      (tenders             "Тендеры"                    (:list-of-links tender))
      (rating              "Рейтинг"                    (:num)))
     :perm
     (:view                :all
      :show                :all
      :update              :self))


    ;; Иерархический каталог ресурсов

    ;; Категория
    (:entity               category
     :container            category
     :fields
     ((name                "Имя"                        (:str))
      (parent              "Родительская группа"        (:link category))
      (child-categoryes    "Дочерние группы"            (:list-of-links category))
      (resources           "Ресурсы"                    (:list-of-links resource)))
     :perm
     (:view                :all
      :show                :all
      :update              :nobody))

    ;; Ресурс
    (:entity               resource
     :container            resource
     :fields
     ((name                "Наименование"               (:str))
      (categoryes          "Группы"                     (:list-of-links category))
      ;; (category            "Группа"                     (:link category))
      (resource-type       "Тип ресурса"                (:list-of-keys resource-types))
      (unit                "Единица измерения"          (:str))
      (suppliers           "Поставляющие организации"   (:list-box supplier))
      (resource-prices     "Цены ресурса"               (:list-of-links resource-price)))
     :perm
     (:view                :all
      :show                :all
      :update              :nobody))

    ;; Цены на ресурс
    (:entity               resource-price
     :container            resource-price
     :fields
     ((estimate            "Сметная цена"               (:num))
      (wholesale           "Оптовая цена"               (:num))
      (price-reference     "Справочник цен"             (:link price-reference))
      (resource            "Ресурс"                     (:link resource)))
     :perm
     (:view                :all
      :show                :all
      :update              :nobody))

    ;; Справочники цен
    (:entity               price-reference
     :container            price-reference
     :fields
     ((name                "Наименование"               (:str))
      (date                "Дата"                       (:str))
      (resource-prices     "Цены"                       (:list-of-links resource-price)))
     :perm
     (:view                :all
      :show                :all
      :update              :nobody))


    ;; Тендеры
    ;; Незалогиненный видит Номер, название, срок проведения, статус
    ;; Недобросовестный поставщик видит то же что и незалогиненный
    (:entity               tender
     :container            tender
     :fields
     ((name                "Название"                   (:str))
      (status              "Статус"                     (:list-of-keys tender-status))
      (owner               "Заказчик"                   (:link builder))
      ;; Дата, когда тендер стал активным (первые сутки новые тендеры видят только добростовестные поставщики)
      (all                 "Срок проведения"            (:interval))
      (claim               "Срок подачи заявок"         (:interval))
      (analize             "Срок рассмотрения заявок"   (:interval))
      (interview           "Срок проведения интервью"   (:interval))
      (result              "Срок подведения итогов"     (:interval))
      (winner              "Победитель тендера"         (:link supplier))
      (price               "Рекомендуемая стоимость"    (:num)) ;; вычисляется автоматически на основании заявленных ресурсов
      (resources           "Ресурсы"                    (:list-of-links tender-resource))
      (documents           "Документы"                  (:list-of-links tender-document)) ;; закачка и удаление файлов
      (suppliers           "Поставщики"                 (:list-of-links supplier)) ;; строится по ресурсам автоматически при создании тендера
      (offers              "Заявки"                     (:list-of-links offer)))
     :perm
     (:view                :all ;; (and :logged (or :stale (and :fresh :fair)))
      :show                :all
      :update              :owner))

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
     (:view               :all  ;;(and :logged (or :stale (and :fresh :fair)))
      :show               :all
      :update             :all))  ;;(or :admin :owner)))


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
     (:view                :all
      :show                :all
      :update              :all)) ;;(and :active :owner)    ;; Заявка модет быть отредактирвана пока срок приема заявок не истек.

    ;; Связующий объект: Ресурсы и цены для заявки на участие в тендере
    (:entity                offer-resource
     :container             offer-resource
     :fields
     ((offer               "Заявка"                      (:link offer))
      (tender-resource     "Ресурс тендера"              (:link tender-resource))
      (quantity            "Кол-во"                      (:num))
      (price               "Цена до собеседования"       (:num)) ;; Первоначально цена заполняется из справочника
      (price-result        "Цена после собеседования"    (:num))
      (comment             "Комментарий"                 (:str))
      (delivery            "Доставка"                    (:bool))
      (delivery-price      "Стоимость доставки"          (:num))
      (marked              "Отметка застройщика"         (:bool))
      (rank                "Занятое место"               (:num)))
     :perm
     (:view                :all
      :show                :all
      :update              :all)) ;; (and :active :owner)))


    ;; Связанные с тендерами документы
    (:entity               tender-document
     :container            tender-document
     :fields
     ((name                "Название"                   (:str))
      (origin              "Оригинальное имя файла"     (:str))
      (filename            "Имя файла в директории"     (:str))
      (tender              "Тендер"                     (:link tender)))
     :perm
     (:view                :all
      :show                :all
      :update              :owner))
    ))
