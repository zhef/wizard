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
;;                                  (redirect (hunchentoot:request-uri*))))
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
;; (redirect (hunchentoot:request-uri*)))))))))
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
;;                                  (redirect (hunchentoot:request-uri*))))))
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
;;                                   (redirect (hunchentoot:request-uri*))))
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
;;                                  (redirect (hunchentoot:request-uri*))))))))

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
