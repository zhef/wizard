(in-package #:WIZARD)


(defun auth (login password)
  (loop :for obj :being the :hash-values :in *USER* :using (hash-key key) :do
     (when (and (equal (a-login obj) login)
                (equal (a-password obj) password))
       (return-from auth
         (progn
           (setf (hunchentoot:session-value 'userid) key)
           (hunchentoot:redirect (hunchentoot:request-uri*))))))
  (hunchentoot:redirect (hunchentoot:request-uri*)))


;; Перед вызовом действия (даже если это показ поля) в процедуру проверки прав передается правило, субьект действия (пользователь)
;; и объект действия (объект, над котором действие совершается), если разрешение получено - выполняется действие
;; Разрешения полей перекрывают разрешения определенные для сущности, если они есть, иначе поля получают разрешения общие для сущности.


;; Возможные действия над объектами
(defparameter *perm-actions*
    '(:create   ;; Создание
      :delete   ;; Удаление
      :view     ;; Отображение
      :show     ;; Отображение в составе коллекции
      :update   ;; Изменение
      ))


(defun perm-check (perm subj obj)
  "subj - cur-user, obj - cur-obj"
  (cond ((consp    perm)
         (loop :for item :in perm :collect (perm-check item subj obj)))
        ((keywordp perm)
         (ecase perm
           (:all       t)                                            ;; Все пользователи
           (:nobody    nil)                                          ;; Никто
           (:system    nil) ;; Система (загрузка данных на старте и изменение статуса поставщиков, когда время добросовестности истеклл)
           ;; Subjects
           (:notlogged (if subj nil t))                              ;; Незалогиненный пользователь (может зарегистрироваться как поставщик)
           (:logged    (if subj t nil))                              ;; Залогиненный пользователь
           (:admin     (if (equal (type-of subj) 'ADMIN) t nil))     ;; Администратор
           (:expert    (if (equal (type-of subj) 'EXPERT) t nil))    ;; Пользователь-Эксперт
           (:builder   (if (equal (type-of subj) 'BUILDER) t nil))   ;; Пользователь-Застройщик
           (:supplier  (if (equal (type-of subj) 'SUPPLIER) t nil))  ;; Пользователь-Поставщик
           ;; Objects
           (:fair      (if (equal (type-of obj) 'SUPPLIER)           ;; Объект является добросовестным поставщиком
                           (if (equal (a-status obj) :fair)
                               t
                               nil)
                           nil))
           (:unfair    (if (equal (type-of obj) 'SUPPLIER)          ;; Объект является недобросовестным поставщиком
                           (if (equal (a-status obj) :unfair)
                               t
                               nil)
                           nil))
           (:active    (error "perm-todo :active"))    ;; Объект является активным тендером, т.е. время подачи заявок не истекло
           (:unacitve  (error "perm-todo :unacitve"))  ;; Объект является неакивным тендером, т.е. время подачи заявок не наступило
           (:fresh     (error "perm-todo :fresh"))     ;; Объект является свежим тендером, т.е. недавно стал активным
           (:stale     (error "perm-todo :stale"))     ;; Объект является тендером, который давно стал активным
           (:finished  (error "perm-todo :finished"))  ;; Объект является завершенным тендером
           (:cancelled (error "perm-todo :cancelled")) ;; Объект является отмененным тендером
           ;; Mixed
           (:selfpage  (destructuring-bind (root obj-type id) ;; Объект выполняет операцию на своей странице
                           (request-list)
                         (if (and (not (null obj-type))
                                  (not (null id))
                                  (equal id (format nil "~A" (parse-integer id :junk-allowed t)))
                                  (string= (string-upcase obj-type) (type-of (gethash (parse-integer id :junk-allowed t) *USER*))))
                             t
                             nil)))
           (:owner     (destructuring-bind (root obj-type id) ;; Объект, над которым совершается действие имеет поле owner текущего пользователя
                           (request-list)
                         (if (and (not (null obj-type))
                                  (not (null id))
                                  (equal id (format nil "~A" (parse-integer id :junk-allowed t))))
                             (let* ((hash   (intern  (string-upcase (format nil "*~A*"  "tender")) :wizard))
                                    (target (gethash (parse-integer "12" :junk-allowed t) (symbol-value hash))))
                               (if (and (slot-exists-p target 'owner)
                                        (equal (a-owner target) subj))
                                   t
                                   nil)))))
           ))
        (t perm)))


(let* ((hash   (intern  (string-upcase (format nil "*~A*"  "tender"))))
       (target (gethash (parse-integer "12" :junk-allowed t) (symbol-value hash))))
  target)


(defun check-perm (perm subj obj)
  ;; t)
  (let ((rs (eval (perm-check perm subj obj))))
    (safe-write (path "perm-log.txt") (format nil "perm: ~A; result: ~A; subj: ~A; obj: ~A~%" perm rs subj obj))
    (eval rs)
  ))


;; TEST
;; (check-perm '(or :ADMIN :SELF) (gethash 0 *USER*) (gethash 1 *USER*))
;; (perm-check '(or :admin (and :all :nobody)) 1 2)
;; (check-perm '(or :admin (or :all :nobody)) 1 2)
;; (check-perm ':nobody 1 2)
;; (check-perm ':nobody 1 2)
