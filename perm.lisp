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
  (cond ((consp    perm)
         (loop :for item :in perm :collect (perm-check item subj obj)))
        ((keywordp perm)
         (ecase perm
           (:all         t)                                          ;; "Все пользователи"
           (:nobody      nil)                                        ;; "Никто"
           (:system      nil) ;; "Система (загрузка данных на старте и изменение статуса поставщиков, когда время добросовестности истеклл)"
           (:notlogged   (not (cur-user)))                           ;; "Незалогиненный пользователь (может зарегистрироваться как поставщик)"
           (:logged      (unless (cur-user) t))                      ;; "Залогиненный пользователь"
           (:admin       (if (equal (type-of subj) 'ADMIN) t nil))   ;; "Администратор"
           (:expert      nil) ;; "Пользователь-Эксперт"
           (:builder     nil) ;; "Пользователь-Застройщик"
           (:supplier    nil) ;; "Пользователь-Поставщик"
           ;; Objects
           (:fair        (if (equal (type-of subj) 'SUPPLIER)        ;; "Объект является добросовестным поставщиком"
                             (if (equal (a-status (cur-user)) :fair) t nil)
                             nil))
           (:unfair      (if (equal (type-of subj) 'SUPPLIER)        ;; "Объект является недобросовестным поставщиком"
                             (if (equal (a-status (cur-user)) :unfair) t nil)
                             nil))
           (:active      nil) ;; "Объект является активным тендером, т.е. время подачи заявок не истекло"
           (:unacitve    nil) ;; "Объект является неакивным тендером, т.е. время подачи заявок не наступило"
           (:fresh       nil) ;; "Объект является свежим тендером, т.е. недавно стал активным"
           (:stale       nil) ;; "Объект является тендером, который давно стал активным"
           (:finished    nil) ;; "Объект является завершенным тендером"
           (:cancelled   nil) ;; "Объект является отмененным тендером"
           ;; Mixed
           (:self        (progn ;; TODO проверять соответствие типов
                           ;; (safe-write (path "perm-log.txt") (format nil "cur-user: ~A; cur-id ~A; ~%" (cur-user-id) (cur-id)))
                           (equal (cur-user-id) (cur-id))))          ;; "Объект олицетворяет пользователя, который совершает над ним действие"
           (:owner       nil) ;; "Объект, над которым совершается действие имеет поле owner содержащее ссылку на объект текущего пользователя"
           ))
        (t perm)))


(defclass DYMMY (entity)
  ((DYMMY :initarg :DYMMY :initform nil :accessor A-DYMMY)))


(defun check-perm (perm subj &optional (obj (make-instance 'DYMMY)))
  (let ((rs (eval (perm-check perm subj obj))))
    (safe-write (path "perm-log.txt") (format nil "perm: ~A; result: ~A; subj: ~A; obj: ~A~%" perm rs subj obj))
    (eval rs)
  ;; t
  ))


;; (check-perm '(or :admin :self) (gethash 2 *USER*))

;; TEST
;; (check-perm '(or :ADMIN :SELF) (gethash 0 *USER*) (gethash 1 *USER*))
;; (perm-check '(or :admin (and :all :nobody)) 1 2)
;; (check-perm '(or :admin (or :all :nobody)) 1 2)
;; (check-perm ':nobody 1 2)
;; (check-perm ':nobody 1 2)
