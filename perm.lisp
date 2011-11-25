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
  (cond ((consp     perm)
         (loop :for item :in perm :collect (perm-check item subj obj)))
        ((or (equal perm 'or)
             (equal perm 'and))
         perm)
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
           (:fair      (if (equal (type-of subj) 'SUPPLIER)           ;; Субьект (залогиненный пользователь) является добросовестным поставщиком
                           (if (equal (a-status subj) :fair)
                               t
                               nil)
                           nil))
           (:unfair    (if (equal (type-of subj) 'SUPPLIER)           ;; Субьект (залогиненный пользователь) является недобросовестным поставщиком
                           (if (equal (a-status subj) :unfair)
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
           (:self      (destructuring-bind (root obj-type id) ;; Залогиненный пользователь (subj) и просматриваемая страница (request-list)
                           (request-list)                     ;; указывают на один объект.
                         (if (and (not (null obj-type))       ;; Этот вид прав не должен использоваться с ajax-объектамми, например grid,
                                  (not (null id))             ;; так как requuest-list для них не указывает на объект!
                                  (equal id (format nil "~A" (parse-integer id :junk-allowed t)))
                                  (string= (string-upcase obj-type) (type-of (gethash (parse-integer id :junk-allowed t) *USER*)))
                                  (equal subj (gethash (parse-integer id :junk-allowed t) *USER*)))
                             t
                             nil)))
           (:owner     (if (and (slot-exists-p obj 'owner)    ;; Объект, над которым совершается действие имеет поле owner текущего пользователя
                                (equal subj (a-owner obj)))
                           t
                           nil))
                       ;; (destructuring-bind (root obj-type id)
                       ;;     (request-list)
                       ;;   (if (and (not (null obj-type))
                       ;;            (not (null id))
                       ;;            (equal id (format nil "~A" (parse-integer id :junk-allowed t))))
                       ;;       (let ((hash (string-upcase (format nil "*~A*"  obj-type)))
                       ;;             (target))
                       ;;         (if (or (string= hash "*SUPPLIER*")
                       ;;                 (string= hash "*BUILDER*")
                       ;;                 (string= hash "*ADMIN*")
                       ;;                 (string= hash "*EXPERT*"))
                       ;;             (setf hash "*USER*"))
                       ;;         (setf hash (intern hash :wizard))
                       ;;         (setf target (gethash (parse-integer id :junk-allowed t) (symbol-value hash)))
                       ;;         (if (and (slot-exists-p target 'owner)
                       ;;                  (equal (a-owner target) subj))
                       ;;             t
                       ;;             nil))))
           )
         )
        (t (error (format nil "error permissoin predicate: ~A" perm)))))


(defun check-perm (perm subj obj)
  (let ((rs (eval (perm-check perm subj obj))))
    (prog1 rs
      (safe-write (path "perm-log.txt")
                  (format nil "~A ~A | subj: ~A; obj: ~A~%"
                          (if rs "✔" "✘")
                          perm
                          subj
                          obj)))))


;; TEST
(perm-check '(or :logged (and :admin :supplier)) (gethash 0 *USER*) 'nine)
(check-perm '(or :logged (and :admin :supplier)) (gethash 0 *USER*) 'nine)
(check-perm :admin (gethash 1 *USER*) 'nine)
