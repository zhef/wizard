;; (mapcar #'(lambda (x)
;;             (asdf:oos 'asdf:load-op x))
;;         '(:cl-mysql :cl-store :restas :restas-directory-publisher :cl-json :cl-ppcre :cl-smtp :cl-mime :arnesi :closer-mop :drakma))

(restas:define-module #:WIZARD
    (:use #:CL #:ITER #:cl-mysql #:alexandria #:anaphora #:ppcre))

(in-package #:WIZARD)


(defparameter *dbg* nil)
(defparameter *host* "http://gdestroytorg.ru")
(defparameter *mapkey*  "AKd6p04BAAAAN6JBTQIAVVk3tt2BoBL03SxnV1Q883Tx2N8AAAAAAAAAAAAg4NNZvAEKtUxUl-gPDH65Ud3jMA==")
(defparameter *db-password* "12")


(defun get-username (&aux (pid (sb-posix:getpid)))
  (sb-posix:passwd-name
   (sb-posix:getpwuid
    (sb-posix:stat-uid
     (sb-posix:stat (format nil "/proc/~A" pid))))))

(when (string= "rigidus" (get-username))
  (defparameter *dbg* t)
  (defparameter *host* "")
  (defparameter *mapkey*  "AKOwoE4BAAAAzn_UAAQAmXdybST_B2x-mnLcto5q_tTa2B4AAAAAAAAAAAAtC7dNu632YaEJuBnHz1d5g8a1IQ==")
  (defparameter *db-password* "root")
  (defparameter *map-disabled* t))


(let ((path '(:RELATIVE "wizard")))
  (setf asdf:*central-registry*
        (remove-duplicates (append asdf:*central-registry*
                                   (list (merge-pathnames
                                          (make-pathname :directory path)
                                          (user-homedir-pathname))))
                           :test #'equal)))

(defparameter *basedir*  (format nil "/home/~A/wizard/" (get-username)))

(defun path (relative)
  (merge-pathnames relative *basedir*))

(defun decode-date (timestamp)
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time timestamp)
    (declare (ignore second minute hour))
    (format nil
            "~2,'0d.~2,'0d.~d"
            date
            month
            year)))

(defun parse-date (datestring)
  "Date is YYYY-MM-DD, returns universal time."
  (unless (string-equal date "")
    (apply #'encode-universal-time
           (append '(0 0 0)
                   (nreverse (mapcar #'parse-integer
                                     (ppcre:split "-" date)))))))

(defmacro err (var)
  `(error (format nil "ERR:[~A]" (bprint ,var))))

(defun redirect (uri)
  (hunchentoot:redirect (format nil "~A~A" *host* uri)))

(defmacro to (format-str form-elt)
  `(redirect
    (format nil ,format-str (get-btn-key ,form-elt))))

(defmacro ent-to-mi (ent-list)
  "Translate action to object"
  `(let ((result (mi (intern (symbol-name (car ,ent-list))) :title (cadr ,ent-list))))
     (loop :for key :in (cddr ,ent-list) :by #'cddr :do
        (setf (slot-value result (intern (symbol-name key))) (getf ,ent-list key)))
     result))

(defmacro with-query-select ((query-str fields) &body body)
  (let* ((fld-str (format nil "~{`~A`~^, ~}" fields))
         (fld-sym (loop :for fld :in fields :collect (intern (string-upcase fld) (find-package "WIZARD")))))
    (with-gensyms (res row)
      `(let ((,res (caar (query (replace-all ,query-str "|:::|" ,fld-str)))))
         (aif ,res
              (loop :for ,row :in ,res :collect
                 (destructuring-bind ,fld-sym
                     ,row
                   ,@body))
              nil)))))

(defmacro cons-hash-list (hash)
  `(loop :for obj :being the :hash-values :in ,hash :using (hash-key key) :collect
      (cons key obj)))

(defmacro push-hash (hash class &body init)
  (with-gensyms (name-block result present id)
    `(block ,name-block
       (loop :for ,id :from 0 :do
          (multiple-value-bind (,result ,present)
              (gethash ,id ,hash)
            (unless ,present
              (return-from ,name-block
                (values
                 (setf (gethash ,id ,hash)
                       (make-instance ,class ,@init))
                 ,id))))))))

(defmacro cons-inner-objs (hash inner-lst)
  `(let ((inner-lst ,inner-lst)
         (cons-hash (cons-hash-list ,hash)))
     (loop :for obj :in inner-lst :collect
        (block in-ret
          (loop :for cons :in cons-hash :collect
             (when (equal (cdr cons) obj)
               (return-from in-ret cons)))
          nil))))

(defmacro form-fld (fld)
  `(cdr (assoc ,(symbol-name fld) (form-data) :test #'equal)))

(defmacro with-obj-create ((hash class flds) &body body)
  `(multiple-value-bind (obj id)
       (push-hash ,hash ,class)
     ,@(loop :for fld :in flds :collect
          `(setf (,(intern (format nil "A-~A" (symbol-name fld))) obj)
                 (form-fld ,fld)))
     ,@body))

(defmacro with-obj-save (obj &rest flds)
  `(progn
     ,@(loop :for fld :in flds :collect
          `(setf (,(intern (format nil "A-~A" (symbol-name fld))) ,obj)
                 (form-fld ,fld)))))

(defmacro del-inner-obj (form-element hash inner-lst)
  (with-gensyms (key hobj)
    `(let* ((,key  (get-btn-key ,form-element))
            (,hobj (gethash ,key ,hash)))
       (setf ,inner-lst
             (remove-if #'(lambda (x)
                            (equal x ,hobj))
                        ,inner-lst))
       (remhash ,key ,hash)
       (redirect (hunchentoot:request-uri*)))))

(defmacro add-inner-obj (hash class inner-lst &body inits)
  (with-gensyms (obj id)
    `(multiple-value-bind (,obj ,id)
         (push-hash ,hash ,class ,@inits)
       (append-link ,inner-lst ,obj)
       (values ,obj ,id))))


(defmacro append-link (lst elt)
  `(setf ,lst (append ,lst (list ,elt))))

(defmacro bprint (var)
  `(subseq (with-output-to-string (*standard-output*)  (pprint ,var)) 1))

(defun cur-user-id ()
  "get current logged user id as string"
  (hunchentoot:session-value 'userid))

(defun cur-user ()
  "get current logged user obj from session"
  (gethash (cur-user-id) *USER*))

(defun cur-page-id ()
  "get current viewed page obj"
  (parse-integer (car (last (request-list))) :junk-allowed t))

(defun form-data ()
  "get form data (get/post/ajax unification)"
  (hunchentoot:post-parameters*))

(defun request-str ()
  "multireturn request interpretations"
  (let* ((request-full-str (hunchentoot:request-uri hunchentoot:*request*))
         (request-parted-list (split-sequence:split-sequence #\? request-full-str))
         (request-str (string-right-trim "\/" (car request-parted-list)))
         (request-list (split-sequence:split-sequence #\/ request-str))
         (request-get-plist (if (null (cadr request-parted-list))
                                nil
                                ;; else
                                (let ((result))
                                  (loop :for param :in (split-sequence:split-sequence #\& (cadr request-parted-list)) :do
                                     (let ((split (split-sequence:split-sequence #\= param)))
                                       (setf (getf result (intern (string-upcase (car split)) :keyword))
                                             (if (null (cadr split))
                                                 ""
                                                 (cadr split)))))
                                  result))))
    (values request-str request-list request-get-plist)))

(defun request-get-plist ()
  ""
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    (declare (ignore request-str request-list))
    request-get-plist))

(defun request-list ()
  ""
  (multiple-value-bind (request-str request-list request-get-plist)
      (request-str)
    (declare (ignore request-str request-get-plist))
    request-list))


;; replace-all


(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
                         :start2 old-pos
                         :test test)
       do (write-string string out
                        :start old-pos
                        :end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))


;; form processing


(defun get-btn-key (btn)
  "separate for btn form data"
  (let* ((tilde (position #\~ btn))
         (id    (if tilde
                    (subseq btn (+ 1 tilde))
                    btn)))
    (parse-integer id)))


(defun activate (acts)
  "activation form processing"
  (when (assoc "AUTH" (form-data) :test #'equal)
    (return-from activate
      (auth (cdr (assoc "LOGIN" (form-data) :test #'equal))
            (cdr (assoc "PASSWORD" (form-data) :test #'equal)))))
  (when (assoc "LOGOUT" (form-data) :test #'equal)
    (return-from activate
      (progn
        (hunchentoot:delete-session-value 'userid)
        (redirect (hunchentoot:request-uri*)))))
  (with-output-to-string (*standard-output*)
    (format t "form-data: ")
    (print (form-data))
    (format t "<br />~%acts:")
    (loop :for key :in acts :do
       (print (car key))
       (when (assoc (car key)
                    (form-data)
                    :test #'(lambda (a b)
                              (flet ((tld (x)
                                       (let ((tilde (position #\~ x)))
                                         (if tilde
                                             (subseq x 0 tilde)
                                             x))))
                                (funcall #'equal (tld a) (tld b)))))
         (return-from activate (funcall (cdr key)))))
    "err: unk:post:controller"))


;; xls-decoder


(defun decoder-3-csv  (in-string)
  "Второе возвращаемое значение показывает, была ли закрыта кавычка, или строка
   закончилась посередине обрабатываемой ячейки, что указывает на разрыв строки"
  (let ((err))
    (values
     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab) y))
             (mapcar #'(lambda (y) (ppcre:regex-replace-all "\\s+" y " "))
                     (mapcar #'(lambda (y) (string-trim '(#\Space #\Tab #\") y))
                             (let ((inp) (sav) (acc) (res))
                               (loop :for cur :across in-string do
                                  ;; (print cur)
                                  (if (null inp)
                                      (cond ((equal #\" cur) (progn (setf inp t)
                                                                    ;; (print "open quote : inp t")
                                                                    ))
                                            ((equal #\, cur)  (progn (push "" res)
                                                                     ;; (print "next")
                                                                     ))
                                            ;; (t (print "unknown sign out of quite"))
                                            )
                                      ;; else
                                      (cond ((and (null sav) (equal #\" cur)) (progn (setf sav t)
                                                                                     ;; (print "close quote : sav t")
                                                                                     ))
                                            ((and sav (equal #\" cur)) (progn (setf sav nil)
                                                                              ;; (print (list ".." #\"))
                                                                              (push #\" acc)))
                                            ((and sav (equal #\, cur)) (progn (setf sav nil)
                                                                              (setf inp nil)
                                                                              (push (coerce (reverse acc) 'string) res)
                                                                              ;; (print "inp f")
                                                                              (setf acc nil)))
                                            ((equal #\Return cur)      nil)
                                            (t (progn (push cur acc)
                                                      ;; (print (list "." cur))
                                                      )))))
                               (when acc
                                 ;; незакрытая кавычка
                                 (if (and inp (null sav))
                                     (setf err t))
                                 ;; (print (list ":" inp sav acc res))
                                 (push (coerce (reverse acc) 'string) res))
                               (reverse res)))))
     err)))


(defun xls-processor (infile)
  (let* ((result)
         (output (with-output-to-string (*standard-output*)
                   (let* ((proc (sb-ext:run-program "/usr/bin/xls2csv"
                                                    (list "-q3" (format nil "~a" infile)) :wait nil :output :stream)))
                     (with-open-stream (in (sb-ext:process-output proc))
                       (loop :for i from 1 do
                          (tagbody loop-body
                             (handler-case
                                 (let ((in-string (read-line in)))
                                   (format nil "~A" in-string)
                                   ;; начинаем декодировать
                                   (tagbody start-decoding
                                      (multiple-value-bind (line err-string-flag)
                                          (decoder-3-csv in-string)
                                        (when err-string-flag
                                          (setf in-string (concatenate 'string in-string (read-line in)))
                                          ;; (format t "~%warn-broken-string [~a] ~a~%" i in-string)
                                          (incf i)
                                          (go start-decoding))
                                        (format t "~%~%str: ~a~%lin: ~a" in-string (bprint line))
                                        (unless (null line)
                                          (handler-case
                                              (push line result)
                                            (SB-INT:SIMPLE-PARSE-ERROR () nil))
                                          )))
                                   )
                               (END-OF-FILE () (return i)))))))
                   )))
    (declare (ignore output))
    ;; output
    (reverse result)))
;; (xls-processor "/home/rigidus/xls.xls")


;; eval-always


(defmacro eval-always (&body body)
  "Wrap <_:arg body /> in <_:fun eval-when /> with all keys \(compile, load and execute) mentioned"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ,@body))


;; abbrev


(defmacro abbrev (short long)
  "Abbreviate a <_:arg long /> MACRO or FUNCTION name as <_:arg short />"
  `(eval-always
     (cond
       ((macro-function ',long)       (setf (macro-function ',short) (macro-function ',long)))
       ((special-operator-p ',long)   (error "Can't ABBREViate a special-operator ~a" ',long))
       ((fboundp ',long)              (setf (fdefinition ',short) (fdefinition ',long)))
       (t                             (error "Can't ABBREViate ~a" ',long)))
     (setf (documentation ',short 'function) (documentation ',long 'function))
     ',short))

(abbrev mi make-instance)


;; #` syntax


(eval-always
  (defun |#`-reader| (stream char arg)
    "Literal syntax for zero/one/two argument lambdas.
Use @ as the function's argument, % as the second.
Examples:
CL-USER> #`(+ 2 @)
\(lambda (&optional x y)
   (+ 2 x))
CL-USER>  #`((1+ @) (print @))
\(lambda (&optional x y)
   (1+ x)
   (print x))
CL-USER> #`(+ 1 2)
\(lambda (&optional x y)
   (+ 1 2))
CL-USER>  #`(+ @ %)
\(lambda (&optional x y)
   (+ x y))
"
    (declare (ignore char arg))
    (let ((sexp (read stream t nil t))
          (x (gensym "X"))
          (y (gensym "Y")))
      `(lambda (&optional ,x ,y)
         (declare (ignorable ,x)
                  (ignorable ,y))
         ,@(subst y '%
                  (subst x '@
                         (if (listp (car sexp))
                             sexp
                             (list sexp)))))))
  ;; set #`
  (set-dispatch-macro-character #\# #\` #'|#`-reader|))


;; anaphoric


(eval-always
  (defmacro if-it (test then &optional else)
    "Like IF. IT is bound to TEST."
    `(let ((it ,test))
       (if it ,then ,else))))

(eval-always
  (defmacro when-it (test &body body)
    "Like WHEN. IT is bound to TEST."
    `(let ((it ,test))
       (when it
         ,@body))))

(eval-always
  (defmacro and-it (&rest args)
    "Like AND. IT is bound to the value of the previous AND form."
    (cond ((null args) t)
          ((null (cdr args)) (car args))
          (t `(when-it ,(car args) (and-it ,@(cdr args)))))))

(eval-always
  (defmacro dowhile-it (test &body body)
    "Like DOWHILE. IT is bound to TEST."
    `(do ((it ,test ,test))
         ((not it))
       ,@body)))

(eval-always
  (defmacro cond-it (&body body)
    "Like COND. IT is bound to the passed COND test."
    `(let (it)
       (cond
         ,@(mapcar #``((setf it ,(car @)) ,(cadr @))
                   ;; uses the fact, that SETF returns the value set
                   body)))))


;; maybe


(defmacro maybecall (val &rest funs)
  `(and-it ,val
           ,@(mapcar (lambda (fun)
                       `(funcall ,fun it))
                     funs)))


(defmacro maybe (form)
  "Return a value, returned by a <_:arg form /> or nil, if <_:class error /> is signalled"
  `(restart-case
       (handler-bind ((error #'(lambda (c)
                                 (declare (ignore condition))
                                 (invoke-restart 'skip))))
         ,form)
     (skip () nil)))


;; dotree


(defmacro dotree ((var tree-form &optional result-form) &body body)
  "The analog of <_:fun dolist />, operating on trees"
  (with-unique-names (traverser list list-element)
    `(progn
       (labels ((,traverser (,list)
                  (dolist (,list-element ,list)
                    (if (consp ,list-element)
                        (,traverser ,list-element)
                        (let ((,var ,list-element))
                          ,@body)))))
         (,traverser ,tree-form)
         ,result-form))))


;; obj-equal


(defgeneric obj-equal-by-slots (obj1 obj2 &optional test)
  (:documentation "Slot-by-slot comparison of objects of one class.
If objs are of different classes the result is NIL.
Obviously, can be specialized on classes, and even for two objects of
diffenet classes, why not...")
  (:method (obj1 obj2 &optional (test 'equal))
    (let ((cls (class-of obj1)))
      (when (eq cls (class-of obj2))
        (handler-case
            (apply #'every test
                   (mapcar (lambda (obj)
                             (mapcar (lambda (slot)
                                       (slot-value
                                        obj
                                        (closer-mop:slot-definition-name slot)))
                                     (closer-mop:class-slots (class-of obj))))
                           (list obj1 obj2)))
          (unbound-slot () nil))))))


(defgeneric obj-equal (obj1 obj2 &optional test)
  (:documentation
   "Class-aware polymorphic equality with an optional <_:arg test />")
  (:method (obj1 obj2 &optional (test 'equal))
    "For non CLOS primitive types"
    (funcall test obj1 obj2))
  (:method ((obj1 sequence) (obj2 sequence) &optional (test 'equal))
    "For sequences -- recursively look inside the ordered variants"
    (when (= (length obj1) (length obj2))
      (every (lambda (obj1 obj2)
               (obj-equal obj1 obj2 test))
             obj1 obj2))) ; add lexicographical ordering
  (:method ((obj1 standard-class) (obj2 standard-class) &optional (test 'equal))
    "Slot-by-slot comparison for STANDARD-CLASSES.
If objs are of different classes the result is NIL."
    (obj-equal-by-slots obj1 obj2 test)))


;; send-email


(defun send-email (text &rest reciepients)
  "Generic send SMTP mail with some TEXT to RECIEPIENTS"
  (cl-smtp:with-smtp-mail (out "localhost" "noreply@fin-ack.com" reciepients)
    (cl-mime:print-mime out
                        (make-instance 'cl-mime:text-mime
                                       :encoding :base64 :charset "UTF-8"
                                       :content (arnesi:string-to-octets text :utf-8))
                        t t)))

;; safe-write


(defparameter *safe-write-sleep* 0.01)
(defun safe-write (pathname string &aux stream)
  (setf stream (open pathname :direction :output :if-does-not-exist :create :if-exists :append))
  (unwind-protect
       (loop
          until (block try-lock
                  (handler-bind ((error (lambda (condition)
                                          (if (= sb-posix:eagain
                                                 (sb-posix:syscall-errno condition))
                                              (return-from try-lock)
                                              (error condition)))))
                    (sb-posix:lockf stream sb-posix:f-tlock 0)
                    (princ string stream)
                    (close stream)))
          do (sleep *safe-write-sleep*))
    (close stream)))


;; extract


(defun extract (regex in-string)
  (multiple-value-bind (start end)
      (scan (create-scanner regex) in-string)
    (if (or (null start)
            (null end))
        nil
        (subseq in-string start end))))


;; geo-coder

(setf drakma:*drakma-default-external-format* :utf-8)

(defun geo-coder (addr)
  (if (and (boundp '*map-disabled*) *dbg*)
      (return-from geo-coder 0))
  (multiple-value-bind (body status headers ret-uri stream must-close reason)
      (drakma:http-request "http://geocode-maps.yandex.ru/1.x/"
                           :method :get
                           :parameters `(("geocode" . ,addr)
                                         ("key" . ,(format nil "~A" *mapkey*))
                                         ("format" . "json")))
    (declare (ignore status headers ret-uri stream must-close reason))
    (let ((tree (json:decode-json-from-string
                 (sb-ext:octets-to-string body))))
      ;; (print tree)
      (if-it (maybecall tree #'car #'cdr #'car #'cdr #'cdr #'car #'cdr #'caar #'cdr #'fourth #'cdr #'car #'cdr)
             (format nil "~{~A~^, ~}" (split-sequence:split-sequence #\Space it))
             nil))))


;; with-defclass

(defmacro with-defclass ((class-name super-class-names) &body slots)
  `(prog1
       (defclass ,class-name ,super-class-names
         ,(loop :for (slot-name initform) :in slots :collect
             `(,slot-name :initarg  ,(intern (symbol-name slot-name) :keyword)
                          :initform ,initform
                          :accessor ,(intern (format nil "A-~A" (symbol-name slot-name))))))
     ;; (defmethod print-object ((obj ,class-name) stream)
     ;;   (format stream
     ;;           (format nil "#[ ~A | ~A]"
     ;;                   ',class-name
     ;;                   (loop :for slot :in (closer-mop:class-slots (find-class ',class-name)) :collect
     ;;                      (format nil ":~A ~A"
     ;;                              (closer-mop:slot-definition-name slot)
     ;;                              (bprint (funcall (intern (format nil "A-~A" (symbol-name (closer-mop:slot-definition-name slot)))) obj)))))))
     ))


(with-defclass (yapoint ())
  (title "")
  (link  "")
  (descr "")
  (coord ""))

;; CLASS PERM
(with-defclass (perm ())
  (update :all)
  (show   :all)
  (view   :all))


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
  :txt                  "textarea"
  :link                 "связанная сущность (модификатор: тип сущности)"
  :list-of-links        "список связанных сущностей"
  :list-of-keys         "выпадающий список ключей, с выбором одного из них"
  :text                 "текстовое поле"
  :date                 "дата и время"
  :interval             "диапазоны дат, относящиеся к тендеру"
  :img                  "изображения, картинки")

;; TODO: при создании экземпляра entity проверять тип поля
(defparameter *types* (loop :for ftype :in *fld-types* :by #'cddr :collect ftype))



(defun posts-by-section (section count)
  (list (mapcar #'(lambda (x)
                    (let ((elt (cdr x)))
                      (list :date (a-date elt)
                            :title (a-title elt)
                            :announce (a-announce elt)
                            :xref (format nil "/post/~A" (car x)))))
                (last (remove-if-not #'(lambda (x)
                                         (equal section (a-section (cdr x))))
                                     (reverse (cons-hash-list *POST-ITEM*))) count))))

(defun posts-by-sales (count)
  (list (mapcar #'(lambda (x)
                    (let ((elt (cdr x)))
                      (list :date (a-date elt)
                            :announce (a-announce elt)
                            :title (a-title elt)
                            :xref (format nil "/sale/~A" (car x)))))
                (last (cons-hash-list *SALE*) count))))


(defun re-tpl ()
  (closure-template:compile-template :common-lisp-backend #P"tpl/root.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/right.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/templates.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/main.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/about.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/contacts.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/services.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/help.htm")
  (closure-template:compile-template :common-lisp-backend #P"tpl/expert.htm"))

(defun re-path ()
  (restas:mount-submodule -css- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("css"))
    (restas.directory-publisher:*directory* (path "css/")))
  (restas:mount-submodule -js- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("js"))
    (restas.directory-publisher:*directory* (path "js/")))
  (restas:mount-submodule -img- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("img"))
    (restas.directory-publisher:*directory* (path "img/")))
  (restas:mount-submodule -img-popups- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("img/popups"))
    (restas.directory-publisher:*directory* (path "img/popups/")))
  (restas:mount-submodule -img-jqgrid- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("img/jqgrid"))
    (restas.directory-publisher:*directory* (path "img/jqgrid/")))
  (restas:mount-submodule -img-techno- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("pic_tech"))
    (restas.directory-publisher:*directory* (path "techno/pic_tech/")))
  (restas:mount-submodule -img-news- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("pic_news"))
    (restas.directory-publisher:*directory* (path "news/pic_news/")))
  (restas:mount-submodule -img-ivent- (#:restas.directory-publisher)
    (restas.directory-publisher:*baseurl* '("pic_ivent"))
    (restas.directory-publisher:*directory* (path "ivent/pic_ivent/"))))

(defun clear ()
  (loop :for var :being :the :symbols :in :wizard.impl.routes :do (unintern var))
  (restas:reconnect-all-routes))


(re-tpl)
(re-path)

(defun re-load ()
  (load "lib.lisp")
  (load "ent.lisp")
  (load "places.lisp")
  (load "gen.lisp")
  (load "grid.lisp")
  (load "perm.lisp")
  (load "defmodule.lisp")
  (load "render.lisp")
  (re-path))


(defun passwd ()
  (with-open-file (file-stream "passwd.txt" :direction :output :if-exists :supersede)
    (maphash #'(lambda (k v)
                 (declare (ignore k))
                 (unless (equal 'admin(type-of v))
                   (format file-stream "~%\"~A\" ~30t : ~A | ~A - ~A"
                           (a-name v)
                           (a-login v)
                           (a-password v)
                           (type-of v))))
             *USER*)))

;; (passwd)


