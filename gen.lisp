(in-package #:WIZARD)

(defparameter *menu*               nil)
(defparameter *controllers*        nil)
(defparameter *ajaxdataset*        nil)
(defparameter *param-id-flag*      nil)


(defun gen-fld-symb (fld entity-param)
  (let* ((entity    (find-if #'(lambda (entity)  (equal (getf entity :entity) entity-param))  *entityes*))
         (record    (find-if #'(lambda (x)       (equal (car x) fld))                         (getf entity :fields)))
         (obj-perm  (getf entity :perm))
         (fld-perm  obj-perm)
         (width     200))
    (destructuring-bind (fld name typedata &rest rest)
        record
      (ecase (length rest)
        (0 nil)
        (1 (etypecase (car rest)
             (cons    (setf fld-perm (car rest)))
             (integer (setf width (car rest)))))
        (2 (progn
             (etypecase (car rest)
               (cons    (setf fld-perm (car rest)))
               (integer (setf width (car rest))))
             (etypecase (cadr rest)
               (cons    (setf fld-perm (cadr rest)))
               (integer (setf width (cadr rest)))))))
      (format nil "~%~25T (list :fld \"~A\" :typedata '~A :name \"~A\" :width ~A ~%~31T :permlist '~A)"
              fld
              (bprint typedata)
              name
              width
              (let ((res-perm))
                (loop :for perm :in obj-perm :by #'cddr :do
                   (if (null (getf fld-perm perm))
                       (setf (getf res-perm perm) (getf obj-perm perm))
                       (setf (getf res-perm perm) (getf fld-perm perm))))
                (bprint res-perm))))))


(defun gen-fld-cons (fld)
  (let ((instr (car fld)))
    (ecase instr
      (:btn         (cond ((not (null (getf fld :act)))    (let ((gen (string-downcase (symbol-name (gensym "B")))))
                                                             (setf *controllers*
                                                                   (append *controllers*
                                                                           (list (list gen (getf fld :act)))))
                                                             (format nil "~%~25T (list :btn \"~A\" :width ~A :perm ~A :value \"~A\")"
                                                                     gen
                                                                     (aif (getf fld :width) it "200")
                                                                     (bprint (getf fld :perm))
                                                                     (getf fld instr))))
                          ((not (null (getf fld :popup)))  (let* ((gen    (string-downcase (symbol-name (gensym "P")))))
                                                             (format nil "~%~25T (list :popbtn \"~A\" :width ~A :perm ~A :value \"~A\" ~%~31T :action ~A)"
                                                                     gen
                                                                     (aif (getf fld :width) it "200")
                                                                     (bprint (getf fld :perm))
                                                                     (getf fld instr)
                                                                     (gen-action (eval (getf fld :popup)))
                                                                     )))))

      (:action      (gen-action fld))
      (:file        (format nil "~%~25T (list :file \"~A\" :perm ~A :value \"~A\")"
                            (getf fld instr)
                            (bprint (getf fld :perm))
                            (getf fld :name))))))


(defun gen-fields (fields entity)
  ;; (format t "~%gen-fields : ~A : ~A" fields entity)
  (format nil "(list ~{~A~})"
          (loop :for fld :in fields :collect
             (etypecase fld
               (symbol  (gen-fld-symb fld entity))
               (cons    (gen-fld-cons fld))))))


(defun gen-action (action)
  ;; (format t "~%--------------------action: ~A" (getf action :action)) ;;
  (let ((pre-generated-fields (gen-fields (eval (getf action :fields)) (getf action :entity))))
    (format nil "~%~14T (list :action \"~A\" ~%~20T :showtype ~A ~%~20T :perm '~A ~A ~%~20T :val (named-lambda ~A () ~A)~% ~20T :fields ~A)"
            (getf action :action)
            (bprint (getf action :showtype))
            (bprint (getf action :perm))
            (if (not (equal :grid (getf action :showtype)))
                ""
                (let ((grid (string-downcase (symbol-name (gensym "JG")))))
                  (setf *ajaxdataset*
                        (append *ajaxdataset*
                                (list (list grid
                                            (format nil "(named-lambda ~A () ~A)"
                                                    (symbol-name (gensym "GRDNL-"))
                                                    (bprint (getf action :val)))
                                            pre-generated-fields
                                            *param-id-flag*))))
                  (format nil "~%~20T :grid \"~A\" ~%~20T :param-id ~A"
                          grid
                          *param-id-flag*)))
            (symbol-name (gensym "ACTNL-"))
            (bprint (getf action :val))
            pre-generated-fields)))


(with-open-file (out (path "defmodule.lisp") :direction :output :if-exists :supersede)
  ;; Required
  (format out "(in-package #:~A)"  (package-name *package*))
  ;; Containers
  (let ((containers))
    ;; Containers
    (loop :for entity :in *entityes* :do
       (let ((container (getf entity :container)))
         (unless (null container)
           (push container containers))))
    (setf containers (reverse (remove-duplicates containers)))
    (format out "~%~%;; Containers~%")
    (loop :for container :in containers :do
       (format out "~%~<(defvar *~A* ~43:T (make-hash-table :test #'equal))~:>"
               `(,container)))
    ;; Classes
    (format out "~%~%;; Classes")
    (loop :for entity :in *entityes* :do
       (format out "~%~%~%~<(defclass ~A (entity)~%(~{~A~^~%~}))~:>"
               `(,(getf entity :entity)
                  ,(loop :for field :in (getf entity :fields) :collect
                      (let ((fld (car field)))
                        (format nil "~<(~A ~23:T :initarg :~A ~53:T :initform nil :accessor ~A)~:>"
                                `(,fld ,fld ,(format nil "A-~A" fld)))))))))
  ;; Places
  (setf *menu* nil)
  (setf *ajaxdataset* nil)
  (loop :for place :in *places* :do
     ;; *param-id-flag*
     (setf *param-id-flag* (not (null (search "/:id" (getf place :url) :test #'equal))))
     ;; clear *controllers*
     (setf *controllers* nil)
     ;; menu
     (unless (null (getf place :navpoint))
       (push (list :link (getf place :url) :title (getf place :navpoint)) *menu*))
     ;; base route
     ;; (format t "~%--------::place::[~A]" (getf place :place)) ;;
     (format out "~%~%(restas:define-route ~A-page (\"~A\")"
             (string-downcase (getf place :place))
             (getf place :url))
     (format out "~A~%  (let ((session (hunchentoot:start-session))~%~7T (acts (list ~{~A~})))~%~5T(declare (ignore session))~%~5T(show-acts acts)))"
             (if *param-id-flag* (format nil "~%  (declare (ignore id))") "")
             (loop :for action :in (eval (getf place :actions)) :collect
                (gen-action action) ;; append *controllers* and *ajaxdataset* (!)
                ))
     ;; CONTROLLERS for this place
     ;; (unless (null *controllers*) ;; всегда нужно иметь контроллеры, т.к. есть логин на всех страницах
     (format out "~%~%(restas:define-route ~A/ctrs (\"~A\" :method :post)"
             (string-downcase (getf place :place))
             (getf place :url))
     (format out  "~A~%  (let ((session (hunchentoot:start-session))~%~7T (acts `(~{~%~A~}))) ~%~5T(declare (ignore session))~%~5T(activate acts)))"
             (if *param-id-flag* (format nil "~%  (declare (ignore id))") "")
             (loop :for (gen act) :in *controllers* :collect
                (format nil "(\"~A\" . ,(named-lambda ~A () ~A))"
                        gen
                        (symbol-name (gensym "CTRNL-"))
                        (bprint act)
                        ))))
  ;; AJAXDATASET for all places
  (unless (null *ajaxdataset*)
    (loop :for (grid val fields param-id) :in *ajaxdataset* :do
       (format out "~%~%(restas:define-route ~A/ajax (\"/~A~A\")"
               grid
               grid
               (if param-id "/:id" ""))
       (format out "~A~%  (example-json ~%~2T ~A ~%~2T ~A))"
               (if param-id (format nil "~%  (declare (ignore id))") "")
               val
               fields)))
  ;; out *menu*
  (format out "~%~%~%(defun menu ()  '")
  (pprint (reverse *menu*) out)
  (format out ")"))
