(in-package #:WIZARD)


(defun gen-fld-symb (fld entity-param)
  (let* ((entity    (find-if #'(lambda (entity)     (equal (getf entity :entity) entity-param))    *entityes*))
         (name      (cadr   (find-if #'(lambda (x)   (equal (car x) fld))                           (getf entity :fields))))
         (typedata  (caddr  (find-if #'(lambda (x)   (equal (car x) fld))                           (getf entity :fields))))
         (fld-perm  (cadddr (find-if #'(lambda (x)   (equal (car x) fld))                           (getf entity :fields))))
         (obj-perm  (getf entity :perm)))
    (format nil "~%~25T (list :fld \"~A\" :typedata '~A :name \"~A\" ~%~31T :permlist '~A)"
            fld
            (subseq (with-output-to-string (*standard-output*)  (pprint typedata)) 1)
            name
            (let ((res-perm))
              (loop :for perm :in obj-perm :by #'cddr :do
                 (if (null (getf fld-perm perm))
                     (setf (getf res-perm perm) (getf obj-perm perm))
                     (setf (getf res-perm perm) (getf fld-perm perm))))
              (bprint res-perm))
            ;;      (if (null (getf fld-perm perm))
            ;;          (setf (getf res-perm perm) (getf obj-perm perm))
            ;;          (setf (getf res-perm perm) (getf fld-perm perm)))
            ;;      (bprint obj-perm)))
            )))


(defun gen-fld-cons (fld)
  (let ((instr (car fld)))
    (ecase instr
      (:calc
       (values
        (format nil "~%~25T (list :calc (lambda (obj) ~A) :perm 111)"
                (subseq (with-output-to-string (*standard-output*) (pprint (getf fld instr))) 1))
        nil))
      (:btn
       (cond ((not (null (getf fld :act)))
              (let ((gen (gensym "B")))
                (values
                 (format nil "~%~25T (list :btn \"~A\" :permlist ~A :value \"~A\")"
                         gen
                         (bprint (getf fld :permlist))
                         (getf fld instr))
                 (list (list gen (getf fld :act))))))
             ((not (null (getf fld :popup)))
              (let ((popup (eval (getf fld :popup)))
                    (gen   (gensym "P")))
                (multiple-value-bind (str ctrs)
                    (gen-fields (eval (getf popup :fields))
                                (getf popup :entity))
                  ;; (format t "~%---| ~A" ctrs)
                  (values
                   (format nil "~%~25T (list :popbtn \"~A\" ~%~31T :value \"~A\" ~%~31T :perm 111 ~%~31T :title \"~A\" ~%~31T :fields ~A)"
                           gen
                           (getf fld instr)
                           (getf popup :caption)
                           str)
                   ctrs))))))
      (:col
       (multiple-value-bind (str ctrs)
           (gen-fields (eval (getf fld :fields))
                       (getf fld :entity))
         (values
          (format nil "~%~25T (list :col \"~A\" :perm 111 ~%~32T:val (lambda () ~A)~%~32T:fields ~A)"
                  (getf fld instr)
                  (getf fld :val)
                  str)
          ctrs))))))


(defun gen-fields (fields entity)
  ;; (format t "~%gen-fields : ~A : ~A : ~A" fields entity show))
  (let ((controllers))
    (values
     (format nil "(list ~{~A~})"
             (loop :for fld :in fields :collect
                (etypecase fld
                  (symbol  (gen-fld-symb fld entity))
                  (cons    (multiple-value-bind (str ctrs)
                               (gen-fld-cons fld)
                             (loop :for ctr :in ctrs :do
                                (setf controllers (append controllers (list ctr))))
                             str)))))
     controllers)))


(defun gen-action (action)
  (let* ((controllers)
         (ajaxdataset)
         (grid     (when (equal (getf action :showtype) :grid)
                     (string-downcase (symbol-name (gensym "JG")))))
         (perm     (bprint (getf action :perm)))
         (caption  (getf action :caption))
         (val      (bprint (getf action :val)))
         (entity   (getf action :entity))
         (fields   (eval (getf action :fields))))
    (multiple-value-bind (str ctrs)
        (gen-fields fields entity)
      (setf controllers (append controllers (list ctrs)))
      ;; #:GRID
      (when grid
        (setf ajaxdataset (list grid
                                (format nil "(lambda () ~A)" val)
                                str)))
      ;; RET
      (values
       (format nil "~%~14T (list :showtype ~A ~%~20T :perm '~A ~A ~%~20T :title \"~A\"~% ~20T :val (lambda () ~A)~% ~20T :fields ~A)"
               (bprint (getf action :showtype))
               perm
               (if (null grid) "" (format nil "~%~20T :grid \"~A\"" grid))
               caption val str)
       controllers
       ajaxdataset))))


(with-open-file (out (path "defmodule.lisp") :direction :output :if-exists :supersede)
  ;; Required
  (format out "(in-package #:~A)"  (package-name *package*))
  ;; Containers
  (let ((containers)
        (classes (make-hash-table :test #'equal)))
    ;; Containers
    (loop :for entity :in *entityes* :do
       (let ((container (getf entity :container)))
         (unless (null container)
           (push container containers))))
    (setf containers (reverse (remove-duplicates containers)))
    (format out "~%~%;; Containers~%")
    (loop :for container :in containers :do
       (format out "~%~<(defparameter *~A* ~43:T (make-hash-table :test #'equal))~:>"
               `(,container)))
    ;; Classes
    (format out "~%~%;; Classes")
    (loop :for entity :in *entityes* :do
       (let ((super (getf entity :super)))
         (format out "~%~%~%~<(defclass ~A (entity)~%(~{~A~^~%~}))~:>"
                 `(,(getf entity :entity)
                    ,(loop :for field :in (getf entity :fields) :collect
                        (let ((fld (car field))
                              (tpi (car (nth 2 field))))
                          (format nil "~<(~A ~23:T :initarg :~A ~53:T :initform nil :accessor ~A)~:>"
                                  `(,fld ,fld ,(format nil "A-~A" fld))))))))
       ;; (let ((perm (getf entity :perm)))
       ;;   (unless (null (getf perm :create))
       ;;     (format out "~%~%(defmethod initialize-instance :after ((object ~A) &key)"
       ;;             (getf entity :entity))
       ;;     (format out "~%  ;; Здесь будет проверка прав~%  ;; ...~%  ;; Запись в контейнер")
       ;;     (format out "~%  (setf (gethash (hash-table-count *~A*) *~A*) object)"
       ;;             (getf entity :container)
       ;;             (getf entity :container))
       ;;     (format out ")"))
       ;;   (unless (null (getf perm :view))
       ;;     (format out "~%~%(defmethod view ((object ~A) &key)"
       ;;             (getf entity :entity))
       ;;     (format out "~%  ;; Здесь будет проверка прав~%  ;; ...~%  ;; Печать")
       ;;     (let ((fields (getf entity :fields)))
       ;;       (loop :for fld :in fields :collect
       ;;          (let ((caption (cadr fld))
       ;;                (name    (car fld)))
       ;;            (format out "~%  (format t \"~A~A : ~A\" (~A object))" "~%" caption "~A" name))))
       ;;     (format out ")")))
       ))
  ;; Places
  (let ((menu))
    (loop :for place :in *places* :do
       (unless (null (getf place :navpoint))
         (push (list :link (getf place :url) :title (getf place :navpoint)) menu))
       (let ((controllers)
             (ajaxdataset))
         (format t "~%--------::place::[~A]" (getf place :place)) ;;
         (format out "~%~%(restas:define-route ~A-page (\"~A\")"
                 (string-downcase (getf place :place))
                 (getf place :url))
         (format out "~%  (let ((session (hunchentoot:start-session))~%~7T (acts (list ~{~A~}))) ~A)"
                 (loop :for action :in (eval (getf place :actions)) :collect
                    (progn
                      ;; (format t "~%--------------------caption: ~A" (getf action :caption)) ;;
                      (multiple-value-bind (str ctrs ajax)
                        (gen-action action) ;; <-------------------
                      (loop :for ctr :in ctrs :do
                         (setf controllers (append controllers ctr)))
                      ;; (print ajax) ;;
                      (unless (null ajax)
                        (setf ajaxdataset (append ajaxdataset (list ajax))))
                      str)))
                 (format nil  "~%    (show-acts acts))"))
         (unless (null controllers) ;; CONTROLLERS
           ;; (loop :for ctr :in controllers :do                  ;;
           ;;    (format t "~%~A | ~A" (car ctr) (cadr ctr)))     ;;
           (format out "~%~%(restas:define-route ~A-page/post (\"~A\" :method :post)"
                   (string-downcase (getf place :place))
                   (getf place :url))
           (format out  "~%  (let ((session (hunchentoot:start-session))~%~7T (acts `(~{~%~A~}))) ~%       (activate acts)))"
                   (loop :for controller :in controllers :collect
                      (format nil "(\"~A\" . ,(lambda () ~A))"
                              (car controller)
                              (bprint (cadr controller))
                              ))))
         ;; (format t "~%~A" (bprint ajaxdataset))                   ;;
         (unless (null ajaxdataset) ;; AJAXDATASET
           (loop :for ajax :in ajaxdataset :do
                (format out "~%~%(restas:define-route ~A-page/ajax (\"/~A\")"
                        (string-downcase (getf place :place))
                        (string-downcase (nth 0 ajax)))
                (format out "~%  (example-json ~%~2T ~A ~%~2T ~A))"
                        (nth 1 ajax)
                        (nth 2 ajax))
           ))
         ))
    (format out "~%~%~%(defun menu ()  '")
    (pprint (reverse menu) out)
    (format out ")")))
