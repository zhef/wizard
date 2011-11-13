(in-package #:WIZARD)

(defparameter *menu*               nil)
(defparameter *controllers*        nil)
(defparameter *ajaxdataset*        nil)
(defparameter *param-id-flag*      nil)

(defmacro ent-to-mi (ent-list)
  "Translate action to object"
  `(let ((result (mi (intern (symbol-name (car ,ent-list))) :title (cadr ,ent-list))))
     (loop :for key :in (cddr ,ent-list) :by #'cddr :do
        (setf (slot-value result (intern (symbol-name key))) (getf ,ent-list key)))
     result))

;; (defmethod gen ((fld list) &key entity-param)
;;   (let ((instr (car fld)))
;;     (ecase instr
;;       (:fld         (let* ((oldfld fld)
;;                            (entity    (find-if #'(lambda (entity)  (equal (getf entity :entity) entity-param))  *entityes*))
;;                            (record    (find-if #'(lambda (x)       (equal (car x) (cadr fld)))       (getf entity :fields)))
;;                            (obj-perm  (getf entity :perm))
;;                            (fld-perm  obj-perm)
;;                            (width     200))
;;                       (destructuring-bind (fld name typedata &rest rest)
;;                           record
;;                         (ecase (length rest)
;;                           (0 nil)
;;                           (1 (etypecase (car rest)
;;                                (cons    (setf fld-perm (car rest)))
;;                                (integer (setf width (car rest)))))
;;                           (2 (progn
;;                                (etypecase (car rest)
;;                                  (cons    (setf fld-perm (car rest)))
;;                                  (integer (setf width (car rest))))
;;                                (etypecase (cadr rest)
;;                                  (cons    (setf fld-perm (cadr rest)))
;;                                  (integer (setf width (cadr rest)))))))
;;                         (format nil "~%~25T (mi 'fld :name \"~A\" :typedata '~A :title \"~A\" :width ~A :xref ~A ~%~31T :perm ~A)"
;;                                 fld
;;                                 (bprint typedata)
;;                                 name
;;                                 width
;;                                 (bprint (getf oldfld :xref))
;;                                 (let ((res-perm))
;;                                   (loop :for perm :in obj-perm :by #'cddr :do
;;                                      (if (null (getf fld-perm perm))
;;                                          (setf (getf res-perm perm) (getf obj-perm perm))
;;                                          (setf (getf res-perm perm) (getf fld-perm perm))))
;;                                   (push ''perm res-perm)
;;                                   (push 'mi res-perm)
;;                                   (bprint res-perm))))))
;;       (:btn         (cond ((not (null (getf fld :act)))
;;                            (let ((genid (string-downcase (symbol-name (gensym "B")))))
;;                              ;; add controller
;;                              (setf *controllers*
;;                                    (append *controllers*
;;                                            (list (list genid (getf fld :act)))))
;;                              ;; output
;;                              (format nil "~%~25T (mi 'btn :name \"~A\" :width ~A :perm ~A :value \"~A\")"
;;                                      genid
;;                                      (aif (getf fld :width) it "200")
;;                                      (bprint (getf fld :perm))
;;                                      (getf fld instr))))
;;                           ;; popbtn if :popup exists
;;                           ((not (null (getf fld :popup)))
;;                            (let* ((genid (string-downcase (symbol-name (gensym "P")))))
;;                              ;; output
;;                              (format nil "~%~25T (mi 'popbtn :name \"~A\" :width ~A :perm ~A :value \"~A\" ~%~31T :action ~A)"
;;                                      genid
;;                                      (aif (getf fld :width) it "200")
;;                                      (bprint (getf fld :perm))
;;                                      (getf fld instr)
;;                                      (let ((action (eval (getf fld :popup))))
;;                                        (gen (ent-to-mi action))))))))
;;       (:calc        (format nil "~%~25T (list :calc \"~A\" :perm ~A :width ~A :func #'~A)"
;;                             (getf fld :calc)
;;                             (bprint (getf fld :perm))
;;                             (bprint (getf fld :width))
;;                             (bprint (getf fld :func))))
;;       (:action      (let ((action fld))
;;                       (gen (ent-to-mi ACTION))))
;;       (:file        (format nil "~%~25T (mi 'file :name \"~A\" :perm ~A :value \"~A\")"
;;                             (getf fld instr)
;;                             (bprint (getf fld :perm))
;;                             (getf fld :name))))))


;; (defmethod gen ((action action) &key)
;;   (let ((pre-generated-fields (format nil "(list ~{~A~})"
;;                                       (loop :for fld :in (eval (a-fields action)) :collect
;;                                          (gen fld :entity-param (a-entity action))))))
;;     (format nil "~%~14T (mi 'action :title \"~A\" ~%~20T :showtype ~A ~%~20T :perm '~A ~A ~%~20T :val (named-lambda ~A () ~A)~% ~20T :fields ~A)"
;;             (a-title action)
;;             (bprint (a-showtype action))
;;             (bprint (a-perm action))
;;             (if (not (equal :grid (a-showtype action)))
;;                 ""
;;                 (let ((grid (string-downcase (symbol-name (gensym "JG")))))
;;                   (setf *ajaxdataset*
;;                         (append *ajaxdataset*
;;                                 (list (list grid
;;                                             (format nil "(named-lambda ~A () ~A)"
;;                                                     (symbol-name (gensym "GRDNL-"))
;;                                                     (bprint (a-val action)))
;;                                             pre-generated-fields
;;                                             *param-id-flag*))))
;;                   (format nil "~%~20T :grid \"~A\" ~%~20T :param-id ~A :height \"~A\""
;;                           grid
;;                           *param-id-flag*
;;                           (a-height action))))
;;             (symbol-name (gensym "ACTNL-"))
;;             (bprint (a-val action))
;;             pre-generated-fields)))


(defmethod gen ((param none) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%~14T (mi 'none :title ~A :perm ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))))


(defmethod gen ((param yamap) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%~14T (mi 'yamap :title ~A :perm ~A :center-coord \"30.313622, 59.937720\" ~%~18T :mark-points ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(mapcar #'(lambda (x)
                                        (mi 'yapoint
                                            :title (nth 0 x)
                                            :descr (nth 1 x)
                                            :coord (geo-coder (nth 2 x))))
                                 ~A)"
                  (bprint (a-val param)))))


(defmethod gen ((param tpl) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%~14T (mi 'tpl :title ~A :perm ~A ~%~18T :val ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "GRDNL-"))
                  (bprint (a-val param)))))


(defmethod gen ((param linear) &key entity-param)
  (format nil "~%~14T (mi 'linear :title ~A ~%~20T :perm '~A ~%~20T :val ~A ~%~20T :fields ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "ACTNL-"))
                  (bprint (a-val param)))
          (format nil "(list ~{~A~})"
                  (loop :for fld :in (eval (a-fields param)) :collect
                     (gen fld :entity-param (a-entity param))))))


(defmethod gen ((param grid) &key entity-param)
  (let* ((pre-generated-fields (format nil "(list ~{~A~})"
                                       (loop :for fld :in (eval (a-fields param)) :collect
                                          (gen fld :entity-param (a-entity param)))))
         (grid (string-downcase (symbol-name (gensym "JG")))))
    (setf *ajaxdataset*
          (append *ajaxdataset*
                  (list (list grid
                              (format nil "(named-lambda ~A () ~A)"
                                      (symbol-name (gensym "GRDNL-"))
                                      (bprint (a-val param)))
                              pre-generated-fields
                              *param-id-flag*))))
    (format nil "~%~14T (mi 'grid :title ~A ~%~20T :perm '~A ~%~20T :grid ~A ~%~20T :param-id ~A ~%~20T :height ~A ~%~20T :val ~A ~%~20T :fields ~A)"
            (bprint (a-title param))
            (bprint (a-perm param))
            (bprint grid)
            (bprint *param-id-flag*)
            (bprint (a-height param))
            (format nil "(named-lambda ~A () ~A)"
                    (symbol-name (gensym "ACTNL-"))
                    (bprint (a-val param)))
            pre-generated-fields)))


(defmethod gen ((param announce) &key entity-param)
  (format nil "~%~14T (mi 'announce :title ~A ~%~20T :perm '~A ~%~20T :val ~A ~%~20T :fields ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "ACTNL-"))
                  (bprint (a-val param)))
          (format nil "(list ~{~A~})"
                  (loop :for fld :in (eval (a-fields param)) :collect
                     (gen fld :entity-param (a-entity param))))))


(defmethod gen ((param post) &key entity-param)
  (format nil "~%~14T (mi 'post :title ~A ~%~20T :perm '~A ~%~20T :val ~A ~%~20T :fields ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "ACTNL-"))
                  (bprint (a-val param)))
          (format nil "(list ~{~A~})"
                  (loop :for fld :in (eval (a-fields param)) :collect
                     (gen fld :entity-param (a-entity param))))))



;; dispatcher -->
(defmethod gen ((param list) &key entity-param)
  (format t "~%-- :~A" param)
  (format t "~%== :~A" entity-param)
  (gen (ent-to-mi param) :entity-param entity-param))


(defmethod gen ((param fld) &key entity-param)
  (let* ((entity    (find-if #'(lambda (entity)
                                 (equal (getf entity :entity) entity-param))
                             *entityes*))
         (record    (find-if #'(lambda (x)
                                 (equal (car x) (a-title param))) ;; NB!: ent-to-me TITLE = NAME
                             (getf entity :fields)))
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
      (format nil "~%~25T (mi 'fld :name \"~A\" :typedata '~A :title ~A :width ~A :xref ~A ~%~31T :perm ~A)"
              (bprint fld)
              (bprint typedata)
              (bprint name)
              width
              (bprint (a-xref param))
              (let ((res-perm))
                (loop :for perm :in obj-perm :by #'cddr :do
                   (if (null (getf fld-perm perm))
                       (setf (getf res-perm perm) (getf obj-perm perm))
                       (setf (getf res-perm perm) (getf fld-perm perm))))
                (push ''perm res-perm)
                (push 'mi res-perm)
                (bprint res-perm))))))


(defmethod gen ((param btn) &key entity-param)
  (let ((genid (string-downcase (symbol-name (gensym "B")))))
    ;; add controller
    (setf *controllers*
          (append *controllers*
                  (list (list genid (a-act param)))))
    ;; output
    (format nil "~%~25T (mi 'btn :name \"~A\" :width ~A :perm ~A :value \"~A\")"
            genid
            (aif (a-width param) it "200")
            (bprint (a-perm param))
            (a-title param))))


(defmethod gen ((param popbtn) &key entity-param)
  (declare (ignore entity-param))
  (let* ((genid (string-downcase (symbol-name (gensym "P")))))
    ;; output
    (format nil "~%~25T (mi 'popbtn :name \"~A\" :width ~A :perm ~A :value \"~A\" ~%~31T :action ~A)"
            genid
            (aif (a-width param) it "200")
            (bprint (a-perm param))
            (a-title param)
            (let ((action (eval (a-action param))))
              (gen (ent-to-mi action))))))


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
       (format out "~%~%(with-defclass (~A (entity))~%~{  ~A~^~%~})"
               (getf entity :entity)
               (loop :for field :in (getf entity :fields) :collect
                  (format nil "(~A ~23:T nil)" (car field))))))
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
     (format out "~A~%  (let ((session (hunchentoot:start-session))~%~7T (acts (list ~{~A~})))~%~4T(declare (ignore session))~%~4T(show-acts acts)))"
             (if *param-id-flag* (format nil "~% (declare (ignore id))") "")
             (loop :for action :in (eval (getf place :actions)) :collect
                (gen action) ;; <-- dispatcher: (defmethod gen ((param list) &key entity-param)
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
                        )))
     )
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
