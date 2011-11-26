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


(defmethod gen ((param none) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%   (mi 'none :title ~A :perm ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))))


(defmethod gen ((param yamap) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%   (mi 'yamap :title ~A :perm ~A :center-coord \"30.313622, 59.937720\" ~%~19T :mark-points ~A)"
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
  (format nil "~%   (mi 'tpl :title ~A :perm ~A ~%~11T :val ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "GRDNL-"))
                  (bprint (a-val param)))))


(defmethod gen ((param linear) &key entity-param)
  (format nil "~%   (mi 'linear :title ~A :perm '~A ~%~14T :val ~A ~%~14T :fields ~A)"
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
    (format nil "~%   (mi 'grid :title ~A ~%~12T :perm '~A ~%~12T :grid ~A ~%~12T :param-id ~A ~%~12T :height ~A ~%~12T :val ~A ~%~12T :fields ~A)"
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
  (format nil "~%   (mi 'announce :title ~A ~%~16T :perm '~A ~%~16T :val ~A ~%~16T :fields ~A)"
          (bprint (a-title param))
          (bprint (a-perm param))
          (format nil "(named-lambda ~A () ~A)"
                  (symbol-name (gensym "ACTNL-"))
                  (bprint (a-val param)))
          (format nil "(list ~{~A~})"
                  (loop :for fld :in (eval (a-fields param)) :collect
                     (gen fld :entity-param (a-entity param))))))


(defmethod gen ((param post) &key entity-param)
  (format nil "~%   (mi 'post :title ~A ~%~12T :perm '~A ~%~12T :val ~A ~%~12T :fields ~A)"
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
  ;; (format t "~%-- :~A" param)
  ;; (format t "~%== :~A" entity-param)
  (gen (ent-to-mi param) :entity-param entity-param))


(defmethod gen ((param fld) &key entity-param)
  (let* ((entity    (find-if #'(lambda (entity)
                                 (equal (getf entity :entity) entity-param))
                             *entityes*))
         (record    (find-if #'(lambda (x)
                                 (equal (car x) (a-title param))) ;; NB!: ent-to-me TITLE = NAME
                             (getf entity :fields)))
         (obj-perm  (getf entity :perm)))
    (destructuring-bind (fld name typedata)
        record
      (format nil "~%~6T (mi 'fld :name \"~A\" :typedata '~A :title ~A :width ~A ~A ~%~15T :update ~A :view ~A :show ~A)"
              (bprint fld)
              (bprint typedata)
              (bprint name)
              (aif (a-width param) it 200)
              (aif (a-xref param)  (format nil ":xref ~A" (bprint it)) "")
              (bprint (aif (a-update param)
                           (a-update param)
                           (getf obj-perm :update)))
              (bprint (aif (a-view param)
                           (a-view param)
                           (getf obj-perm :view)))
              (bprint (aif (a-show param)
                           (a-show param)
                           (getf obj-perm :show)))))))


(defmethod gen ((param btn) &key entity-param)
  (let ((genid (string-downcase (symbol-name (gensym "B")))))
    ;; add controller
    (setf *controllers*
          (append *controllers*
                  (list (list genid (a-act param)))))
    ;; output
    (format nil "~%~6T (mi 'btn :name \"~A\" :width ~A :perm ~A :value \"~A\")"
            genid
            (aif (a-width param) it "200")
            (bprint (a-perm param))
            (a-title param))))


(defmethod gen ((param popbtn) &key entity-param)
  (declare (ignore entity-param))
  (let* ((genid (string-downcase (symbol-name (gensym "P")))))
    ;; output
    (format nil "~%~6T (mi 'popbtn :name \"~A\" :top ~A :left ~A :width ~A :height ~A :perm ~A :value \"~A\" ~%~19T :action ~A)"
            genid
            (aif (a-top param) it "200")
            (aif (a-left param) it "400")
            (aif (a-width param) it "200")
            (aif (a-height param) it "400")
            (bprint (a-perm param))
            (a-title param)
            (let ((action (eval (a-action param))))
              (gen (ent-to-mi action))))))


(defmethod gen ((param file) &key entity-param)
  (declare (ignore entity-param))
  (format nil "~%~6T (mi 'file :name \"~A\" :perm ~A :value \"~A\")"
          (a-title param)
          (bprint (a-perm param))
          (a-name param)))




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
     (format out "~A~%  (list ~{~A~}))"
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
