(in-package #:WIZARD)

(defgeneric show-grid (infld &key))

(defmethod show-grid ((infld t) &key)
  (error "no applicable method SHOW-GRID for ~A" (type-of infld)))


(defmethod show-grid ((infld fld) &key)
  (unless (check-perm (a-show (a-perm infld)) (cur-user))
    (return-from show-grid nil))
  (values
   (a-title infld)
   `(("name"     . ,(a-name infld))
     ("index"    . ,(a-name infld))
     ("width"    . ,(a-width infld))
     ("align"    . ,(if (equal '(:num) (a-typedata infld)) "center" "left"))
     ("sortable" . t)
     ("editable" . nil))))


(defmethod show-grid ((infld btn) &key)
  (unless (check-perm (a-perm infld) (cur-user))
    (return-from show-grid nil))
  (values
   ""
   `(("name"     . "")
     ("index"    . "")
     ("width"    . ,(a-width infld))
     ("align"    . "center")
     ("sortable" . nil)
     ("editable" . nil))))


(defmethod show-grid ((infld popbtn) &key)
  (unless (check-perm (a-perm infld) (cur-user))
    (return-from show-grid nil))
  (let ((in-action (a-action infld)))
    (push
     (list :id (a-name infld)
           :title (a-title in-action)
           :content (show-act in-action)
           :left 200
           :width 500)
     *popups*))
  (values
   ""
   `(("name"     . "")
     ("index"    . "")
     ("width"    . ,(a-width infld))
     ("align"    . "center")
     ("sortable" . nil)
     ("editable" . nil))))


;; (cond
  ;; ((equal :calc (car infld))
  ;;  (when (check-perm (getf infld :perm) (cur-user))
  ;;    (let* ((in-name (getf infld :calc))
  ;;           (width   (getf infld :width))
  ;;           (model `(("name" . ,in-name)
  ;;                    ("index" . ,in-name)
  ;;                    ("width" . ,width)
  ;;                    ("align" . "left")
  ;;                    ("sortable" . t)
  ;;                    ("editable" . nil))))
  ;;      (push in-name col-names)
  ;;      (push model col-model))))
  ;; (t (error "show-grid unk fld" )))
