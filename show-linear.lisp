(in-package #:WIZARD)


(defgeneric show-linear (infld val &key))

(defmethod show-linear (infld val &key)
  (error "no applicable method SHOW-LINEAR for ~A" (type-of infld)))

(defmethod show-linear ((infld fld) val &key)
  (let ((val       (funcall val))
        (namefld   (a-name       infld))
        (captfld   (a-title      infld))
        (permfld   (a-perm       infld))
        (typedata  (a-typedata   infld)))
    (declare (ignore permfld))
    (show-linear-elt typedata val namefld captfld permfld)))


(defmethod show-linear ((infld btn) val &key)
  (if (check-perm (a-perm infld) (cur-user))
      (tpl:btnlin (list :name (a-name infld) :value (a-value infld)))
      ""))


(defmethod show-linear ((infld popbtn) val &key)
  (if (check-perm (a-perm infld) (cur-user))
      (progn
        (let ((in-action (a-action infld)))
          (push
           (list :id (a-name infld)
                 :title (a-title in-action)
                 :content (show-act in-action)
                 :left 200
                 :width 800)
           *popups*))
        (tpl:popbtnlin (list :popid (a-name infld)
                             :value (a-value infld))))
      ""))


(defmethod show-linear ((infld grid) val &key)
  (format nil "<div style=\"border: 1px solid red:\"> ~A</div>" (show-act infld)))


;; (cond ((equal 'fld (type-of infld))
;;        (show infld :val val))
;; ((equal 'btn (type-of infld))
;;  (show infld))
;; ((equal 'popbtn (type-of infld))
;;  (show infld))
;; ((equal 'file (type-of infld))
;;  (show infld))
;; ((equal 'action (type-of infld))
;;  (format nil "<div style=\"border: 1px solid red:\"> ~A</div>" (show-act infld)))
;; (t (error "show-linear bad infld")))

