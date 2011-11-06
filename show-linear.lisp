(in-package #:WIZARD)


(defmacro a-fld (name obj)
  `(if (equal val :clear)
       ""
       (funcall
        (intern
         (format nil "A-~A" ,name)
         (find-package "WIZARD"))
        ,obj)))

(defun show-fld-helper (captfld tplfunc namefld valuefld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (funcall tplfunc (list :name namefld
                                            :value valuefld)))))


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
    (cond ((equal typedata '(:bool))     (show-fld-helper captfld #'tpl:flagupd namefld (a-fld namefld val)))
          ((equal typedata '(:num))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:str))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:pswd))     (show-fld-helper captfld #'tpl:pswdupd namefld (a-fld namefld val)))
          ((equal typedata '(:interval))
           (let ((val (a-fld namefld val)))
             (if (equal 'INTERVAL (type-of val))
                 (tpl:fld
                  (list :fldname captfld
                        :fldcontent (tpl:intervalupd (list :name namefld
                                                           :valuebegin (decode-date (interval-begin val))
                                                           :valueend   (decode-date (interval-end val))))))
                 (tpl:fld
                  (list :fldname captfld
                        :fldcontent (tpl:intervalupd (list :name namefld
                                                           :valuebegin ""
                                                           :valueend   "")))))))
          ((equal typedata '(:date))
           (let ((val (a-fld namefld val)))
             (if (or (null val)
                     (equal "" val))
                 (tpl:fld
                  (list :fldname captfld
                        :fldcontent (tpl:dateupd (list :name namefld
                                                       :value ""))))
                 (tpl:fld
                  (list :fldname captfld
                        :fldcontent (tpl:dateupd (list :name namefld
                                                       :value (decode-date val))))))))
          ((equal typedata '(:list-of-keys supplier-status))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (getf *supplier-status* (a-fld namefld val)))))))
          ((equal typedata '(:list-of-keys offer-status))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (getf *offer-status* (a-fld namefld val)))))))
          ((equal typedata '(:list-of-keys resource-types))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (getf *resource-types* (a-fld namefld val)))))))
          ((equal typedata '(:list-of-keys tender-status))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (getf *tender-status* (a-fld namefld val)))))))
          ((equal typedata '(:link builder))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
          ((equal typedata '(:link category))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
          ((equal typedata '(:link supplier))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (let ((it (a-fld namefld val)))
                                                          (if (null it) "" (a-name it))))))))
          ((equal typedata '(:link tender))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
          ((equal typedata '(:link tender-resource))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (a-name (a-resource (a-fld namefld val))))))))
          ((equal typedata '(:text))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:textupd (list :name namefld
                                                 :value (a-fld namefld val))))))
          ((equal typedata '(:list-of-str))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:textupd (list :name namefld
                                                 :value (a-fld namefld val))))))
          ((equal typedata '(:link supplier-resource-price))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :name namefld
                                                 :value (a-name (a-fld namefld val)))))))
          ((equal typedata '(:link resource))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :name namefld
                                                 :value (a-name (a-fld namefld val)))))))
          (t (format nil "<br />err:unk2 typedata: ~A | ~A" namefld typedata)))))


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

