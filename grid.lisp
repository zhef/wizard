(in-package #:WIZARD)

(defparameter *popups* nil)


;; (defmacro a-fld (name obj)
;;   `(if (equal val :clear)
;;        ""
;;        (funcall
;;         (intern
;;          (format nil "A-~A" ,name)
;;          (find-package "WIZARD"))
;;         ,obj)))


;; (defun show-fld-helper (captfld tplfunc namefld valuefld)
;;   (tpl:fld
;;    (list :fldname captfld
;;          :fldcontent (funcall tplfunc (list :name namefld
;;                                             :value valuefld)))))


;; (defmethod show ((infld fld) &key val) ;; keys: val
;;   (let ((namefld   (a-name  infld))
;;         (captfld   (a-title infld))
;;         (permfld   (a-perm  infld))
;;         (typedata  (a-typedata infld)))
;;     (declare (ignore permfld))
;;     (cond ((equal typedata '(:bool))     (show-fld-helper captfld #'tpl:flagupd namefld (a-fld namefld val)))
;;           ((equal typedata '(:num))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
;;           ((equal typedata '(:str))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
;;           ((equal typedata '(:pswd))     (show-fld-helper captfld #'tpl:pswdupd namefld (a-fld namefld val)))
;;           ((equal typedata '(:interval))
;;            (let ((val (a-fld namefld val)))
;;              (if (equal 'INTERVAL (type-of val))
;;                  (tpl:fld
;;                   (list :fldname captfld
;;                         :fldcontent (tpl:intervalupd (list :name namefld
;;                                                            :valuebegin (decode-date (interval-begin val))
;;                                                            :valueend   (decode-date (interval-end val))))))
;;                  (tpl:fld
;;                   (list :fldname captfld
;;                         :fldcontent (tpl:intervalupd (list :name namefld
;;                                                            :valuebegin ""
;;                                                            :valueend   "")))))))
;;           ((equal typedata '(:date))
;;            (let ((val (a-fld namefld val)))
;;              (if (or (null val)
;;                      (equal "" val))
;;                  (tpl:fld
;;                   (list :fldname captfld
;;                         :fldcontent (tpl:dateupd (list :name namefld
;;                                                        :value ""))))
;;                  (tpl:fld
;;                   (list :fldname captfld
;;                         :fldcontent (tpl:dateupd (list :name namefld
;;                                                        :value (decode-date val))))))))
;;           ((equal typedata '(:list-of-keys supplier-status))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (getf *supplier-status* (a-fld namefld val)))))))
;;           ((equal typedata '(:list-of-keys offer-status))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (getf *offer-status* (a-fld namefld val)))))))
;;           ((equal typedata '(:list-of-keys resource-types))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (getf *resource-types* (a-fld namefld val)))))))
;;           ((equal typedata '(:list-of-keys tender-status))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (getf *tender-status* (a-fld namefld val)))))))
;;           ((equal typedata '(:link builder))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
;;           ((equal typedata '(:link category))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
;;           ((equal typedata '(:link supplier))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (let ((it (a-fld namefld val)))
;;                                                           (if (null it) "" (a-name it))))))))
;;           ((equal typedata '(:link tender))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
;;           ((equal typedata '(:link tender-resource))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :value (a-name (a-resource (a-fld namefld val))))))))
;;           ((equal typedata '(:text))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:textupd (list :name namefld
;;                                                  :value (a-fld namefld val))))))
;;           ((equal typedata '(:list-of-str))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:textupd (list :name namefld
;;                                                  :value (a-fld namefld val))))))
;;           ((equal typedata '(:link supplier-resource-price))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :name namefld
;;                                                  :value (a-name (a-fld namefld val)))))))
;;           ((equal typedata '(:link resource))
;;            (tpl:fld
;;             (list :fldname captfld
;;                   :fldcontent (tpl:strview (list :name namefld
;;                                                  :value (a-name (a-fld namefld val)))))))
;;           (t (format nil "<br />err:unk2 typedata: ~A | ~A" namefld typedata)))))


;; (defmethod show ((infld btn) &key)
;;   (if (check-perm (a-perm infld) (cur-user))
;;       (tpl:btnlin (list :name (a-name infld) :value (a-value infld)))
;;       ""))


;; (defmethod show ((infld popbtn) &key)
;;   (if (check-perm (a-perm infld) (cur-user))
;;       (progn
;;         (let ((in-action (a-action infld)))
;;           (push
;;            (list :id (a-name infld)
;;                  :title (a-title in-action)
;;                  :content (show-act in-action)
;;                  :left 200
;;                  :width 800)
;;            *popups*))
;;         (tpl:popbtnlin (list :popid (a-name infld)
;;                              :value (a-value infld))))
;;       ""))


;; (defmethod show ((infld file) &key)
;;   (tpl:fld
;;    (list :fldname (a-value infld)
;;          :fldcontent (tpl:fileupd (list :name (a-name infld))))))


;; (defun show-linear (act val)
;;   (let ((flds (loop :for infld :in (a-fields act) :collect
;;                  (cond ((equal 'fld (type-of infld))
;;                         (show infld :val val))
;;                        ((equal 'btn (type-of infld))
;;                         (show infld))
;;                        ((equal 'popbtn (type-of infld))
;;                         (show infld))
;;                        ((equal 'file (type-of infld))
;;                         (show infld))
;;                        ((equal 'action (type-of infld))
;;                         (format nil "<div style=\"border: 1px solid red:\"> ~A</div>" (show-act infld)))
;;                        (t (error "show-linear bad infld"))))))
;;     (tpl:frmobj (list :content (format nil "~{~A~}" flds)))))


;; (defmethod show ((obj yamap) &key)
;;   (tpl:map (list :center (a-center-coord obj)
;;                  :placemarks (format nil "~{~A~}"
;;                                      (mapcar #'(lambda (point)
;;                                                  (tpl:placemark
;;                                                   (list :title (a-title point)
;;                                                         :coord (a-coord point)
;;                                                         :descr (a-descr point))))
;;                                              (a-mark-points obj))))))

;; (defun show-map (act val)
;;   (let ((yamap (mi 'YAMAP
;;                    :center-coord (nth 0 (car val))
;;                    :mark-points  (mapcar #'(lambda (point)
;;                                              (mi 'YAPOINT
;;                                                  :title (aif (nth 2 point) it "")
;;                                                  :descr (aif (nth 1 point) it "")
;;                                                  :coord (aif (nth 0 point) it "")))
;;                                          val))))
;;     (show yamap)))



(defmethod show ((param t) &key)
  (error "no applicable method SHOW for ~A" (type-of param)))

(defmethod show ((param none) &key)
  (tpl:content-block
   (list :title (a-title param)
         :content "Раздел находится в разработке")))

(defmethod show ((param tpl) &key)
  (tpl:content-block
   (list :title (a-title param)
         :content (format nil "~A" (funcall (a-val param))))))


(defmethod show ((param grid) &key)
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-names)
        (col-model)
        (col-replace))
    (declare (special *popups*))
    (loop :for infld :in (a-fields param) :collect
       (cond ((equal 'fld (type-of infld))
              (when (check-perm (a-show (a-perm infld)) (cur-user))
                (push (a-title infld) col-names)
                (let* ((in-name (a-name infld))
                       (width   (a-width infld))
                       (model `(("name"  . ,in-name)
                                ("index" . ,in-name)
                                ("width" . ,width)
                                ("align" . ,(if (equal '(:num) (a-typedata infld))
                                                "center"
                                                "left"))
                                ("sortable" . t)
                                ("editable" . nil))))
                  (push model col-model))))
             ((equal 'btn (type-of infld))
              (when (check-perm (a-perm infld) (cur-user))
                (let* ((in-name "")
                       (width   (a-width infld))
                       (model `(("name"  . ,in-name)
                                ("index" . ,in-name)
                                ("width" . ,width)
                                ("align" . "center")
                                ("sortable" . nil)
                                ("editable" . nil))))
                  (push in-name col-names)
                  (push model col-model))))
             ((equal :calc (car infld))
              (when (check-perm (getf infld :perm) (cur-user))
                (let* ((in-name (getf infld :calc))
                       (width   (getf infld :width))
                       (model `(("name" . ,in-name)
                                ("index" . ,in-name)
                                ("width" . ,width)
                                ("align" . "left")
                                ("sortable" . t)
                                ("editable" . nil))))
                  (push in-name col-names)
                  (push model col-model))))
             ((equal :popbtn (car infld))
              (when (check-perm (getf infld :perm) (cur-user))
                (let* ((in-name "" #|(getf infld :popbtn)|#)
                       (width   (getf infld :width))
                       (model `(("name" . ,in-name)
                                ("index" . ,in-name)
                                ("width" . ,width)
                                ("align" . "center")
                                ("sortable" . nil)
                                ("editable" . nil))))
                  (push in-name col-names)
                  (push model col-model)
                  (let ((in-action (getf infld :action)))
                    (push
                     (list :id (getf infld :popbtn) :title (getf in-action :action) :content (show-act in-action) :left 200 :width 500)
                     *popups*)))))
             (t (error "show-grid unk fld" ))))
    (tpl:content-block
     (list :title   (a-title param)
           :content (tpl:gridview
                     (list :idgrid grid-id
                           :idpager pager-id
                           :json (replace-all
                                  (json:encode-json-to-string
                                   `(("url" . ,(format nil "/~A~A"
                                                       (a-grid param)
                                                       (if (a-param-id param) (format nil "/~A" (cur-id)) "")
                                                       )) ;; absolute uri
                                     ("datatype" . "json")
                                     ("colNames" . ,(reverse col-names))
                                     ("colModel" . ,(reverse col-model))
                                     ("rowNum" . 10)
                                     ("rowList" . (10 20 30))
                                     ("pager" . ,(format nil "#~A" pager-id))
                                     ("sortname" . "id")
                                     ("viewrecords" . t)
                                     ("sortorder" . "desc")
                                     ("height" . ,(aif (a-height param) it "180"))
                                     ("editurl" . "/edit_url")
                                     ("gridComplete" . "-=|=-")
                                     ("caption" . ,(a-title param))))
                                  "\"-=|=-\"" ;; замена после кодирования в json - иначе никак не вставить js :)
                                  (format nil "
function(){
  var ids = jQuery(\"#~A\").jqGrid('getDataIDs');
  for(var i=0;i < ids.length;i++){
    var cl = ids[i];
    ~{~A~%~}
  }
}
" grid-id (loop :for (in-name btn-str) :in (reverse col-replace) :collect
             (format nil "jQuery(\"#~A\").jqGrid('setRowData',ids[i],{~A: ~A});"
                     grid-id in-name btn-str)))
                                  )))))))


(defun show-act (act)
  (unless (check-perm (a-perm act) (cur-user) (a-val act))
    (return-from show-act ""))
  (show act))


(defun show-acts (acts)
  (let* ((personal  (let ((userid (hunchentoot:session-value 'userid)))
                      (if (null userid)
                          (tpl:loginform)
                          (tpl:logoutform (list :user (a-name (gethash userid *USER*))
                                                :usertype (string-downcase (type-of (gethash userid *USER*)))
                                                :userid userid)))))
         (*popups*  (list
                     (list :id "trest"      :title "Регистрация" :content "TODO"           :left 200 :width 500)
                     (list :id "popupLogin" :title "Вход"        :content (tpl:popuplogin) :left 720 :width 196)))
         (content   (format nil "~{~A~}" (loop :for act :in acts :collect (show-act act)))))
    (declare (special *popups*))
    (tpl:root
     (list
      :mapkey    *mapkey*
      :personal  personal
      :popups    *popups*
      :navpoints (menu)
      :content  content))))


(defun json-assembly (cur-page total-page rows-per-page rows)
  "example call: (json-assembly 1 2 2 '( (1 \"one\" \"two\") (2 \"three\" \"fourth\")))"
  (json:encode-json-to-string
   `(("page"    . ,cur-page)
     ("total"   . ,total-page)
     ("records" . ,rows-per-page)
     ("rows"    . ,(loop :for row :in rows :collect
                      `(("id"   . ,(car row))
                        ("cell" . ,(cdr row))))))))


(defmethod get-accessor-perm ((infld fld))
  (let ((perm  (a-show (a-perm infld)))
        (symb  (find-symbol (format nil "A-~A" (a-name infld)) (find-package "WIZARD")))
        (accessor))
    (cond ((equal '(:bool)                             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (aif (funcall symb x) "да" "нет")))))
          ((equal '(:str)                              (a-typedata infld))
           (let ((tmp (a-xref infld)))
             (if (null tmp)
                 (setf accessor (lambda (x) (format nil "~A" (funcall symb x))))
                 (setf accessor (lambda (x) (format nil "<a href=\"/~A/%|id|%\">~A</a>"
                                                    tmp
                                                    (funcall symb x)))))))
          ((equal '(:text)                             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (aif (funcall symb x) it "")))))
          ((equal '(:num)                              (a-typedata infld))
           (setf accessor (lambda (x)
                            (let ((val (funcall symb x)))
                              (if (realp val)
                                  (format nil "~F" val)
                                  (format nil "~A" val))))))
          ((equal '(:link resource)                    (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:link tender)                      (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:list-of-keys tender-status)       (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *tender-status* (funcall symb x))))))
          ((equal '(:list-of-keys offer-status)        (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *offer-status* (funcall symb x))))))
          ((equal '(:link builder)                     (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:link supplier)                    (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (funcall symb x))))))
          ((equal '(:link tender-resource)             (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (a-name (a-resource (funcall symb x)))))))
          ((equal '(:list-of-keys resource-types)      (a-typedata infld))
           (setf accessor (lambda (x) (format nil "~A" (getf *resource-types* (funcall symb x))))))
          ((equal '(:interval)                         (a-typedata infld))
           (setf accessor (lambda (x)  (let ((val (funcall symb x)))
                                         (format nil "~A-~A"
                                                 (decode-date (interval-begin val))
                                                 (decode-date (interval-end val)))))))
          (t ;; default - print unknown type message in grid field
           (let ((err-str (format nil "unk typedata: ~A" (a-typedata infld))))
             (setf accessor (lambda (x)
                              (declare (ignore x))
                              err-str)))))
    (values
     accessor
     perm)))


(defmethod get-accessor-perm ((infld btn))
  (let* ((perm      (a-perm  infld))
         (btn       (a-name  infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
         (value     (a-value infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
         (accessor  (lambda (x)
                      (declare (ignore x))
                      (format nil "<form method='post'><input type='submit' name='~A~~%|id|%' value='~A' /></form>"
                              btn
                              value))))
    (values accessor perm)))


(defmethod get-accessor-perm ((infld popbtn))
  ;; (let* ((perm      (getf infld :perm))
  ;;        (btn       (getf infld :popbtn))                ;; тут важно чтобы вычисление происходило вне лямбды
  ;;        (value     (getf infld :value))                 ;; тут важно чтобы вычисление происходило вне лямбды
  ;;        (accessor  (lambda (x)
  ;;                     (declare (ignore x))
  ;;                     (format nil "<input type='button' name='~A~~%|id|%' value='~A' onclick='ShowHide(\"~A\")' />"
  ;;                             btn
  ;;                             value
  ;;                             btn))))
  ;;   (push (cons accessor perm) field-cons))
  (error "TODO-ERROR: generic method (get-accessor-perm (popbtn)) not implemented"))


;; (defmethod get-accessor-perm ((infld calc))
;;    (let* ((perm      (getf infld :perm))
;;           (calc      (getf infld :cacl))                  ;; тут важно чтобы вычисление происходило вне лямбды
;;           (func      (getf infld :func))                  ;; тут важно чтобы вычисление происходило вне лямбды
;;           (accessor  (getf infld :func)))
;;      (push (cons accessor perm) field-cons)))


(defmethod get-accessor-perm (infld)
  (error "unk pager directive"))


(defun pager (val fields page rows-per-page)
  ;; Полезная отладка, чтобы определить, какой контроллер вызывает ошибку
  ;; (error (format nil "~A" val))) ;;
  (let* ((rows             (funcall val))
         (cnt-rows         (length rows))
         (slice-cons)      ;; many of (id . #<object>)
         (field-cons))     ;; manu of (#<function-accessor> . plist-permlist)
    ;; slice-cons (overloop) - выбираем те объекты, которые попадают на страницу
    (loop :for num :from (* page rows-per-page) :below (* (+ 1 page) rows-per-page) :do
       (let ((row (nth num rows)))
         (unless (null row)
           (push (nth num rows) slice-cons))))
    ;; field-cons (innerloop) - получаем для каждого поля функцию-accessor и права
    (loop :for infld :in fields :collect
       (multiple-value-bind (accessor perm)
           (get-accessor-perm infld)
       (push (cons accessor perm) field-cons)))
    ;; Применяем к каждому выбранному на первом шаге объекту функции-accessor-ы полученные на втором
    (let ((result
           (loop :for (id . obj) :in (reverse slice-cons) :collect
              (list* id
                     (loop :for (accessor . perm) :in (reverse field-cons) :collect
                        (if (check-perm perm (cur-user) obj)
                            (replace-all (funcall accessor obj)
                                         "%|id|%"
                                         (format nil "~A" id))
                            (if (boundp '*dbg*) "permission denied in grid pager" "")))))))
      (values result cnt-rows))))


(defun example-json (val fields)
  "Выясняем страницу и кол-во строк в ней, вызываем pager и формируем вывод с помощью json-assembly"
  (let* ((page            (- (parse-integer (hunchentoot:get-parameter "page")) 1))
         (rows-per-page   (parse-integer (hunchentoot:get-parameter "rows"))))
    (multiple-value-bind (slice cnt-rows)
        (pager val fields page rows-per-page)
      (json-assembly  (+ page 1)  (ceiling cnt-rows rows-per-page)  (length slice) slice))))
