(in-package #:WIZARD)

(defparameter *popups* nil)


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


(defmethod show ((infld fld) &key val) ;; keys: val
  (let ((namefld   (a-name  infld))
        (captfld   (a-title infld))
        (permfld   (a-perm  infld))
        (typedata  (a-typedata infld)))
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


(defmethod show ((infld btn) &key)
  (if (check-perm (a-perm infld) (cur-user))
      (tpl:btnlin (list :name (a-name infld) :value (a-value infld)))
      ""))


(defmethod show ((infld popbtn) &key)
  (if (check-perm (a-perm infld) (cur-user))
      (progn
        (let ((in-action (a-action infld)))
          (push
           (list :id (a-name infld)
                 :title (getf in-action :action)
                 :content (show-act in-action)
                 :left 200
                 :width 800)
           *popups*))
        (tpl:popbtnlin (list :popid (a-name infld)
                             :value (a-value infld))))
      ""))


(defmethod show ((infld file) &key)
  (tpl:fld
   (list :fldname (a-value infld)
         :fldcontent (tpl:fileupd (list :name (a-name infld))))))


(defun show-linear (act val)
  (let ((flds (loop :for infld :in (getf act :fields) :collect
                 (cond ((equal 'fld (type-of infld))
                        (show infld :val val))
                       ((equal 'btn (type-of infld))
                        (show infld))
                       ((equal 'popbtn (type-of infld))
                        (show infld))
                       ((equal 'file (type-of infld))
                        (showinfld))
                       ((equal :action (car infld))
                        (format nil "<div style=\"border: 1px solid red:\"> ~A</div>" (show-act infld)))
                       (t (error "show-linear bad infld"))))))
    (tpl:frmobj (list :content (format nil "~{~A~}" flds)))))


(defun grid-replace-helper (grid-id col-replace)
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
                     grid-id in-name btn-str))))


(defun show-grid (act val)
  (declare (ignore val))
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-names)
        (col-model)
        (col-replace))
    (declare (special *popups*))
    (loop :for infld :in (getf act :fields) :collect
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
    (grid-helper grid-id pager-id (replace-all
                                   (json:encode-json-to-string
                                    `(("url" . ,(format nil "/~A~A"
                                                        (getf act :grid)
                                                        (if (getf act :param-id) (format nil "/~A" (cur-id)) "")
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
                                      ("editurl" . "/edit_url")
                                      ("gridComplete" . "-=|=-")
                                      ("caption" . ,(getf act :action))))
                                   "\"-=|=-\"" ;; замена после кодирования в json - иначе никак не вставить js :)
                                   (grid-replace-helper grid-id col-replace)))))


(defmethod show ((obj yamap) &key)
  (tpl:map (list :center (a-center-coord obj)
                 :placemarks (format nil "~{~A~}"
                                     (mapcar #'(lambda (point)
                                                 (tpl:placemark
                                                  (list :title (a-title point)
                                                        :coord (a-coord point)
                                                        :descr (a-descr point))))
                                             (a-mark-points obj))))))

(defun show-map (act val)
  (let ((yamap (mi 'YAMAP
                   :center-coord (nth 0 (car val))
                   :mark-points  (mapcar #'(lambda (point)
                                             (mi 'YAPOINT
                                                 :title (aif (nth 2 point) it "")
                                                 :descr (aif (nth 1 point) it "")
                                                 :coord (aif (nth 0 point) it "")))
                                         val))))
    (show yamap)))


(defun show-act (act)
  (if (not (check-perm (getf act :perm) (cur-user) (getf act :val)))
      ""
      ;; (format nil "permission denied in defun show-act: ~A"
      ;;         (bprint (getf act :perm) #| act |#))
      ;; else
      (let ((val (funcall (getf act :val))))
        (case (getf act :showtype)
          (:none     "Раздел в разработке")
          (:linear   (show-linear act val))
          (:grid     (show-grid   act val))
          (:map      (show-map    act val))
          (otherwise (format nil "unknown showtype [~A] in defun show-act [~A]"
                             (bprint (getf act :showtype))
                             (bprint (getf act :action))))))))


(defun show-acts (acts)
    (let* ((personal  (let ((userid (hunchentoot:session-value 'userid)))
                        (if (null userid)
                            (tpl:loginform)
                            (tpl:logoutform (list :user (a-login (gethash userid *USER*)))))))
           (*popups*  (list
                       (list :id "trest"      :title "Регистрация" :content "TODO"           :left 200 :width 500)
                       (list :id "popupLogin" :title "Вход"        :content (tpl:popuplogin) :left 720 :width 196)))
           (content   (format nil "~{~A~}"
                              (loop :for act :in acts :when (check-perm (getf act :perm) (cur-user) (getf act :val))  :collect
                                 (tpl:content-block
                                  (list :title (getf act :action)
                                        :content (show-act act)))))))
      (declare (special *popups*))
      (tpl:root
       (list
        :mapkey    *mapkey*
        :personal  personal
        :popups    *popups*
        :navpoints (menu)
        :content  content))))



(defun grid-helper (grid-id pager-id json-code)
  (format nil "<div style=\"margin: 10px 0 10px 0\">
               <table id=\"~A\"></table><div id=\"~A\"></div>
               <script type=\"text/javascript\">
               jQuery('#~A').jqGrid(~A);
               jQuery('#~A').jqGrid('navGrid','#~A',{edit:false,add:false,del:false});
               </script>
               </div>"
          grid-id pager-id grid-id json-code grid-id pager-id))


(defun json-assembly (cur-page total-page rows-per-page rows)
  "rows: `(id fld1 fld2 fld3...)"
  (json:encode-json-to-string
   `(("page"    . ,cur-page)
     ("total"   . ,total-page)
     ("records" . ,rows-per-page)
     ("rows"    . ,(loop :for row :in rows :collect
                      `(("id"   . ,(car row))
                        ("cell" . ,
                                ;; (list*
                                    ;; "<a href=\"sss\">fdd</a>"
                                    (cdr row)
                                    ;; )
                                ))))))) ;; <------ here inserted

(defun pager (val fields page rows-per-page)
  (let* ((rows             (funcall val))
         (cnt-rows         (length rows))
         (slice-cons)      ;; many of (id . #<object>)
         (field-cons))     ;; manu of (#<function-accessor> . plist-permlist)
    ;; slice-cons (overloop)
    (loop :for num :from (* page rows-per-page) :below (* (+ 1 page) rows-per-page) :do
       (let ((row (nth num rows)))
         (unless (null row)
           (push (nth num rows) slice-cons))))
    ;; field-cons (innerloop)
    (loop :for infld :in fields :collect
       (cond ((equal 'fld (type-of infld))
              (let ((perm  (a-show (a-perm infld)))
                    (symb  (find-symbol (format nil "A-~A" (a-name infld)) (find-package "WIZARD")))
                    (accessor))
                (cond ((equal '(:bool)                             (a-typedata infld))
                       (setf accessor (lambda (x) (format nil "~A" (aif (funcall symb x) "да" "нет")))))
                      ((equal '(:str)                              (a-typedata infld))
                       (if (equal 'a-name symb)
                           (setf accessor (lambda (x) (format nil "<a href=\"/\">~A</a>" (funcall symb x))))
                           (setf accessor (lambda (x) (format nil "~A" (funcall symb x))))))
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
                (push (cons accessor perm) field-cons)))

             ((equal 'btn (type-of infld))
              (let* ((perm      (a-perm  infld))
                     (btn       (a-name  infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
                     (value     (a-value infld))                     ;; тут важно чтобы вычисление происходило вне лямбды
                     (accessor  (lambda (x)
                                  (declare (ignore x))
                                  (format nil "<form method='post'><input type='submit' name='~A~~%|id|%' value='~A' /></form>"
                                          btn
                                          value))))
                (push (cons accessor perm) field-cons)))

             ((equal :popbtn (car infld))
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
              "zzz"
              )

             ((equal :calc (car infld))
              (let* ((perm      (getf infld :perm))
                     (calc      (getf infld :cacl))                  ;; тут важно чтобы вычисление происходило вне лямбды
                     (func      (getf infld :func))                  ;; тут важно чтобы вычисление происходило вне лямбды
                     (accessor  (getf infld :func)))
                (push (cons accessor perm) field-cons)))


             (t (error "unk pager directive"))
             ) ;; end cond
       ) ;; end loop field cons (innerloop)
    (values
     ;; result: get values from obj
     (loop :for (id . obj) :in (reverse slice-cons) :collect
        (list* id
               (loop :for (accessor . perm) :in (reverse field-cons) :collect
                  (if (check-perm perm (cur-user) obj)
                      (replace-all (funcall accessor obj)
                                   "%|id|%"
                                   (format nil "~A" id))
                      (if (boundp '*dbg*) "permission denied in grid pager" "")))))
     ;; cnt-rows - two result
     cnt-rows)))


(defun example-json (val fields)
  (let* ((page            (- (parse-integer (hunchentoot:get-parameter "page")) 1))
         (rows-per-page   (parse-integer (hunchentoot:get-parameter "rows"))))
    (multiple-value-bind (slice cnt-rows)
        (pager val fields page rows-per-page)
      (json-assembly  (+ page 1)  (ceiling cnt-rows rows-per-page)  (length slice) slice))))
