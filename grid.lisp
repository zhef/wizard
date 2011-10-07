(in-package #:WIZARD)


(defmacro a-fld (name obj)
  `(if (equal val :clear)
       ""
       (funcall
        (intern
         (format nil "A-~A" ,name)
         (find-package "WIZARD"))
        ,obj)))


(defmacro with-in-fld-case (fields &rest cases)
  `(loop :for infld :in ,fields :collect
      (ecase (car infld)
        ,@(loop :for case :in cases :by #'cddr :collect
             (list case (getf cases case))))))


(defun show-fld-helper (captfld tplfunc namefld valuefld)
  (tpl:fld
   (list :fldname captfld
         :fldcontent (funcall tplfunc (list :name namefld
                                            :value valuefld)))))


(defun show-fld (infld act val)
  (let ((namefld   (getf infld :fld))
        (captfld   (getf infld :name))
        (permfld   (getf infld :perm))
        (typedata  (getf infld :typedata)))
    (cond ((equal typedata '(:str))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:pswd))     (show-fld-helper captfld #'tpl:pswdupd namefld (a-fld namefld val)))
          ((equal typedata '(:num))      (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:interval)) (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:date))     (show-fld-helper captfld #'tpl:strupd  namefld (a-fld namefld val)))
          ((equal typedata '(:list-of-keys supplier-status))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (getf *supplier-status* (a-fld namefld val)))))))
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
                                                          (if (null it)
                                                              ""
                                                              (a-name it))))))))
          ((equal typedata '(:link tender))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:strview (list :value (a-name (a-fld namefld val)))))))
          ((equal typedata '(:list-of-str))
           (tpl:fld
            (list :fldname captfld
                  :fldcontent (tpl:textupd (list :name namefld
                                                 :value (a-fld namefld val))))))
          (t (format nil "<br />err:unk2 typedata: ~A | ~A" namefld typedata)))))


(defun show-btn (infld act)
  (tpl:btnlin (list :name (getf infld :btn) :value (getf infld :value))))


(defun show-popbtn (infld act)
  (let ((in-action (getf infld :action)))
    (push
     (list :id (getf infld :popbtn) :title (getf in-action :action) :content (show-act in-action) :left 200 :width 500)
     *popups*))
  (tpl:popbtnlin (list :popid (getf infld :popbtn) :value (getf infld :value))))


(defun show-linear (act val)
  (let ((flds (with-in-fld-case (getf act :fields) ;; infld variable
                :fld     (show-fld infld act val)
                :btn     (show-btn infld act)
                :action  (format nil "<div style=\"border: 1px solid red:\"> ~A</div>" (show-act infld))
                :popbtn  (show-popbtn infld act))))
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
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-names)
        (col-model)
        (col-replace))
    (with-in-fld-case (getf act :fields) ;; infld variable
      :fld (when (check-perm (getf (getf infld :permlist) :show) (cur-user))
             (push (getf infld :name) col-names)
             (let* ((in-name (getf infld :fld))
                    (model `(("name" . ,in-name)
                             ("index" . ,in-name)
                             ("width" . "200")
                             ("sortable" . t)
                             ("editable" . nil #|(if (check-perm (getf (getf infdls :permlist) :update)
                                                           t
                                                           nil)) |# )))) ;; rulez
               (push model col-model)))
      :btn (when (check-perm (getf infld :perm) (cur-user))
             (let* ((in-name (getf infld :btn))
                    (in-capt (getf infld :value))
                    (btn-str (format nil "\"<form method='post'><input type='submit' name='~A~~\"+cl+\"' value='~A' /></form>\"" in-name in-capt))
                    (model `(("name" . ,in-name)
                             ("index" . ,in-name)
                             ("width" . "200")
                             ("sortable" . nil)
                             ("editable" . nil))))
               (push in-name col-names)
               (push model col-model)
               ;; (push `(,in-name . ,btn-str) col-replace) ;; commented for change algorithm
               ))
      :popbtn (when (check-perm (getf infld :perm) (cur-user))
                (let* ((in-name (getf infld :popbtn))
                       (in-capt (getf infld :value))
                       (model `(("name" . ,in-name)
                                ("index" . ,in-name)
                                ("width" . "200")
                                ("sortable" . nil)
                                ("editable" . nil))))
                  (push in-name col-names)
                  (push model col-model)
                  (let ((in-action (getf infld :action)))
                    (push
                     (list :id (getf infld :popbtn) :title (getf in-action :action) :content (show-act in-action) :left 200 :width 500)
                     *popups*))))
      :calc (error "dumb error calc"))
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


(defun show-act (act)
  (if (not (check-perm (getf act :perm) (cur-user) (getf act :val)))
      (format nil "permission denied in defun show-act: ~A"
              (bprint #| (getf act :perm) |# act))
      ;; else
      (let ((val (funcall (getf act :val))))
        (case (getf act :showtype)
          (:none     "showtype is null in defun show-act")
          (:linear   (show-linear act val))
          (:grid     (show-grid   act val))
          (otherwise (format nil "unknown showtype in defun show-act: ~A" (bprint act)))))))


(defun show-acts (acts)
    (let* ((personal  (let ((userid (hunchentoot:session-value 'userid)))
                        (if (null userid)
                            (tpl:loginform)
                            (tpl:logoutform (list :user (a-login (gethash userid *USER*)))))))
           (*popups*  (list
                       (list :id "trest"      :title "Регистрация" :content "TODO"           :left 200 :width 500)
                       (list :id "popupLogin" :title "Вход"        :content (tpl:popuplogin) :left 720 :width 196)))
           (content   (format nil "~{~A~}"
                              (loop :for act :in acts #|:when (check-perm (getf act :perm) (cur-user) (getf act :val)) |# :collect
                                 (tpl:content-block
                                  (list :title (getf act :action)
                                        :content (show-act act)))))))
      (declare (special *popups*))
      (tpl:root
       (list
        :personal  personal
        :popups    *popups*
        :navpoints (menu)
        :content  content))))



(defun grid-helper (grid-id pager-id json-code)
  (format nil "<table id=\"~A\"></table><div id=\"~A\"></div>
               <script type=\"text/javascript\">
               jQuery('#~A').jqGrid(~A);
               jQuery('#~A').jqGrid('navGrid','#~A',{edit:false,add:false,del:false});
               </script>"
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
  "[debugged 29.08.2011]"
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
    (with-in-fld-case fields ;; infld variable
      :fld       (let ((perm (getf (getf infld :permlist) :show)))
                   (cond ((equal '(:str) (getf infld :typedata))
                          (let ((accessor  (find-symbol (format nil "A-~A" (getf infld :fld)) (find-package "WIZARD"))))
                            (push (cons accessor perm) field-cons)))
                         (t ;; default - print unknown type message in grid field
                          (let ((accessor  (lambda (x) "unknown typedata in grid pager")))
                            (push (cons accessor perm) field-cons)))))
      :btn       (let* ((perm      (getf infld :perm))
                        (btn       (getf infld :btn))                    ;; тут важно чтобы вычисление происходило вне лямбды
                        (value     (getf infld :value))                  ;; тут важно чтобы вычисление происходило вне лямбды
                        (accessor  (lambda (x)
                                     (format nil "<form method='post'><input type='submit' name='~A~~%|id|%' value='~A~~%|id|%' /></form>"
                                             btn
                                             value))))
                   (push (cons accessor perm) field-cons))
      :popbtn    (let* ((perm      (getf infld :perm))
                        (btn       (getf infld :popbtn))                 ;; тут важно чтобы вычисление происходило вне лямбды
                        (value     (getf infld :value))                  ;; тут важно чтобы вычисление происходило вне лямбды
                        (accessor  (lambda (x)
                                     (format nil "<input type='button' name='~A~~%|id|%' value='~A~~%|id|%' onclick='ShowHide(\"~A\")' />"
                                             btn
                                             value
                                             btn))))
                   (push (cons accessor perm) field-cons))
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
                      "permission denied in grid pager"))))
     ;; cnt-rows - two result
     cnt-rows)))


(defun example-json (val fields)
  (let* ((page            (- (parse-integer (hunchentoot:get-parameter "page")) 1))
         (rows-per-page   (parse-integer (hunchentoot:get-parameter "rows"))))
    (multiple-value-bind (slice cnt-rows)
        (pager val fields page rows-per-page)
      (json-assembly  (+ page 1)  (ceiling cnt-rows rows-per-page)  (length slice) slice))))
