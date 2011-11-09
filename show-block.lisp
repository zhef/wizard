(in-package #:WIZARD)

(defgeneric show-block (param &key))

(defmethod show-block ((param t) &key)
  (error "no applicable method SHOW-BLOCK for ~A" (type-of param)))

(defmethod show-block ((param none) &key)
  (tpl:content-block
   (list :title (a-title param)
         :content "Раздел находится в разработке")))

(defmethod show-block ((param tpl) &key)
  (tpl:content-block
   (list :title (a-title param)
         :content (format nil "~A" (funcall (a-val param))))))

(defmethod show-block ((param grid) &key)
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-titles)
        (col-models)
        (col-replace)) ;; <-- не используется, но возможно пригодится в будущем
    (declare (special *popups*))
    (loop :for infld :in (a-fields param) :collect
       (multiple-value-bind (col-title col-model)
           (show-grid infld) ;; <-- dispatcher
         (unless (null col-title)
           (push col-title col-titles)
           (push col-model col-models))))
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
                                     ("colNames" . ,(reverse col-titles))
                                     ("colModel" . ,(reverse col-models))
                                     ("rowNum" . 10)
                                     ("rowList" . (10 20 30))
                                     ("pager" . ,(format nil "#~A" pager-id))
                                     ("sortname" . "id")
                                     ("viewrecords" . t)
                                     ("sortorder" . "desc")
                                     ("height" . ,(aif (a-height param) it "180"))
                                     ("editurl" . "/edit_url")
                                     ("gridComplete" . "-=|=-")
                                     ("caption" . ,(a-title param))
                                     ;; ("search" . ((caption . "Search")
                                     ;;              (Find . "Find")
                                     ;;              (Reset . "Reset")
                                     ;;              (odata . ("equal"  "not equal"))
                                     ;;              (matchText . " match")
                                     ;;              (rulesText . " rules")
                                     ;;              ))
                                     ))
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




(defmethod show-block ((param linear) &key)
  (let* ((val  (a-val param))
         (flds (loop :for infld :in (a-fields param) :collect
                  (show-linear infld val)))) ;; <-- dispatcher
    (tpl:frmobj (list :content (format nil "~{~A~}" flds)))))


(defmethod show-block ((param announce) &key)
  (format nil "~{~A~}"
          (loop :for (id . announce) :in (funcall (a-val param)) :append
             (list
              (tpl:postannounce (list :title (a-title announce)
                                  :date (a-date announce)
                                  :photoannounce (or (a-photo-announce announce) "")
                                  :announce (a-announce announce)
                                  :id id))))))


(defmethod show-block ((param post) &key)
  (let ((val (funcall (a-val param))))
    (tpl:posttext (list :title (a-title val)
                        :text (a-text val)
                        :date (a-date val)
                        :phototext (or (a-photo-text val) "")))))
