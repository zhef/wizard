(in-package :wizard)

;; activate
;; example-json
;; show-acts

(defclass wizard-render () ()) ;; default
(defclass action-render () ()) ;; actions: none, tpl, grid, linear, announce, yamap, file.

(setf *default-render-method* (mi 'wizard-render))

;; RENDER DISPATCHER (ACTS)
(defmethod restas:render-object ((designer wizard-render) (acts list))
  (let* ((*popups*  (list (list :id "popupLogin" :title "Вход" :content (tpl:popuplogin) :left 720 :width 196)))
         (personal  (let ((userid (hunchentoot:session-value 'userid)))
                      (if (null userid)
                          (tpl:loginform)
                          (tpl:logoutform (list :user (a-name (gethash userid *USER*))
                                                :usertype (string-downcase (type-of (gethash userid *USER*)))
                                                :userid userid))))))
    (declare (special *popups*))
    (tpl:root
     (list
      :mapkey    *mapkey*
      :popups    *popups*
      :personal  personal
      :right (if (eql 1 (length (request-list))) ;; main page
                 (tpl:right)
                 "")
      :navpoints (menu)
      :searchcategory (aif (hunchentoot:post-parameter "searchcategory") it "")
      :searchstring   (aif (hunchentoot:post-parameter "searchstring") it "")
      :content  (format nil "~{~A~}"
                        (loop :for act :in acts :collect
                           (if (check-perm (a-perm act) (cur-user) (a-val act))
                               (restas:render-object (mi 'action-render) act)
                               "")))))))

;; before start-session
(defmethod restas:render-object :before ((designer wizard-render) (acts list))
  (hunchentoot:start-session))


;; ACT: default (error)
(defmethod restas:render-object ((designer action-render) (obj action))
  (error "no applicable render method for subclass of ACTION: ~A" (type-of obj)))


;; ACT: none
(defmethod restas:render-object ((designer action-render) (obj none))
  (tpl:content-block
   (list :title (a-title obj)
         :content "Раздел находится в разработке")))


;; ACT: tpl
(defmethod restas:render-object ((designer action-render) (obj tpl))
  (tpl:content-block
   (list :title (a-title obj)
         :content (format nil "~A" (funcall (a-val obj))))))


;; ACT: grid
(defmethod restas:render-object ((designer action-render) (obj grid))
  (let ((grid-id  (gensym "J"))
        (pager-id (gensym "P"))
        (col-titles)
        (col-models)
        (col-replace)) ;; <-- не используется, но возможно пригодится в будущем
    (declare (special *popups*))
    (loop :for infld :in (a-fields obj) :collect
       (multiple-value-bind (col-title col-model)
           (show-grid infld) ;; <-- dispatcher
         (unless (null col-title)
           (push col-title col-titles)
           (push col-model col-models))))
    (tpl:content-block
     (list :title   (a-title obj)
           :content (tpl:gridview
                     (list :idgrid grid-id
                           :idpager pager-id
                           :json (replace-all
                                  (json:encode-json-to-string
                                   `(("url" . ,(format nil "/~A~A" (a-grid obj) (if (a-param-id obj) (format nil "/~A" (cur-id)) ""))) ;; absolute uri
                                     ("datatype" . "json")
                                     ("colNames" . ,(reverse col-titles))
                                     ("colModel" . ,(reverse col-models))
                                     ("rowNum" . 10)
                                     ("rowList" . (10 20 30))
                                     ("pager" . ,(format nil "#~A" pager-id))
                                     ("sortname" . "id")
                                     ("viewrecords" . t)
                                     ("sortorder" . "desc")
                                     ("height" . ,(aif (a-height obj) it "180"))
                                     ("editurl" . "/edit_url")
                                     ("gridComplete" . "-=|=-")
                                     ("caption" . ,(a-title obj))
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
                     grid-id in-name btn-str))))))))))


;; ACT: linear
(defmethod restas:render-object ((designer action-render) (obj linear))
  (let* ((val  (a-val obj))
         (flds (loop :for infld :in (a-fields obj) :collect
                  (show-linear infld val)))) ;; <-- dispatcher
    (tpl:frmobj (list :content (format nil "~{~A~}" flds)))))


;; ACT: announce
(defmethod restas:render-object ((designer action-render) (obj announce))
  (format nil "~{~A~}"
          (loop :for (id . announce) :in (funcall (a-val obj)) :append
             (list
              (tpl:postannounce (list :title (a-title announce)
                                      :date (a-date announce)
                                      :photoannounce (or (a-photo-announce announce) "")
                                      :announce (a-announce announce)
                                      :id id))))))

;; ACT: post
(defmethod restas:render-object ((designer action-render) (obj post))
  (let ((val (funcall (a-val obj))))
    (tpl:posttext (list :title (a-title val)
                        :text (a-text val)
                        :date (a-date val)
                        :phototext (or (a-photo-text val) "")))))


;; ACT: yamap
(defmethod restas:render-object ((designer action-render) (obj yamap))
  (tpl:map (list :center (a-center-coord obj)
                 :placemarks (format nil "~{~A~}"
                                     (mapcar #'(lambda (point)
                                                 (tpl:placemark
                                                  (list :title (a-title point)
                                                        :coord (a-coord point)
                                                        :descr (a-descr point))))
                                             (a-mark-points obj))))))


;; ACT: file
(defmethod restas:render-object ((designer action-render) (obj file))
  (tpl:fld
   (list :fldname (a-value obj)
         :fldcontent (tpl:fileupd (list :name (a-name obj))))))


;; ACT: #'function

;; (defmethod restas:render-object ((designer wizard-render) (func function))
;;   (funcall func))


