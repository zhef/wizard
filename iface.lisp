(in-package #:wizard)

(defmacro def~fld (name &key (update nil update-p) (show nil show-p) (view nil view-p) (xref nil xref-p)  (width 200 width-p))
  (let ((initargs))
    (if update-p  (setf (getf initargs :update) update))
    (if view-p    (setf (getf initargs :view) view))
    (if show-p    (setf (getf initargs :show) show))
    (if xref-p    (setf (getf initargs :xref) xref))
    (if width-p   (setf (getf initargs :width) width))
    `(mi '~fld :title ',name ,@initargs)))

(defmacro def~btn ((title perm &key (width 200 width-p)) &body act)
  (let ((initargs))
    (if width-p   (setf (getf initargs :width) width))
    `(mi '~btn :title ,title :perm ,perm ,@initargs :act ',@act)))

(defmacro def~upl ((file perm name))
  `(mi '~upl :title ',file :perm ,perm :name ,name))

(defmacro def~pop ((title perm &key (height 100 height-p)  (width 200 width-p)) &body actions)
  (let ((initargs))
    (if height-p (setf (getf initargs :height) height))
    (if width-p  (setf (getf initargs :width)  width))
    `(mi '~pop :title ,title :perm ,perm ,@initargs :actions (list ,@actions))))

(defmacro def~grd ((title perm entity val &key (height 100 height-p)) &body fields)
  (let ((initargs))
    (if height-p (setf (getf initargs :height) height))
    `(mi '~grd :title ,title :perm ,perm :entity ',entity :val ',val ,@initargs :fields (list ,@fields))))

(defmacro def~blk ((title perm) &body contents)
  `(mi '~blk :title ,title :perm ,perm :contents (list ,@contents)))

(defmacro def~lin ((title perm entity val) &body fields)
  `(mi '~lin :title ,title :perm ,perm :entity ',entity :val ',val :fields (list ,@fields)))

(defmacro def~tpl ((tpl) &body val)
  `(mi '~tpl :title ,tpl :val ',@val))

(defmacro def~map ((yamap) &body val)
  `(mi '~map :title ,yamap :val ',@val))

(defmacro def~nop ((none))
  `(mi '~nop :title ,none))

(defmacro def~pst ((post entity val) &body fields)
  `(mi '~pst :title ,post :entity ',entity :val ',val :fields (list ,@fields)))

(defmacro def~ann ((announce entity val) &body fields)
  `(mi '~ann :title ,announce :entity ',entity :val ',val :fields (list ,@fields)))


(defmacro def~plc ((name url &key (navpoint nil navpoint-p)) &body actions)
  "TODO: -controllers-"
  `(let ((rs (list :place ',name :url ,url)))
     ,(if navpoint-p
          `(progn
             (nconc rs (list :navpoint ,navpoint))
             (if (boundp '-navpoints-) ;; if exists special var -navpoints- — save navpoint!
                 (nconc -navpoints- (list (list :link ,url :title ,navpoint))))))
     (nconc rs (list :actions (list 'quote (list ,@actions))))
     rs))

(defmacro def~asm (&body places)
  "TODO: -ajax-data-set-"
  `(let* ((-navpoints- (list 'dymmy)))
     (declare (special -navpoints-)) ;; special for menu
     (let ((-places- (list ,@(loop :for item :in places :collect item))))
       (defparameter *places* (remove-if #'null -places-))
       (defparameter *navpoints* (cdr -navpoints-)))))
