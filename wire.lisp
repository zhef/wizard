(in-package #:wizard)

;; base class el & generics

(defmacro with-defel ((class-name super-class-names) &body slots)
  `(prog1
       (defclass ,class-name ,super-class-names
         ,(loop :for (slot-name initform) :in slots :collect
             `(,slot-name :initarg  ,(intern (symbol-name slot-name) :keyword)
                          :initform ,initform
                          :accessor ,(intern (format nil "A-~A" (symbol-name slot-name))))))
     (defmethod print-object ((obj ,class-name) stream)
       (format stream
               (format nil "#[ ~A | ~A]"
                       ',class-name
                       (loop :for slot :in (closer-mop:class-slots (find-class ',class-name)) :collect
                          (format nil ":~A ~A"
                                  (closer-mop:slot-definition-name slot)
                                  "";; (bprint (funcall (intern (format nil "A-~A" (symbol-name (closer-mop:slot-definition-name slot)))) obj))
                                  )))))))

(with-defel (el ()))

(defgeneric lnk (src dst)
  (:documentation "link objects"))

(defgeneric act (obj voltage)
  (:documentation "change state obj"))


;; class wire

(with-defel (wire (el))
  (in  nil)
  (out nil)
  (voltage 0))

(defmethod act ((obj wire) voltage)
  (setf (a-voltage obj) voltage)
  (unless (null (a-out obj))
    (act (a-out obj) voltage)))

(defmethod lnk ((sock socket) (wire wire))
  (setf (a-out sock) wire)
  (setf (a-in wire) sock)
  nil)



;; class resistor

(with-defel (resistor (el))
  (in           nil)
  (out          nil)
  (resistance   0)
  (power        0)
  (voltage-in   0)
  (voltage-out  0))



;; test

(defparameter *start-socket* (mi 'socket))
(defparameter *start-wire* (mi 'wire))
(lnk *start-socket* *start-wire*)

(act *start-socket* 5)
(a-voltage *start-socket*)
(a-voltage *start-wire*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-defel (wire (el)))

(defparameter a (mi 'wire))
(defparameter b (mi 'wire))
(defparameter c (mi 'wire))
(defparameter d (mi 'wire))
(defparameter e (mi 'wire))
(defparameter s (mi 'wire))


(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(defun half-adder (a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(defun full-adder (a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun logical-not (s)
  "for inverter"
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Неправильный сигнал" s))))

(defun inverter (input output)
  "link between wires"
  (add-action input
              #'(named-lambda invert-input ()
                  (let ((new-value (logical-not (get-signal input))))
                    (after-delay inverter-delay
                                 #'(named-lambda ()
                                       (setf (a-signal-value output) new-value)))))))

(defun and-gate (a1 a2 output)
  "and-gate"
  (let ((and-action-procedure (named-lambda and-action-procedure ()
                                (let ((new-value (logical-and (a-signal-value a1)
                                                              (a-signal-value a2))))
                                  (after-delay and-gate-delay
                                               #'(named-lambda ()
                                                     (setf (a-signal-value output) new-value)))))))
    (add-action a1 and-action-procedure)
    (add-action a2 and-action-procedure)
    'ok))

;;;;;;;;;;;;;;;;;;;; debugged wires

(with-defel (wire (el))
  (signal-value 0)
  (action-procedures '()))

(defun call-each (procedures)
  "recursive message passing"
  (if (null procedures)
      'done
      (progn
        (funcall (car procedures))
        (call-each (cdr procedures)))))

(defmethod (setf a-signal-value) (new-val (obj wire))
  "set-my-signal!"
  (if (not (= (a-signal-value obj) new-val))
      (progn
        (setf (slot-value obj) new-val)
        (call-each (a-action-procedures obj))))
  'done)

(defmethod add-action ((obj wire) proc)
  (setf (a-action-procedures obj)
        (append (a-action-procedures obj)
                (list proc)))
  (funcall proc))

;;;;;;;;;;;;;;;;;;;

(with-defel (agenda (el))
  (cur-time 0)
  (actions nil))

(defmethod is-empty ((obj agenda))
  (if (null (a-actions obj)) t nil))

(defmethod get-first ((obj agenda))
  (car (a-actions obj)))

(defmethod del-first ((obj agenda))
  (pop (a-actions obj)))

(defmethod add-agenda ((obj agenda) time action)
  (push (a-actions obj)
  )

(defparameter *the-agenda* (mi 'agenda))

(defmethod after-delay ((obj agenda) delay action)
  (add-agenda obj (+ delay (a-cur-time obj)) delay action))

(defmethod propagate ((obj agenda))
  (if (is-empty obj)
      'done
      (let ((first-item (get-first obj)))
        (funcall first-item)
        (del-first obj)
        (propagate obj))))






;; Тестер говорит проводу, что, каждый раз, когда сигнал
;; изменяет значение, нужно напечатать новое значение сигнала, а также текущее время и
;; имя провода:

(defun probe (name wire)
  (add-action! wire
               (lambda ()
                 (print)
                 (print name)
                 (print " ")
                 (print (current-time the-agenda))
                 (print " New-value = ")
                 (print (get-signal wire)))))


;; Сначала мы инициализируем план действий и указываем задержки для элементарных
;; функциональных элементов:

(defun the-agenda (make-agenda))
(defun inverter-delay 2)
(defun and-gate-delay 3)
(defun or-gate-delay 5)


;; Затем мы создаем четыре провода и к двум из них подсоединяем тестеры:
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe ’sum sum)
;; sum 0 New-value = 0

(probe ’carry carry)
;; carry 0 New-value = 0


;; Затем мы связываем провода, образуя схему полусумматора (как на рис. 3.25), устанав-
;; ливаем сигнал на входе input-1 в 1, и запускаем модель:
(half-adder input-1 input-2 sum carry)
;; ok
(set-signal! input-1 1)
;; done
(propagate)
;; sum 8 New-value = 1
;; done







