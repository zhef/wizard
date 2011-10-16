(in-package #:wizard)

(defparameter *list-of-hashes* '(
                                 user
                                 post
                                 sale
                                 supplier-resource
                                 category
                                 resource
                                 resource-price
                                 price-reference
                                 tender
                                 tender-resource
                                 offer
                                 offer-resource
                                 document
                                 ))

(defun store ()
  (loop :for hash :in *list-of-hashes* :collect
     (cl-store:store
      (symbol-value (intern (format nil "*~A*"(symbol-name hash))))
      (format nil "z_~A.bin" hash))))

(defun restore ()
  (loop :for hash :in *list-of-hashes* :collect
     (setf
      (symbol-value (intern (format nil "*~A*_"(symbol-name hash))))
      (cl-store:restore (format nil "z_~A.bin" hash)))))



(defclass TEST-1 (entity)
  ((NAME             :initarg :NAME                :initform nil :accessor A-NAME)
   (IN               :initarg :IN                  :initform nil :accessor A-IN)))

(defclass TEST-2 (entity)
  ((NAME             :initarg :NAME                :initform nil :accessor A-NAME)))




(defparameter *test-1* (make-hash-table :test #'equal))
(defparameter *test-2* (make-hash-table :test #'equal))

(setf (gethash 1 *test-2*) (make-instance 'TEST-2 :name "this is inner"))
(setf (gethash 1 *test-1*) (make-instance 'TEST-1
                                          :name "this is outer"
                                          :in (gethash 1 *test-2*)
                                          ))

(a-name (a-in (gethash 1 *test-1*)))

(cl-store:store *test-1* "z_test-1.bin")
(cl-store:store *test-2* "z_test-2.bin")

(clrhash *test-1*)
(clrhash *test-2*)

(a-name (a-in (gethash 1 *test-1*)))

(setf *test-1* (cl-store:restore  "z_test-1.bin"))
(setf *test-2* (cl-store:restore  "z_test-2.bin"))

(setf (a-name (a-in (gethash 1 *test-1*))) "this")

(a-name (gethash 1 *test-2*))




;; (store)

(hash-table-count *resource-price*)

(defun resource-price ()
  (clrhash *PRICE-REFERENCE*)
  (clrhash *RESOURCE-PRICE*)
  ;; Забираем все справочники
  (with-query-select ("SELECT |:::| FROM `jos_gt_resource_price_level`"
                      ("id" "level" "title"))
    ;; Создаем справочник
    (let ((reference-id id)
          (this-price-reference (setf (gethash id *PRICE-REFERENCE*)
                                      (make-instance 'PRICE-REFERENCE
                                                     :date level
                                                     :name title
                                                     :resource-prices nil))))
      ;; (format t "~& ~A | ~A"  (a-date this-price-reference) (a-name this-price-reference)) ;;
      ;; Забираем все цены для этого справочника
      (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource_price` WHERE `level_id` = ~A" reference-id)
                          ("id" "code_id" "estimate" "wholesale" "level_id"))
        ;; Создаем цену, связывая ее со справочником
        (let ((this-resource-price (setf (gethash id *RESOURCE-PRICE*)
                                         (make-instance 'RESOURCE-PRICE
                                                        :estimate estimate
                                                        :wholesale wholesale
                                                        :price-level this-price-reference
                                                        :resource nil))))
          ;; Связываем справочник с созданной ценой
          (append-link (a-resource-prices this-price-reference) this-resource-price)
          ;; По code_id получаем код ресурса
          (with-query-select ((format nil "SELECT |:::| FROM `jos_gt_resource_code` WHERE `code`=~A" code_id)
                              ("resource_id"))
            ;; По коду ресурса получаем ресурс, с которым связываем цену
            (let ((this-resource (gethash resource_id *RESOURCE*)))
              (when this-resource ;; иногда бывает nil, что вероятно связано с неполным дампом
                (setf (a-resource this-resource-price) this-resource)
                ;; Добавляем цену к ресурсу
                (append-link (a-resource-prices this-resource) this-resource-price))
              t)t)t)t)t)t)t)


;; tests
;; (hash-table-count *PRICE-REFERENCE*)

;; tests
;; (maphash #'(lambda (k v)
;;              (print (list k (a-name v)))
;;              (print (length (a-resource-prices v)))
;;              (loop :for a :in (a-resource-prices v) :do
;;                 (when (a-resource a)
;;                   (print (list (a-estimate a)
;;                                (a-wholesale a)
;;                                (a-price-level a)
;;                                (a-name (a-resource a))
;;                                (a-name (a-category (a-resource a)))))
;;                   (return))))
;;          *PRICE-REFERENCE*)

(resource-price)


#|

Когда забили ресурсы в тендер система находит всех поставщиков этих ресурсов
и заносит их в tender_supplier, где создатель тендера может некоторых их них удалить или добавить своего
в этой таблице is_invited - мемоизация, не переносить

tender_offer - это приглашение на тендер поставщику

Поставщик в ответ на приглашение может создать заявку (tender_order), которая через offer_id привязана к приглашению
status - это для того чтобы можно было заявку отменить

При создании заявки, создаются записи в tender_order_resource, где
order_id - заявка
tender_resource_id - запись в таблице tender_resource, чтобы увидеть рекомендуемую цену, обьем итд

ФИНАЛЬНЫЕ ТАбЛИЦЫ - перечень ресурсов : поставщики

#|
