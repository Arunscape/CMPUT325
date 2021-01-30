(load "assignment1.lisp")

(defun reached (x L)
  (find-links x L L nil))


(defun find-links (x L original acc)
  (cond
    ((null L)
     acc)
    ((and
      (equal x (caar L))
      (not (xmember x acc)))
     (find-links (cadar L) original original (cons (cons (cadar L) acc) (find-links x (cdr L) original nil)))) ; found a match, add to acc
    (t
     (find-links x (cdr L) original acc)))) ; continue iteration


(defun test-case (ID Test Result)
  (if (equal Test Result)
      (format t "Test ~S OK~%" ID)
      (format t "FAIL: Test ~S expected ~S got ~S~%" ID Result Test)
      )
  )



(test-case 'backwards (reached 'google '( (shopify aircanada) (google shopify))) '(shopify aircanada))
(test-case 6.11 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada))) '(SHOPIFY AIRCANADA))
(test-case 6.12 (reached 'google '( (google shopify) (shopify amazon) (amazon google) ) ) '(SHOPIFY AMAZON))
(test-case 6.13 (reached 'google '( (google shopify) (shopify amazon) (amazon indigo)  )) '(SHOPIFY AMAZON INDIGO))
(test-case 6.14 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) ))  '(SHOPIFY AIRCANADA DELTA))

