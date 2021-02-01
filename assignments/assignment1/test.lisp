(load "assignment1.lisp")

(defun test-case (ID Test Result)
  (if (equal Test Result)
      (format t "Test ~S OK~%" ID)
      (format t "FAIL: Test ~S expected ~S got ~S~%" ID Result Test)
      )
  )

(test-case 1.1(xmember '1 '(1))T)
(test-case 1.2(xmember '1 '( (1) 2 3))NIL)
(test-case 1.3(xmember '(1) '((1) 2 3)) T)
(test-case 1.4(xmember nil nil) NIL)
(test-case 1.5(xmember nil '(nil)) T)
(test-case 1.6(xmember nil '((nil))) NIL)
(test-case 1.7(xmember '(nil) '(1 2 3 (nil))) T)
(test-case 1.8(xmember '(nil) '(nil)) NIL)

(test-case 2.1(flatten '(a (b c) d)) '(a b c d))
(test-case 2.2(flatten '((((a))))) '(a))
(test-case 2.3(flatten '(a (b c) (d ((e)) f))) '(a b c d e f))

(test-case 3.1 (remove-duplicate '(a b c a d b)) '(c a d b))

(test-case 4.1 (mix '(a b c) '(d e f)) '(a d b e c f))
(test-case 4.2 (mix '(1 2 3) '(a)) '(1 a 2 3))
(test-case 4.3 (mix '((a) (b c)) '(d e f g h)) '((a) d (b c) e f g h))
(test-case 4.4 (mix '(1 2 3) nil) '(1 2 3))
(test-case 4.5 (mix '(1 2 3) '(nil)) '(1 nil 2 3))
(test-case 4.6 (mix '(1 3 5) '(2 4 6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
(test-case 4.7 (mix '(2 4 6 7 8 9) '(1 3 5)) '(2 1 4 3 6 5 7 8 9))

(test-case 5.1 (allsubsets nil) '(nil))
(test-case 5.2 (allsubsets '(a)) '((a) nil))
(test-case 5.3 (allsubsets '(a b)) '((a b) (b) (a)  nil))
(test-case 5.4 (allsubsets '(a b c)) '((a b c) (b c) (a c) (c) (a b) (b) (a) () ))


(test-case 6.11 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada))) '(SHOPIFY AIRCANADA))
(test-case 6.12 (reached 'google '( (google shopify) (shopify amazon) (amazon google) ) ) '(amazon shopify))
(test-case 6.13 (reached 'google '( (google shopify) (shopify amazon) (amazon indigo)  )) '(INDIGO amazon shopify))
(test-case 6.14 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) ))  '(SHOPIFY delta AIRCANADA))
(test-case 6.14 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) ))  '(SHOPIFY delta AIRCANADA))
;;backwards reached
(test-case 6.15 (reached 'google '((shopify aircanada) (google shopify))) '(aircanada shopify))
;; cycle
(test-case 6.16 (reached 'a '((a b) (b a) (a c))) '(b c))


(defun test-rank (S L)
  (mapcar (lambda (x) (list (car x) (cdr x)))
          (mySort (get-rank S L))))

(test-case 6.21 (test-rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) '((AIRCANADA 2) (SHOPIFY 1) (GOOGLE 0) (AMAZON 0)))
(test-case 6.22 (test-rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) '((GOOGLE 1) (SHOPIFY 1) (AMAZON 1)))
(test-case 6.23 (test-rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) '((SHOPIFY 1) (AMAZON 1) (INDIGO 1) (GOOGLE 0)))
(test-case 6.24 (test-rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) '((AIRCANADA 2) (SHOPIFY 1) (DELTA 1) (GOOGLE 0) (AMAZON 0)))
;; multiple references don't count and references to self don't count
(test-case 6.25 (test-rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (aircanada aircanada) (aircanada delta) (google google))) '((AIRCANADA 2) (SHOPIFY 1) (DELTA 1) (GOOGLE 0) (AMAZON 0)))

(test-case 6.221 (rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) '(AIRCANADA SHOPIFY GOOGLE AMAZON))
(test-case 6.222 (rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) '(GOOGLE SHOPIFY AMAZON))
(test-case 6.223 (rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) '(SHOPIFY AMAZON INDIGO GOOGLE))
(test-case 6.224 (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) '(AIRCANADA SHOPIFY DELTA GOOGLE AMAZON))
