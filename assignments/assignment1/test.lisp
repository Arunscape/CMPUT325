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
