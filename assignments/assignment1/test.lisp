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

