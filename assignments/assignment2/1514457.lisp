;; the assignment spec says the program will only be tested with valid programs and expressions
;; therefore I am assuming that functions are always called with the correct numbher of arguments
(defun fl-interp (E P)
  (cond
   ((atom E) E)   ;this includes the case where E is nil or a number
   (t
    (let ((f (car E))
          (arg (cdr E))
          (function-names (mapcar (lambda (x) (car x)) P)))
      (cond
       ;;; handle built-in functions
       ((eq f 'first)  (car (fl-interp (car arg) P)))
       ((eq f 'rest)   (cdr (fl-interp (car arg) P)))
       ((eq f 'cons)   (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f 'equal)  (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f 'number) (numberp (fl-interp (car arg) P)))
       ((eq f 'eq)  (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P))) ; same
       ((eq f 'atom) (atom (fl-interp (car arg) P)))
       ((eq f 'null) (null (fl-interp (car arg) P)))
       ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f '>) (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f '<) (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f '=) (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
       ((eq f 'not) (not (fl-interp (car arg) P)))
       ((eq f 'and) (fl-and E P arg))
       ((eq f 'or) (fl-or E P arg))
       ((eq f 'if) (fl-if E P arg))
       ;; user defined function
       ((equal f (car P))
        (fl-interp (lambda (cadr P) (cdddar P) arg) P))

       (t E))))))
                ;;;.....

                ;;; if f is a user-defined function,
                ;;;    then evaluate the arguments
                ;;;         and apply f to the evaluated arguments
                ;;;             (applicative order reduction)
                ;;;.....

                ;;; otherwise f is undefined (not intended to be a function),
                ;;; the E is returned, as if it is quoted in lisp
                ;;;
                ;;;

;; and is special.
;; exception to applicative order reduction
;; we need to evaluate the first argument first, then according to the result stop or continue with the second argument
(defun fl-and (E P arg)
  (if (not (fl-interp (car arg) P))
      nil
    (if (fl-interp (cadr arg) P)
        t
      nil)))

;; or is special.
;; exception to applicative order reduction
;; we need to evaluate the first argument first, then according to the result stop or continue evaluating the second argument
(defun fl-or (E P arg)
  (if (fl-interp (car arg) P)
      t
    (if (fl-interp (cadr arg) P)
        t
      nil)))

;; if is special
;; exception to applicative order reduction
;; evaluate the condition first, then according to the result either continue with the then part or the else part
(defun fl-if (E P arg)
  (if (fl-interp (car arg) P)
      (fl-interp (cadr arg) P)
    (fl-interp (caddr arg) P)))


;; test cases

(assert (eq (fl-interp '(+ 10 5) nil) 15)) ; > 15
(assert (eq (fl-interp '(- 12 8) nil) 4)) ; > 4
(assert (eq (fl-interp '(* 5 9) nil) 45)) ; > 45
(assert (eq (fl-interp '(> 2 3) nil) nil)) ; > nil
(assert (eq (fl-interp '(< 1 131) nil) 't)) ; > 't
(assert (eq (fl-interp '(= 88 88) nil) 't)) ; > 't
(assert (eq (fl-interp '(and nil t) nil) 'nil)) ; > 'nil
(assert (eq (fl-interp '(or t nil) 't)) 't) ; > 't
(assert (eq (fl-interp '(not t) nil) 'nil)) ; > 'nil
(assert (eq  (fl-interp '(number 354) nil) 't)) ; > 't
(assert (eq  (fl-interp '(equal (3 4 1) (3 4 1)) nil) 't)); > 't
(assert (eq  (fl-interp '(if nil 2 3) nil) 3)) ; > 3
(assert (eq  (fl-interp '(null ()) nil) 't)) ; > 't
(assert (eq  (fl-interp '(atom (3)) nil) 'nil)) ; > 'nil
(assert (eq  (fl-interp '(eq x x) nil) 't)) ; > 't
(assert (eq  (fl-interp '(first (8 5 16)) nil) 8)) ; > 8
(assert (eq  (fl-interp '(rest (8 5 16)) nil) '(5 16))) ; > '(5 16)
(assert (eq  (fl-interp '(cons 6 3) nil) '(6 . 3))); > '(6 . 3)

;; More complex = 2.5 total

(assert (eq (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) 12)) ; > 12
(assert (eq (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2)) (not (= 3 2)))) nil) 't)) ; > 't
(assert (eq (fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) 'nil)) ; > 'nil
(assert (eq (fl-interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) '((a c e) . d)) ; > '((a c e) . d)

;; User-defined (U) = 10 points total
;; Basic = 4 total

(assert (eq (fl-interp '(greater 3 5) '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) 5)) ; > 5
(assert (eq (fl-interp '(square 4) '((square (x) = (* x x)))) 16)) ; > 16
(assert (eq (fl-interp '(simpleinterest 4 2 5) '((simpleinterest (x y z) = (* x (* y z))))) 40 )); > 40
(assert (eq (fl-interp '(xor t nil) '((xor (x y) = (if (equal x y) nil t)))) 't)) ; > 't
(assert (eq (fl-interp '(cadr (5 1 2 7)) '((cadr(x) = (first (rest x))))) 1) ; > 1

;; More complex = 6 total
(assert (eq (fl-interp '(last (s u p)) '((last (x) = (if (null (rest x)) (first x) (last (rest x)))))) 'p)) ; > 'p
(assert (eq (fl-interp '(push (1 2 3) 4) '((push (x y) = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4))) ; > '(1 2 3 4)
(assert (eq (fl-interp '(pop (1 2 3)) '((pop (x) = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x) (pop (rest x))))))) '(1 2))) ; > '(1 2)
(assert (eq (fl-interp '(power 4 2) '((power (x y) = (if (= y 1) x (power (* x x) (- y 1)))))) '16)) ; > '16
(assert (eq (fl-interp '(factorial 4) '((factorial (x) = (if (= x 1) 1 (* x (factorial (- x 1))))))) 24)) ; > 24
(assert (eq (fl-interp '(divide 24 4) '((divide (x y) = (div x y 0)) (div (x y z) = (if (> (* y z) x) (- z 1) (div x y (+ z 1))))))) 6)) ; > 6
