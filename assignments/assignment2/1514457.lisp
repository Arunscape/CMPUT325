;; Name: Arun Woosaree
;; Student Number: 1514457
;; Course: CMPUT 325
;; Section: B1
;; Assignment 2

;; What does this function do?
;; The main part of the fl-interpreter
;; It interprets programs written in the fl language defined in the assignment specification
;;
;; How does the function work?
;; first we check if the given expression is an atom. This handles the case where it is nil, or a number
;; next, a bunch of primitives are defined, as defined in the assignment specification.
;;
;; if the given expression does not match any of the primitives, we check if it is a user defined function
;; the get-usrfunc function returns nil if no user functions are found with the given name
;; in this case, we just return the expression
;;
;; if a function is found with the same name and arity, we replace the arguments with the given values
;; we also make sure to interpret the arguments before doing the replacement, so that we are doing
;; applicative order reduction
;; finally, we interpret the result of replacing all these arguments and return that
;;
;; Test Cases
;;
(assert (equal (fl-interp '(+ 10 5) nil) 15))
(assert (equal (fl-interp '(- 12 8) nil) 4))
(assert (equal (fl-interp '(* 5 9) nil) 45))
(assert (equal (fl-interp '(> 2 3) nil) nil))
(assert (equal (fl-interp '(< 1 131) nil) 't))
(assert (equal (fl-interp '(= 88 88) nil) 't))
(assert (equal (fl-interp '(and nil t) nil) 'nil))
(assert (equal (fl-interp '(or t nil) nil) 't))
(assert (equal (fl-interp '(not t) nil) 'nil))
(assert (equal (fl-interp '(number 354) nil) 't))
(assert (equal (fl-interp '(equal (3 4 1) (3 4 1)) nil) 't))
(assert (equal (fl-interp '(if nil 2 3) nil) 3))
(assert (equal (fl-interp '(null ()) nil) 't))
(assert (equal (fl-interp '(atom (3)) nil) 'nil))
(assert (equal (fl-interp '(eq x x) nil) 't))
(assert (equal (fl-interp '(first (8 5 16)) nil) 8))
(assert (equal (fl-interp '(rest (8 5 16)) nil) '(5 16)))
(assert (equal (fl-interp '(cons 6 3) nil) '(6 . 3)))
(assert (equal (fl-interp '(rest (1 2 (3))) nil) '(2 (3))))
(assert (equal (fl-interp '(rest (p 1 2 (3))) nil) '(1 2 (3))))
(assert (equal (fl-interp '(eq (< 3 4) (eq (+ 3 4) (- 2 3))) nil) nil))
(assert (equal (fl-interp '(if (> 1 0) (+ 1 2) (+ 2 3)) nil) 3))
(assert (equal (fl-interp '(if (> 1 0) (if (eq 1 2) 3 4) 5)  nil) 4))
(assert (equal (fl-interp '(cons (first (1 2 3))  (cons a nil)) nil) '(1 a)))
(assert (equal (fl-interp '(and (or T  nil) (> 3 4)) nil) nil))
(assert (equal (fl-interp '(eq (1 2 3) (1 2 3)) nil) nil))
(assert (equal (fl-interp '(equal (1 2 3) (1 2 3)) nil) t))
;; More complex
(assert (equal (fl-interp '(+ (* 2 2) (* 2 (- (+ 2 (+ 1 (- 7 4))) 2))) nil) 12)) ; > 12
(assert (equal (fl-interp '(and (> (+ 3 2) (- 4 2)) (or (< 3 (* 2 2)) (not (= 3 2)))) nil) 't)) ; > 't
(assert (equal (fl-interp '(or (= 5 (- 4 2)) (and (not (> 2 2)) (< 3 2))) nil) 'nil)) ; > 'nil
(assert (equal (fl-interp '(if (not (null (first (a c e)))) (if (number (first (a c e))) (first (a c e)) (cons (a c e) d)) (rest (a c e))) nil) '((a c e) . d))) ; > '((a c e) . d)

;; User-defined (U)
(assert (equal (fl-interp '(greater 3 5) '((greater (x y) = (if (> x y) x (if (< x y) y nil))))) 5)) ; > 5
(assert (equal (fl-interp '(square 4) '((square (x) = (* x x)))) 16)) ; > 16
(assert (equal (fl-interp '(simpleinterest 4 2 5) '((simpleinterest (x y z) = (* x (* y z))))) 40 )); > 40
(assert (equal (fl-interp '(xor t nil) '((xor (x y) = (if (equal x y) nil t)))) 't)) ; > 't
(assert (equal (fl-interp '(cadr (5 1 2 7)) '((cadr(x) = (first (rest x))))) 1)) ; > 1
;; More complex
(assert (equal (fl-interp '(last (s u p)) '((last (x) = (if (null (rest x)) (first x) (last (rest x)))))) 'p)) ; > 'p
(assert (equal (fl-interp '(push (1 2 3) 4) '((push (x y) = (if (null x) (cons y nil) (cons (first x) (push (rest x) y)))))) '(1 2 3 4))) ; > '(1 2 3 4)
(assert (equal (fl-interp '(pop (1 2 3)) '((pop (x) = (if (atom (rest (rest x))) (cons (first x) nil) (cons (first x) (pop (rest x))))))) '(1 2))) ; > '(1 2)
(assert (equal (fl-interp '(power 4 2) '((power (x y) = (if (= y 1) x (power (* x x) (- y 1)))))) '16)) ; > '16
(assert (equal (fl-interp '(factorial 4) '((factorial (x) = (if (= x 1) 1 (* x (factorial (- x 1))))))) 24)) ; > 24
(assert (equal (fl-interp '(divide 24 4) '((divide (x y) = (div x y 0)) (div (x y z) = (if (> (* y z) x) (- z 1) (div x y (+ z 1)))))) 6)) ; > 6
(assert (equal (fl-interp '(f (g 2) (g 1)) '((f (X Y) =  (+ X Y)) (g (X) = (+ 1 X)))) 5))
(assert (equal (fl-interp '(count (1 2 3)) '((count (L) = (if (null L) 0 (+ 1 (count (rest L)))))))3))
;; call function which uses another function
(assert (equal (fl-interp '(reverse (1 2 3)) '((reverse (X) =  (if (null X) nil(append (reverse (rest X))(cons (first X) nil))))(append (X Y) = (if (null X)Y(cons (first X) (append (rest X) Y)))))) '(3 2 1)))
;; higher order function
(assert (equal (fl-interp '(mapcar plus1 (1 2 3)) ' ((mapcar (F L) = (if (null L) nil (cons (F (first L)) (mapcar F (rest L))))) (plus1 (X) = (+ X 1)))) '(2 3 4)))
;; different function arities
(assert (equal (fl-interp '(f 1 2 (f 3 4)) '((f (x y) = (- x y))(f (x y z) = (+ x (+ y z))))) 2))

;; warning: won't terminate since we're doing applicative order
(fl-interp '(h (g 5)) '(  (g (X) = (g (g X))) (h (X) = 1)))
;; warning: won't terminate since we're doing applicative order
(fl-interp '(f 0 (g 1)) '((g (X) = (+ X (g (+ X 1))))(f (X Y) = (if (eq X 0) 0 Y))))

(defun fl-interp (E P)
  (cond
    ((atom E) E)   ;this includes the case where E is nil or a number
    (t
     (let ((f (car E))
           (arg (cdr E)))
       (cond
       ;;; handle built-in functions
         ((eq f 'first)  (car (fl-interp (car arg) P)))
         ((eq f 'rest)   (cdr (fl-interp (car arg) P)))
         ((eq f 'cons)   (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
         ((eq f 'equal)  (if (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P)) t nil))
         ((eq f 'number) (if (numberp (fl-interp (car arg) P)) t nil))
         ((eq f 'eq)  (if (eq (fl-interp (car arg) P) (fl-interp (cadr arg) P)) t nil)) ; same
         ((eq f 'atom) (if (atom (fl-interp (car arg) P)) t nil))
         ((eq f 'null) (if (null (fl-interp (car arg) P)) t nil))
         ((eq f '+) (+ (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
         ((eq f '-) (- (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
         ((eq f '*) (* (fl-interp (car arg) P) (fl-interp (cadr arg) P)))
         ((eq f '>)  (if (> (fl-interp (car arg) P) (fl-interp (cadr arg) P)) t nil))
         ((eq f '<) (if (< (fl-interp (car arg) P) (fl-interp (cadr arg) P)) t nil))
         ((eq f '=) (if (= (fl-interp (car arg) P) (fl-interp (cadr arg) P)) t nil))
         ((eq f 'not) (if (fl-interp (car arg) P) nil t))
         ((eq f 'and) (fl-and P arg))
         ((eq f 'or) (fl-or P arg))
         ((eq f 'if) (fl-if P arg))
         ((get-usrfunc f arg P)
          (let ((matchingfunc (get-usrfunc f arg P)))
            (fl-interp (replace-args (cadr matchingfunc) (mapcar (lambda (x) (fl-interp x P)) arg) (cadddr matchingfunc))P)))

         (t E))))))

;; What does this function do?
;; and is an exception to the applicative order reduction we are doing
;; we need to evaluate the first argument first, then according to the result stop or continue with the second argument
(defun fl-and (P arg)
  (if (not (fl-interp (car arg) P))
      nil
      (if (fl-interp (cadr arg) P)
          t
          nil)))

;; What does this function do?
;; or is an exception to the applicative order reduction we are doing
;; we need to evaluate the first argument first, then according to the result stop or continue with the second argument
(defun fl-or (P arg)
  (if (fl-interp (car arg) P)
      t
      (if (fl-interp (cadr arg) P)
          t
          nil)))

;; What does this function do?
;; if is an exception to the applicative order reduction we are doing
;; we need to evaluate the first argument first, then according to the result stop or continue with the second argument
(defun fl-if (P arg)
  (if (fl-interp (car arg) P)
      (fl-interp (cadr arg) P)
      (fl-interp (caddr arg) P)))

;; What does this function do?
;; it filters items in a list that match a criteria defined by a user defined function
;;
;; How does the function work?
;; it goes through each element of L and tests if the function test returns true for each given element
;; if so, it stays in
;; if not, we ignore it
;;
;; Test cases
;;(assert (equal (xfilter (lambda (x) (eq (mod x 2) 0)) '(1 2 3 4 5 6 7 8 9)) '(2 4 6 8)))
(defun xfilter (test L)
  (cond
    ((null L) nil)
    ((funcall test (car L)) (cons (car L) (xfilter test (cdr L))))
    (t (xfilter test (cdr L)))))

;; What does this function do?
;; it gets the first user defined function that has the same name and arity in the program
;; if it cannot be found, we return nil
;;
;; How does the function work?
;; it uses the xfilter function to filter out user defined functions that don't
;; have the same name or arity, and then returns the first function that matches
;; if the filtered list is empty, car returns nil
;;
;; Test cases
(assert (equal (get-usrfunc 'f '(1 2 3 4) '((g (x) = (+ 1 x)) (f (w x y z) = (+ w (+ x (+ y z)))))) '(f (w x y z) = (+ w (+ x (+ y z))))))
(assert (equal (get-usrfunc 'doesntexist '(1 2 3 4) '((g (x) = (+ 1 x)) (f (w x y z) = (+ w (+ x (+ y z)))))) nil))
(defun get-usrfunc (f args P)
  (car (xfilter (lambda (fun) (and (equal f (car fun)) (eq (len (cadr fun)) (len args)))) P)))

;; should be self explanatory, but in case it's not:
;; this gives the length of a list by going over each element in it and adding one
;; when the list is empty, we return 0 and add the rest of the ones together
(defun len (L)
  (if (null L) 0
      (+ 1 (len (cdr L)))))

;; What does this function do?
;; it returns T if X is a member of Y. If not, it returns NIL
;;
;; How does the function work?
;; it goes through each element of Y and tests if X is equal to Y
;; if so, it returns true
;; if not, it checks the next element until there are no more to check
;; if none of the elements in Y are equal to X, NIL is returned
;;
;; this also tests for lists being members of lists
;; both X and Y may be NIL, or lists containing NIL
;;
;; Test cases
(assert (equal (xmember '1 '(1)) 't))
(assert (equal (xmember '1 '( (1) 2 3))NIL))
(assert (equal (xmember '(1) '((1) 2 3)) T))
(assert (equal (xmember nil nil) NIL))
(assert (equal (xmember nil '(nil)) T))
(assert (equal (xmember nil '((nil))) NIL))
(assert (equal (xmember '(nil) '(1 2 3 (nil))) T))
(assert (equal (xmember '(nil) '(nil)) NIL))
(defun xmember (X Y)
  (cond
    ((not Y) nil)
    ((equal X (car Y)) t)
    (t (xmember X (cdr Y)))))


;; What does this function do?
;; it takes the program body, and replaces every instance of the argument with its given value
;;
;; How does the function work?
;; it maps each element in the program body to either itself, or replaced with the value given if the argument matches
(defun replace-arg (arg val p)
  (if (atom p) p
      (mapcar (lambda (x) (cond
                            ((equal x arg) val)
                            ((not (atom x)) (replace-arg arg val x))
                            (t x)))
              p)))


;; What does this function do?
;; it takes the program body, and replaces every instance of all the arguments given to their corresponding values
;;
;; How does the function work?
;; it goes over each argument and its corresponding value, and uses the replace-arg function to replace
;; every instance of that argument with its corresponding value
(defun replace-args (args vals p)
  (cond
    ((null args) p)
    (t (replace-args (cdr args) (cdr vals) (replace-arg (car args) (car vals) p)))))
