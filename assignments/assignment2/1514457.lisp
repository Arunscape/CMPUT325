(defun fl-interp (E P)
  (cond
    ((atom E) E)   ;this includes the case where E is nil or a number
    (t
     (let ( (f (car E))  (arg (cdr E)))
       (cond
                ;;; handle built-in functions
         ((eq f 'first)  (car (fl-interp (car arg) P)))
         ((eq f 'rest)   (cdr (fl-interp (car arg) P)))
         ((eq f 'cons)   (cons (fl-interp (car arg) P) (fl-interp (cadr arg) P))) ; todo handle the case of 3 or more arguments or 1 or 0 arguments
         ((eq f 'equal)  (equal (fl-interp (car arg) P) (fl-interp (cadr arg) P))) ; same
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
         ((eq f 'and) (and (fl-interp (car arg) P) (fl-interp (cadr arg) P))) ; todo interpret the first one first then the second
         ((eq f 'or) (or (fl-interp (car arg) P) (fl-interp (cadr arg) P))) ; todo interpret the first one first then the second
         ((eq f 'not) (not (fl-interp (car arg) P)))
         ((eq f 'if) (if (fl-interp (car arg) P) (fl-interp (cadr arg) P) (fl-interp (caddr arg) P))) ; evaluate condition first then evaluate result based on whether or not it's true

         (t E))))))

                ;;;.....

                ;;; if f is a user-defined function,
                ;;;    then evaluate the arguments
                ;;;         and apply f to the evaluated arguments
                ;;;             (applicative order reduction)
                ;;;.....

                ;;; otherwise f is undefined (not intended to be a function),
                ;;; the E is returned, as if it is quoted in lisp
