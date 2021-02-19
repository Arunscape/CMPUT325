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
       ((eq f 'and) (fl-and (E P)))
       ((eq f 'or) (fl-or (E P)))
       ((eq f 'not) (not (fl-interp (car arg) P)))
       ((eq f 'if) (fl-if (E P)))

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
(defun fl-and (E P)
  (if (not (fl-interp (car arg) P))
      nil
    (if (fl-interp (cadr arg) P)
        t
      nil)))

;; or is special.
;; exception to applicative order reduction
;; we need to evaluate the first argument first, then according to the result stop or continue evaluating the second argument
(defun fl-or (E P)
  (if (fl-interp (car arg) P)
      t
    (if (fl-interp (cadr arg) P)
        t
      nil)))

;; if is special
;; exception to applicative order reduction
;; evaluate the condition first, then according to the result either continue with the then part or the else part
(defun fl-if (E P)
  (if (fl-interp (car arg) P)
      (fl-interp (cadr arg) P)
    (fl-interp (caddr arg) P)))
