;QUESTION 1
;this returns T if X is a member of Y. If not, it returns NIL
;this also tests for lists being members of lists
;both X and Y may be NIL, or lists containing NIL
;
;here's how it works: 
;returns nil if Y is nil
;returns true if x = f(y)
;else return xmember(x, r(y))  
(defun xmember (X Y)
  (cond
    ((not Y) nil)
    ((equal X (car Y)) t)
    (t (xmember X (cdr Y)))))


(defun flatten (X)
  (cond
    ((not X) nil)
    ((atom (car X)) (cons (car X) (flatten (cdr X))))
    (t (append (flatten (car X)) (flatten (cdr X))))))
  

;(defun flatten-helper (X flat)
;  (cond
;    ((not X) flat)
;    ((atom (car X)) (flatten-helper (cdr X)))
;    ((not flat) (flatten-helper (cdr X) (car X)))
;    (t (flatten-helper (cdr X) (cons flat (car X))))))

(defun flatten-helper (X flat)
  (cond
    ((not X) flat)                                          ; if x is nil, we are done flattening
    ((atom (car X)) (if flat
                        (flatten-helper (cdr X) (append flat (car X)))
                        (flatten-helper (cdr X) (list (car X)))))            ; if x is an atom, append it to the list
    (t (append (flatten (car X)) (flatten (cdr X))))))
