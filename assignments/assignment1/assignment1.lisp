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
