
;; QUESTION 1
;;this returns T if X is a member of Y. If not, it returns NIL
;;this also tests for lists being members of lists
;;both X and Y may be NIL, or lists containing NIL
;;
;;here's how it works:
;;returns nil if Y is nil
;;returns true if x = f(y)
;;else return xmember(x, r(y))
(defun xmember (X Y)
  (cond
   ((not Y) nil)
   ((equal X (car Y)) t)
   (t (xmember X (cdr Y)))))


;; QUESTION 2
(defun flatten (X)
  (cond
   ((not X) nil)
   ((atom (car X)) (cons (car X) (flatten (cdr X))))
   (t (append (flatten (car X)) (flatten (cdr X))))))

;; QUESTION 3
(defun remove-duplicate (X)
  (cond
   ((not X) nil)
   ((xmember (car X) (cdr X)) (remove-duplicates (cdr X)))
   (t (cons (car X) (remove-duplicates (cdr X))))))

;; QUESTION 4
;; it seems to work without checking if l1 is null todo, trace execution and find out why
(defun mix (L1 L2)
  (cond
   ((not L2) L1)
                                        ;  ((not L1) L2)
   (t (cons (car L1) (mix L2 (cdr L1))))))

;; QUESTION 5
(defun allsubsets (L)
  (if (not L) (list nil)
    (gen-subsets (car L) (allsubsets (cdr L)))))

;; wikipedia notation https://en.wikipedia.org/wiki/Power_set#Recursive_definition
;; union of a power set of T and a power set of T whose each element is expanded with e
;; For a non-empty set S (assignment calls it L)
;; let e be any element of the set (car L)
;; and T is the relative complement of e;
;; then the power set of S is a union of a power set of T and
;; a power set of T whose each element is expanded with e
(defun gen-subsets (e Tcomplement)
  (if
      (not tcomplement) nil
    (cons
     (cons e (car Tcomplement))
     (cons (car Tcomplement) (gen-subsets e (cdr Tcomplement))))))


