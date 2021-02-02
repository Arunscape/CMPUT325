;; Name: Arun Woosaree
;; Student Number: 1514457
;; Course: CMPUT 325
;; Section: B1
;; Assignment 1

;; QUESTION 1
;;
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
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 1.1 (xmember '1 '(1))T)
;; (test-case 1.2 (xmember '1 '( (1) 2 3))NIL)
;; (test-case 1.3 (xmember '(1) '((1) 2 3)) T)
;; (test-case 1.4 (xmember nil nil) NIL)
;; (test-case 1.5 (xmember nil '(nil)) T)
;; (test-case 1.6 (xmember nil '((nil))) NIL)
;; (test-case 1.7 (xmember '(nil) '(1 2 3 (nil))) T)
;; (test-case 1.8 (xmember '(nil) '(nil)) NIL)
(defun xmember (X Y)
  (cond
   ((not Y) nil)
   ((equal X (car Y)) t)
   (t (xmember X (cdr Y)))))


;; QUESTION 2
;;
;; What does this function do?
;; it flattens lists with sublists of any depth such that the result is just a list of atoms
;; assumes that NIL will not appear in the list given
;;
;; How does the function work?
;; it goes through each element of X and checks if it is an atom
;; if it is not an atom, we flatten it, and then also flatten the rest of the list
;; if it is an atom, we just need to flatten the rest of the list
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 2.1 (flatten '(a (b c) d)) '(a b c d))
;; (test-case 2.2 (flatten '((((a))))) '(a))
;; (test-case 2.3 (flatten '(a (b c) (d ((e)) f))) '(a b c d e f))
(defun flatten (X)
  (cond
   ((null X) nil)
   ((atom (car X)) (cons (car X) (flatten (cdr X))))
   (t (append (flatten (car X)) (flatten (cdr X))))))

;; QUESTION 3
;;
;; What does this function do?
;; it removes duplicate items from a list, preserving the order of elements
;; assumes that NIL will not appear in the list given
;;
;; How does the function work?
;; it goes through each element of X and checks if it is a member of (cdr X)
;; if so, call remove-duplicate on the rest of the list, ignoring the current item
;; else, include the current item, but remove duplicates on (cdr X)
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 3.1 (remove-duplicate '(a b c a d b)) '(c a d b)) ;abcd is also a valid order
(defun remove-duplicate (X)
  (cond
   ((not X) nil)
   ((xmember (car X) (cdr X)) (remove-duplicates (cdr X)))
   (t (cons (car X) (remove-duplicates (cdr X))))))

;; QUESTION 4
;;
;; What does this function do?
;; it mixes the elements of L1 and L2, interweaving them
;; if one list is shorter than the other, append remaining elements to the end
;;
;; How does the function work?
;; if either of the lists are empty, return the other list
;; else take one item from the first list, then recurse but also switch
;; the order of the arguments, so that in the next recursive call, it gets mixed
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 4.1 (mix '(a b c) '(d e f)) '(a d b e c f))
;; (test-case 4.2 (mix '(1 2 3) '(a)) '(1 a 2 3))
;; (test-case 4.3 (mix '((a) (b c)) '(d e f g h)) '((a) d (b c) e f g h))
;; (test-case 4.4 (mix '(1 2 3) nil) '(1 2 3))
;; (test-case 4.5 (mix '(1 2 3) '(nil)) '(1 nil 2 3))
;; (test-case 4.6 (mix '(1 3 5) '(2 4 6 7 8 9)) '(1 2 3 4 5 6 7 8 9))
;; (test-case 4.7 (mix '(2 4 6 7 8 9) '(1 3 5)) '(2 1 4 3 6 5 7 8 9))
(defun mix (L1 L2)
  (cond
   ((not L2) L1)
   ((not L1) L2)
   (t (cons (car L1) (mix L2 (cdr L1))))))

;; QUESTION 5
;;
;; What does this function do?
;; it returns all the subsets of a given set
;;
;; How does the function work?
;; it finds all the subsets for the first element in the list,
;; and then recursively finds the subsets for all the other elements in the list
;; see gen-subsets
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 5.1 (allsubsets nil) '(nil))
;; (test-case 5.2 (allsubsets '(a)) '((a) nil))
;; (test-case 5.3 (allsubsets '(a b)) '((a b) (b) (a)  nil))
;; (test-case 5.4 (allsubsets '(a b c)) '((a b c) (b c) (a c) (c) (a b) (b) (a) () ))
(defun allsubsets (L)
  (if (null L) (list nil)
    (gen-subsets (car L) (allsubsets (cdr L)))))

;; What does this function do?
;; it generates subsets for a given list
;;
;; How does the function work?
;; I got the algorithm from wikipedia:  https://en.wikipedia.org/wiki/Power_set#Recursive_definition
;; For a non-empty set S (assignment calls it L)
;; let e be any element of the set (car L), which we get from allsubsets above
;; and T is the relative complement of e
;; then the power set of S is a union of a power set of T and
;; a power set of T whose each element is expanded with e
(defun gen-subsets (e Tcomplement)
  (if (not Tcomplement) nil
    (cons
     (cons e (car Tcomplement))
     (cons (car Tcomplement) (gen-subsets e (cdr Tcomplement))))))


;; QUESTION 6
;; Part 1
;;
;; What does this function do?
;; given a list L, which is a list of pairs representing linkage
;; and x, the page to start at
;; it returns a list of all webpages that are reachable if you start at x, and follow links to other pages
;;
;; How does the function work?
;; it finds all the websites that can be reached, then removes duplicate items
;; and also x, if it is present (see remove-x)
;; see find-links
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 6.11 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada))) '(SHOPIFY AIRCANADA))
;; (test-case 6.12 (reached 'google '( (google shopify) (shopify amazon) (amazon google) ) ) '(amazon shopify))
;; (test-case 6.13 (reached 'google '( (google shopify) (shopify amazon) (amazon indigo)  )) '(INDIGO amazon shopify))
;; (test-case 6.14 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) ))  '(SHOPIFY delta AIRCANADA))
;; (test-case 6.14 (reached 'google '( (google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google) ))  '(SHOPIFY delta AIRCANADA))
;; ;;backwards reached
;; (test-case 6.15 (reached 'google '((shopify aircanada) (google shopify))) '(aircanada shopify))
;; ;; cycle
;; (test-case 6.16 (reached 'a '((a b) (b a) (a c))) '(b c))
(defun reached (x L)
  (remove-duplicate
   (remove-x x
             (find-links x L L nil))))

;; What does this function do?
;; given x, and a list L, it will remove occurrences of x in L
;;
;; How does the function work?
;; it checks each item in the list, and if x is equal to that item, we ignore it
;; and recurse on (cdr l)
;; otherwise, we include the item, and remove duplicates from the rest of the list
(defun remove-x (x L)
  (cond
   ((null L) nil)
   ((eq x (car L)) (remove-x x (cdr L)))
   (t (cons (car L) (remove-x x (cdr L))))))

;; What does this function do?
;; it finds all of the places that can be reached
;; (see the description for reached)
;; the difference here, is that there can be duplicates,
;; and the starting point x, can also be included in the result
;; in reached, these are removed from the result returned by this function
;;
;; How does the function work?
;; x is the starting point to search
;; L is the given list of links
;; original is the original list of links. It starts out being the same as L,
;;  but because we need to check each pair, we use L to go over each element in the list
;;  and when we need the original list, we pass that in.
;; visited is a list to keep track of websites we have visited
;; if we've already visited the site, then we return nil
;; if we've exhausted all elements, we return x in a list,
;; if there are still items in L, we recursively build that list
;; so the end result contains all of the websites visited
;;
;; (caar L) is the first website in the first element of the list given
;;  e.g. (caar '((a b) (c d))) should give a
;; (cadar L) is the second website in the first element of the list given
;;  e.g. (caar '((a b) (c d))) should give b
;;
;; when recursively building this list, we check if x is equal to (caar L)
;; which means that the website x links to somewhere else
;; we get that using (cadar L) and then recursively find out which
;; websites (cadar L) links to since those can also be reached from x
;;
;; we also search the rest of the list, since x could link to other places later
;; in the list
(defun find-links (x L original visited)
  (cond
   ((null L) (list x)) ; done iteration, ran out of items
   ((xmember x visited) nil)
   ((eq x (caar L)) (append (find-links (cadar L) original original (cons x visited))
                            (find-links x (cdr L) original visited)))
   (t (find-links x (cdr L) original visited))))

;; QUESTION 6
;; Part 2
;;
;; What does this function do?
;; given S, which is a list of websites
;; L, which is a list of pairs representing linkage between websites,
;; this returns a permutation of S, such that the websites
;; with the most links to it is first in the list
;; if 2 pages are equally important, the order does not matter
;; a site referring to itself does not count
;; and a multiple references to a site from another same site only counts once
;;
;; How does the function work?
;; first, we count the references to each site using get-rank (see below)
;; then, we sort it using mySort, which sorts based on the number of references counted
;; finally, we get rid of the counts, and return just the final sorted list of
;; elements that were originally in S
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 6.221 (rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) '(AIRCANADA SHOPIFY GOOGLE AMAZON))
;; (test-case 6.222 (rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) '(GOOGLE SHOPIFY AMAZON))
;; (test-case 6.223 (rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) '(SHOPIFY AMAZON INDIGO GOOGLE))
;; (test-case 6.224 (rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) '(AIRCANADA SHOPIFY DELTA GOOGLE AMAZON))
(defun rank (S L)
  (map-get-cars
   (mySort
    (get-rank S L))))

;; I can't use mapcar for this assignment, so this just
;; goes over each element, and maps each pair to just the first element
;; example: ((1 2) (3 4) (5 6)) becomes (1 3 5)
(defun map-get-cars (L)
  (if (null L) nil
    (cons (caar L) (map-get-cars (cdr L)))))

;; Custom sort function, this function was given to us on eclass
(defun mySort (L)
  (sort L 'greaterThan))

;; Custom sort function, this function was given to us on eclass
;; it sorts such that the item with the greatest second item in the pair comes first
;; I use cdr instead of cadr here because I end up with a list of dotted pairs, so
;; cdr gets me an atom, which is the second item in the pair
(defun greaterThan (L1 L2)
  (> (cdr L1) (cdr L2)))

;; What does this function do?
;; given S, which is a list of websites
;; L, which is a list of pairs representing linkage between websites,
;; this returns a list of pairs, where the first item in the pair comes from list S,
;; and the second item is how many unique links there are to that website
;;
;; How does the function work?
;; for each element in S, we count how many references there are to that element (see count-references)
;; we then take that element and map it to a pair with the first element being itself, and the second
;; element being the result of count-references
;;
;; Test cases (the test-case function is in a comment at the end of this file)
;; (test-case 6.21 (test-rank '(google shopify aircanada amazon) '((google shopify) (google aircanada) (amazon aircanada))) '((AIRCANADA 2) (SHOPIFY 1) (GOOGLE 0) (AMAZON 0)))
;; (test-case 6.22 (test-rank '(google shopify amazon) '((google shopify) (shopify amazon) (amazon google))) '((GOOGLE 1) (SHOPIFY 1) (AMAZON 1)))
;; (test-case 6.23 (test-rank '(google shopify amazon indigo) '((google shopify) (shopify amazon) (amazon indigo))) '((SHOPIFY 1) (AMAZON 1) (INDIGO 1) (GOOGLE 0)))
;; (test-case 6.24 (test-rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (google google))) '((AIRCANADA 2) (SHOPIFY 1) (DELTA 1) (GOOGLE 0) (AMAZON 0)))
;; ;; multiple references don't count and references to self don't count
;; (test-case 6.25 (test-rank '(google shopify aircanada amazon delta) '((google shopify) (google aircanada) (amazon aircanada) (aircanada delta) (aircanada aircanada) (aircanada delta) (google google))) '((AIRCANADA 2) (SHOPIFY 1) (DELTA 1) (GOOGLE 0) (AMAZON 0)))
;;
;; this just makes sure that my test cases match the result that i'm expecting it to be equal to
;; since get-rank returns a list of dotted pairs instead of a list of pairs
;; (defun test-rank (S L)
;;   (mapcar (lambda (x) (list (car x) (cdr x)))
;;           (mySort (get-rank S L))))
(defun get-rank (S L)
  (if (null S) nil
    (cons
     (cons (car S) (count-references (car S) L nil))
     (get-rank (cdr S) L))))

;; What does this function do?
;; given x, and a list of pairs L,
;; this counts how many references there are to x in L
;;
;; How does the function work?
;; sources keeps track of the sources which have linked to x
;; that way, we can make sure that we don't add duplicates
;;
;; when we're done iterating through the list L, we return the length of sources
;; else, if x is equal to the second element in the current pair (cadar L), we add
;; the first element (caar L) to the list of sources
;; but first, we need to make sure that it is not already in the list of sources
;; and also that it is not a link to itself
;;
;; if those conditions are not satisfied, then we search through the rest of L,
;; until it is exhausted
(defun count-references (x L sources)
  (cond
   ((null L) (len sources))
   ((and
     (equal x (cadar L))
     (not (xmember (caar L) sources))
     (not (eq x (caar L))))
    (count-references x (cdr L) (cons (caar L) sources)))
   (t (count-references x (cdr L) sources))))

;; should be self explanatory, but in case it's not:
;; this gives the length of a list by going over each element in it and adding one
;; when the list is empty, we return 0 and add the rest of the ones together
(defun len (L)
  (if (null L) 0
    (+ 1 (len (cdr L)))))


;; TEST CASE HELPER FUNCTION
;; this is commented out because format is not in the list of allowed functions
;; but I used it to run my test cases defined above
;; (defun test-case (ID Test Result)
;;   (if (equal Test Result)
;;       (format t "Test ~S OK~%" ID)
;;       (format t "FAIL: Test ~S expected ~S got ~S~%" ID Result Test)))
