(declaim (optimize #+sbcl (sb-c::merge-tail-calls 3) #+sbcl (sb-c::insert-debug-catch 0))) 

;; Utility functions
(defun sub1 (n)
  "Decrements n by 1."
  (- n 1))

(defun add1 (n)
  "Increments n by 1."
  (+ n 1))

(defun one? (a)
  "Returns t is one."
  (eqan 1 a))

(defun atom? (a)
  (not (listp a)))

;; Number games
(defun addtup (tup)
  "Sums a tuple."
  (cond
    ((null tup) 0)
    ((+ (car tup) (addtup (cdr tup))))))

(defun x (n m)
  "Recursive multiplication of n by m."
  (cond
    ((zerop m) 0)
    ((+ n (x n (sub1 m))))))


(defun tup+ (tup1 tup2)
  "Adds two tuples together."
  (cond    
    ((null tup1) tup2)
    ((null tup2) tup1)
    ((cons (+ (car tup1) (car tup2))
     (tup+ (cdr tup1) (cdr tup2))))))
      
(defun my> (n m)
  (cond
    ((zerop n) nil)
    ((zerop m) t)
    ((my> (sub1 n) (sub1 m)))))

(defun my< (n m)
  "Is n less than m?"
  (cond
    ((zerop m) nil)
    ((zerop n) t)
    ((my< (sub1 n) (sub1 m)))))

(defun my= (n m)
  "Is n equal to m?"
  (cond
    ((zerop n) (zerop m))
    ((zerop m) nil)
    ((my= (sub1 n) (sub1 m)))))

(defun pow (n m)
  "n to m power."
  (cond
    ((zerop m) 1)
    ((x n (pow n (sub1 m))))))

(defun quot (n m)
  "Recursive division of n by m."
  (cond
    ((my< n m) 0)
    ((add1 (quot (- n m) m)))))

(defun mylen (lat)
  "Returns the number of elements in lat."
  (cond
    ((null lat) 0)
    ((add1 (mylen(cdr lat))))))

(defun pick (n lat)
  "Returns the nth element of lat."
  (cond
    ((one? n) (car lat))
    ((pick (sub1 n) (cdr lat)))))

(defun rempick (n lat)
  "Removes the nth element from lat."
  (cond
    ((one? n) (cdr lat))
    ((cons (car lat) (rempick (sub1 n) (cdr lat))))))

(defun nonums (lat)
  "Removes all the numbers from lat."
  (cond
    ((null lat) nil)
    ((numberp (car lat))
     (nonums (cdr lat)))
    ((cons (car lat) (nonums (cdr lat))))))

(defun allnums (lat)
  "Returns a tup of the numbers in lat."
  (cond
    ((null lat) nil)
    ((numberp (car lat))
     (cons (car lat) (allnums (cdr lat))))
    ((allnums (cdr lat)))))

(defun eqan (a1 a2)
  "Determines if a1 and a2 are equal."
  (cond
    ((and (numberp a1) (numberp a2))
     (my= a1 a2))
    ((or (numberp a1) (numberp a2))
     nil)
    ((eq a1 a2))))
     
(defun occur (a lat)
  "Returns the number of times that a occurs in lat."
  (cond
    ((null lat) 0)
    ((eqan (car lat) a)
     (add1 (occur a (cdr lat))))
    ((occur a (cdr lat)))))

(defun rember* (a lat)
  "Removes every occurance of a from lat."
  (cond
    ((null lat) nil)
    ((atom? (car lat))
     (cond
       ((eqan (car lat) a)
	(rember* a (cdr lat)))
       ((cons (car lat) (rember* a (cdr lat))))))
    ((cons (rember* a (car lat)) (rember* a (cdr lat))))))

(defun insertR* (new old lat)
  "Inserts new to the right each occurance of old in lat."
  (cond
    ((null lat) nil)
    ((atom? (car lat))     
     (cond
       ((eqan (car lat) old)	
	(cons old (cons new (insertR* new old (cdr lat)))))
       (t
	(cons (car lat) (insertR* new old (cdr lat))))))
    (t     
     (cons (insertR* new old (car lat)) (insertR* new old (cdr lat))))))
    
(defun insertL* (new old lat)
  "Inserts new to the right each occurance of old in lat."
  (cond
    ((null lat) nil)
    ((atom? (car lat))     
     (cond
       ((eqan (car lat) old)	
	(cons new (cons old (insertL* new old (cdr lat)))))
       (t
	(cons (car lat) (insertL* new old (cdr lat))))))
    (t     
     (cons (insertL* new old (car lat)) (insertL* new old (cdr lat))))))


(defun occur* (a lat)
  "Counts occurences of a in lat."
  (cond
    ((null lat) 0)
    ((atom? (car lat))
     (cond
       ((eqan (car lat) a)
	(add1 (occur* a (cdr lat))))
       ((occur* a (cdr lat)))))
    (t
     (+ (occur* a (car lat))
	(occur* a (cdr lat))))))

(defun subst* (new old lat)
  "Replaces old with new lat."
  (cond
    ((null lat) nil)
    ((atom? (car lat))
     (cond
       ((eqan (car lat) old)
	(cons new (subst* new old (cdr lat))))
       (t
	(cons (car lat) (subst* new old (cdr lat))))))
    (t
     (cons (subst* new old (car lat)) (subst* new old (cdr lat))))))

(defun member* (a lat)
  "Finds a in lat."
  (cond
    ((null lat) nil)
    ((atom? (car lat))
     (or (eqan (car lat) a)
	 (member* a (cdr lat))))
    (t
     (or (member* a (cdr lat))
	 (member* a (car lat))))))

(defun leftmost (lat)
  "Finds the leftmost atom in a list of lists."
  (cond
    ((null lat) nil)
    ((atom? (car lat))
     (car lat))
    (t
     (leftmost (car lat)))))
	    
(defun eqlist? (l1 l2)
  (cond
    ((and (null l1)
	  (null l2))
     t)
    ((or (null l1)
	 (null l2))
     nil)
    ((and (atom? (car l1))
	  (atom? (car l2)))
     (eqlist? (cdr l1)
	      (cdr l2)))
    ((or (atom? (car l1))
	 (atom? (car l2)))
     nil)
    ((and (eqlist? (car l1) (car l2))
	  (eqlist? (cdr l1) (cdr l2))))))

(defun equal? (l1 l2)
  (cond
    ((and (atom? l1)
	  (atom? l2))
     (eqan l1 l2))
    ((atom? l1)
     nil)
    ((atom? l2)
     nil)
    (t
     (eqlist? l1 l2))))
     
