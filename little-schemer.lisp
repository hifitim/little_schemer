(declaim (optimize #+sbcl (sb-c::merge-tail-calls 3) #+sbcl (sb-c::insert-debug-catch 0))) 

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

(defun sub1 (n)
  "Decrements n by 1."
  (- n 1))

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

(defun add1 (n)
  "Increments n by 1."
  (+ n 1))

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
    ((zerop (sub1 n)) (car lat))
    ((pick (sub1 n) (cdr lat)))))
