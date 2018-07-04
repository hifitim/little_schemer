;; Number games
(defun addtup (tup)
  (cond
    ((null tup) 0)
    ((+ (car tup) (addtup (cdr tup))))))

(defun x (n m)
  (cond
    ((zerop m) 0)
    ((+ n (x n (sub1 m))))))

(defun sub1 (n)
  "Decrements n by 1."
  (- n 1))

(defun tup+ (tup1 tup2)
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
  (cond
    ((zerop m) nil)
    ((zerop n) t)
    ((my< (sub1 n) (sub1 m)))))
