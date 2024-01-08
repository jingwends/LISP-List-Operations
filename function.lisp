(defun my-union (lista listb)
  (cond ((null lista) listb)
        ((my-member (car lista) listb) (my-union (cdr lista) listb))
        (t (cons (car lista) (my-union (cdr lista) listb)))))

(defun my-member (elt lst) 
  (cond ((null lst) nil)
        ((equal elt (car lst)) t)
        (t (my-member elt (cdr lst)))))

(defun my-intersect (lista listb)
  (cond ((null lista) lista)
        ((my-member (car lista) listb) (cons (car lista) (my-intersect (cdr lista) listb)))
        (t (my-intersect (cdr lista) listb))))

(defun my-difference (lista listb)
  (cond ((null lista) lista)
        ((my-member (car lista) listb) (my-difference (cdr lista) listb))
        (t (cons (car lista) (my-difference (cdr lista) )))))
