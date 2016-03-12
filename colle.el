;;; colle --- colle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defun colle:first (coll)
  (seq-elt coll 0))

(cl-defun colle:head (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq x &rest _xs)
     `[:just ,x])))

(cl-defun colle:rest (coll)
  (pcase coll
    ((seq _ &rest xs)
     xs)))

(cl-defun colle:tail (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq _x &rest xs)
     `[:just ,xs])))

(cl-defun colle:last (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq x &rest xs)
     (pcase xs
       ((pred colle:empty-p)
        `[:just ,x])
       ((seq y &rest ys)
        (colle:last xs))))))


(cl-defun colle:map (f coll)
  (colle:foldr (lambda (a b)
             (colle:conj (funcall f a)
                     b))
           (colle:empty coll) coll))


(cl-defun colle:remove (f coll)
  (colle:foldr (lambda (a b)
             (if (not (funcall f a))
                 (colle:conj a b)
               b))
           (colle:empty coll)  coll))

;; deleteBy : (a -> a -> Bool) -> a -> List a -> List a
(cl-defun colle:delete-by (f x coll)
  (pcase coll
    ((pred colle:empty-p)
     (colle:empty coll))
    ((seq y &rest ys)
     (if (funcall f x y)
         ys
       (colle:conj y (colle:delete-by f x ys))))))

;; delete : (Eq a) => a -> List a -> List a
(cl-defun colle:delete (a coll)
  (colle:delete-by #'cl-equalp a coll))

(cl-defun colle:filter (f coll)
  (colle:foldr (lambda (a b)
             (if (funcall f a)
                 (colle:conj a b)
               b))
           (colle:empty coll)  coll))

(cl-defun colle:find (f coll)
  (pcase coll
    ((pred colle:empty-p) nil)
    ((and (let x (colle:first coll))
        (guard (funcall f x)))
     x)
    (_ (colle:find f (colle:rest coll)))))

(cl-defun colle:foldr (c n coll)
  (pcase coll
    ((pred colle:empty-p) n)
    ((seq x &rest xs)
     (funcall c x
        (colle:foldr c n xs)))))

(cl-defun colle:foldr1 (f coll)
  (pcase coll
    ((pred colle:single-p)
     (colle:first coll))
    ((seq x &rest xs)
     (funcall f x
        (colle:foldr1 f xs)))))

(cl-defun colle:foldl (c n coll)
  (pcase coll
    ((pred colle:empty-p) n)
    ((seq x &rest xs)
     (colle:foldl c
              (funcall c n x)
              xs))))

(cl-defun colle:foldl1 (f coll)
  (pcase coll
    ((seq x &rest xs)
     (colle:foldl f x xs))))

(cl-defun colle:empty-p (coll)
  (pcase coll
    (`[] t)
    (`() t)))

(cl-defun colle:empty (coll)
  (pcase coll
    ((pred vectorp) [])
    ((pred consp) ())))

(cl-defun colle:single-p (coll)
  (pcase coll
    (`(,x) t)
    (`[,x] t)
    (_ nil)))

(cl-defun colle:conj (x coll)
  (pcase coll
    ((app type-of `vector)
     (seq-concatenate 'vector
                      `[,x] coll))
    ((or `()
        (app type-of `cons))
     (cons x coll))))

(cl-defun colle:drop-while (f coll)
  (pcase (colle:first coll)
    ((pred (lambda (x) (funcall f x)))
     (colle:drop-while f
                   (colle:rest coll)))
    (_ coll)))

(cl-defun colle:unfoldl (args)
  )

(cl-defun colle:refold (args)
  )

(cl-defun colle:length (x)
  (pcase x
    ((pred colle:empty-p) 0)
    ((app colle:rest xs) (+ 1 (colle:length xs)))))

(cl-defun colle:index (idx x)
  (pcase idx
    (0 (colle:first x))
    (_ (colle:index (1- idx) (colle:rest x)))))

(cl-defun colle:head (x)
  (pcase x
    ((pred colle:empty-p) nil)
    ((app colle:first x) x)))

(cl-defun colle:tail (x)
  (pcase x
    ((pred colle:empty-p) nil)
    ((app colle:rest xs) xs)))

(provide 'colle)

;;; colle.el ends here
