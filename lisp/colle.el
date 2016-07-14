;;; colle --- colle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-deftype colle:coll () `(or list vector))

(cl-defun colle:first (coll)
  (pcase coll
    ((pred listp)
     (car coll))
    ((pred vectorp)
     (aref coll 0))))

(cl-defun colle:second (coll)
  (pcase coll
    ((pred listp)
     (car (cdr coll)))
    ((pred vectorp)
     (aref coll 1))))

(cl-defun colle:third (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr coll))))
    ((pred vectorp)
     (aref coll 2))))

(cl-defun colle:fourth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr coll)))))
    ((pred vectorp)
     (aref coll 3))))

(cl-defun colle:fifth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr coll))))))
    ((pred vectorp)
     (aref coll 4))))

(cl-defun colle:sixth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr (cdr coll)))))))
    ((pred vectorp)
     (aref coll 5))))

(cl-defun colle:seventh (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr (cdr (cdr coll))))))))
    ((pred vectorp)
     (aref coll 6))))

(cl-defun colle:eighth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll)))))))))
    ((pred vectorp)
     (aref coll 7))))

(cl-defun colle:ninth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll))))))))))
    ((pred vectorp)
     (aref coll 8))))

(cl-defun colle:tenth (coll)
  (pcase coll
    ((pred listp)
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll)))))))))))
    ((pred vectorp)
     (aref coll 9))))

(cl-defun colle:index (n coll)
  (pcase n
    (0 (colle:head coll))
    (k (colle:index (- k 1)
                 (colle:tail coll)))))

(cl-defun colle:index^ (n coll)
  (pcase `[,n ,coll]
    (`[,_ ,(or `() `[])]
      [:nothing])
    (`[0 ,(seq x &rest xs)]
      `[:just ,x])
    (`[,n ,(seq x &rest xs)]
      (colle:index^ (- n 1) xs))))

(cl-defun colle:head (coll)
  (pcase coll
    ((seq x &rest _xs)
     x)))

(cl-defun colle:head^ (coll)
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
    ((seq _x &rest xs)
     xs)))

(cl-defun colle:tail^ (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq _x &rest xs)
     `[:just ,xs])))

(cl-defun colle:last (coll)
  (pcase coll
    (`(,x) x)
    (`[,x] x)
    ((seq x y &rest ys)
     (colle:last (colle:conj y ys)))))

(cl-defun colle:last^ (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq x &rest xs)
     (pcase xs
       ((pred colle:empty-p)
        `[:just ,x])
       ((seq y &rest ys)
        (colle:last^ xs))))))

(cl-defun colle:init (coll)
  (pcase coll
    (`(,x) ())
    (`[,x] [])   
    ((seq x y &rest ys)
     (colle:conj x (colle:init
                 (colle:conj y ys))))))

(cl-defun colle:init^ (coll)
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq x &rest xs)
     (pcase xs
       ((pred colle:empty-p)
        `[:just ,(colle:empty coll)])
       ((seq y &rest ys)
        (pcase (colle:init^ xs)
          (`[:nothing]
            [:nothing])
          (`[:just ,j]
            `[:just ,(colle:conj x j)])))))))

(cl-defun colle:take (n coll)
  (pcase `[,n ,coll]
    (`[0 ,xs]
      (colle:empty coll))
    (`[,sn ,(pred colle:empty-p)]
      (colle:empty coll))
    (`[,sn ,(seq x &rest xs)]
      (colle:conj x (colle:take (1- n)
                          xs)))))

(cl-defun colle:drop (n coll)
  (pcase `[,n ,coll]
    (`[0 ,xs]
      xs)
    (`[,sn ,(pred colle:empty-p)]
      (colle:empty coll))
    (`[,sn ,(seq x &rest xs)]
      (colle:drop (1- n)
               xs))))

(cl-defun colle:map (f coll)
  (pcase coll
    ((pred colle:empty-p coll)
     coll)
    ((pred listp)
     (colle:foldl
      (lambda (a b)
        (cons (funcall f b)
              a))
      (colle:empty coll)
      coll))
    ((pred vectorp)
     (colle:foldl
      (lambda (a b)
        (colle:conj
         (funcall f b) a))
      (colle:empty coll)
      coll))))


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
  (colle:foldl (lambda (a b)
              (if (funcall f b)
                  (colle:conj b a)
                a))
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
    ((or (pred listp)
         (pred vectorp))
     (cl-letf ((acc n))
       (mapc
        (lambda (e)
          (setq acc
                (funcall c acc e)))
        coll)
       (colle:reverse acc)))))

(cl-defun colle:reverse (coll)
  (pcase coll
    ((or (pred listp)
         (pred vectorp))
     (reverse coll))
    (_ coll)))

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

(cl-defun colle:each (f coll)
  (colle:foldl
   (lambda (a e) (funcall f e) coll)
   ()
   coll))


(provide 'colle)

;;; colle.el ends here
