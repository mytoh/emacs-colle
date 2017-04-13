;;; colle --- colle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-deftype colle:coll () `(or null list vector))

(cl-defun colle:first (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car coll))
    (vector
     (aref coll 0))))

(cl-defun colle:second (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr coll)))
    (vector
     (aref coll 1))))

(cl-defun colle:third (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr coll))))
    (vector
     (aref coll 2))))

(cl-defun colle:fourth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr coll)))))
    (vector
     (aref coll 3))))

(cl-defun colle:fifth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr coll))))))
    (vector
     (aref coll 4))))

(cl-defun colle:sixth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr (cdr coll)))))))
    (vector
     (aref coll 5))))

(cl-defun colle:seventh (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr (cdr (cdr coll))))))))
    (vector
     (aref coll 6))))

(cl-defun colle:eighth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll)))))))))
    (vector
     (aref coll 7))))

(cl-defun colle:ninth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll))))))))))
    (vector
     (aref coll 8))))

(cl-defun colle:tenth (coll)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type coll colle:coll)
  (cl-etypecase coll
    (list
     (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr (cdr coll)))))))))))
    (vector
     (aref coll 9))))

(cl-defun colle:index (n coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase n
    (0 (colle:head coll))
    (k (colle:index (- k 1)
                 (colle:tail coll)))))

(cl-defun colle:index^ (n coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase `[,n ,coll]
    (`[,_ ,(or `() `[])]
     [:nothing])
    (`[0 ,(seq x &rest xs)]
     `[:just ,x])
    (`[,n ,(seq x &rest xs)]
     (colle:index^ (- n 1) xs))))

(cl-defun colle:head (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((seq x &rest _xs)
     x)))

(cl-defun colle:head^ (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq x &rest _xs)
     `[:just ,x])))

(cl-defun colle:rest (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((seq _ &rest xs)
     xs)))

(cl-defun colle:tail (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((seq _x &rest xs)
     xs)))

(cl-defun colle:tail^ (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((pred colle:empty-p)
     [:nothing])
    ((seq _x &rest xs)
     `[:just ,xs])))

(cl-defun colle:last (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    (`(,x) x)
    (`[,x] x)
    ((seq x y &rest ys)
     (colle:last (colle:conj y ys)))))

(cl-defun colle:last^ (coll)
  (declare (pure t)
           (side-effect-free t))
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
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    (`(,x) ())
    (`[,x] [])   
    ((seq x y &rest ys)
     (colle:conj x (colle:init
                 (colle:conj y ys))))))

(cl-defun colle:init^ (coll)
  (declare (pure t)
           (side-effect-free t))
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
  (declare (pure t)
           (side-effect-free t))
  (pcase `[,n ,coll]
    (`[0 ,xs]
     (colle:empty coll))
    (`[,sn ,(pred colle:empty-p)]
     (colle:empty coll))
    (`[,sn ,(seq x &rest xs)]
     (colle:conj x (colle:take (1- n)
                         xs)))))

(cl-defun colle:drop (n coll)
  (declare (pure t)
           (side-effect-free t))
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
  (colle:foldl
   (lambda (a b)
     (if (not (funcall f b))
         (colle:conj b a)
       a))
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
  (declare (pure t)
           (side-effect-free t))
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
  (declare (pure t)
           (side-effect-free t))
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
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    (`[] t)
    (`() t)))

(cl-defun colle:empty (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    ((pred vectorp) [])
    ((pred consp) ())))

(cl-defun colle:single-p (coll)
  (declare (pure t)
           (side-effect-free t))
  (pcase coll
    (`(,x) t)
    (`[,x] t)
    (_ nil)))

(cl-defun colle:conj (x coll)
  (declare (pure t)
           (side-effect-free t))
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

(cl-defun colle:concat (c1 c2)
  (declare (pure t)
           (side-effect-free t))
  ;; (cl-check-type c1 colle:coll)
  ;; (cl-check-type c2 colle:coll)
  (pcase `[,c1 ,c2]
    (`[() ,(pred consp)]
     c2)
    (`[,(pred consp) ()]
     c1)
    (`[[] ,(pred vectorp)]
     c2)
    (`[,(pred vectorp) []]
     c1)
    (`[,(pred consp) ,(pred consp)]
     (append c1 c2))
    (`[,(pred vectorp) ,(pred vectorp)]
     (vconcat c1 c2))
    (_ [:left "No match"])))

(cl-defun colle:flatten (coll)
  (declare (pure t)
           (side-effect-free t))
  (colle:foldr #'colle:concat (colle:empty coll)
            coll))

(cl-defun colle:unfoldl (args)
  )

(cl-defun colle:refold (args)
  )

(cl-defun colle:length (x)
  (declare (pure t)
           (side-effect-free t))
  (pcase x
    ((pred colle:empty-p) 0)
    ((app colle:rest xs) (+ 1 (colle:length xs)))))

(cl-defun colle:each (f coll)
  (colle:foldl
   (lambda (a e) (funcall f e) coll)
   ()
   coll))

(cl-defmacro colle:do ((x coll) &body body)
  (cl-letf ((c (gensym "colle-do-")))
    `(cl-letf ((,c ,coll))
       (while (not (colle:empty-p ,c))
         (cl-letf ((,x (colle:first ,c)))
           ,@body)
         (setq ,c (colle:rest ,c))))))

(defun colle:replace (smap coll)
  "Replace elements in coll according to smap.
[[https://clojuredocs.org/clojure.core/replace][replace - clojure.core | ClojureDocs - Community-Powered Clojure Documentatio...]]
"
  )

;; [[https://clojuredocs.org/clojure.walk/walk][walk - clojure.walk | ClojureDocs - Community-Powered Clojure Documentation a...]]
(defun colle:walk (inner outer form)
  "docstring"
  )

(provide 'colle)

;;; colle.el ends here
