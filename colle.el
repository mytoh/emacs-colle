;;; colle --- colle -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'seq)

(cl-defun colle:first (coll)
  (seq-elt coll 0))

(cl-defun colle:rest (coll)
  (pcase coll
    ((seq _ &rest xs)
     xs)))

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

(cl-defun colle:foldr (c n coll)
  (pcase coll
    ((pred colle:empty-p) n)
    ((seq x &rest xs)
     (funcall c x
              (colle:foldr c n xs)))))

(cl-defun colle:foldl (c n coll)
  (pcase coll
    ((pred colle:empty-p) n)
    ((seq x &rest xs)
     (colle:foldl c
                  (funcall c n x)
                  xs))))

(cl-defun colle:empty-p (coll)
  (pcase coll
    (`[] t)
    (`() t)))

(cl-defun colle:empty (coll)
  (pcase coll
    ((pred vectorp) [])
    ((pred consp) ())))

(cl-defun colle:conj (x coll)
  (pcase coll
    ((app type-of `vector)
     (seq-concatenate 'vector
                      `[,x] coll))
    ((or `()
         (app type-of `cons))
     (cons x coll))))

(provide 'colle)

;;; colle.el ends here
