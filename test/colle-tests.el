;;; colle-tests --- colle-tests -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(when (featurep 'colle)
  (unload-feature 'colle 'force))
(require 'ert)
(require 'colle)

(ert-delete-all-tests)

(ert-deftest colle-tests-conj ()
  (should (cl-equalp
           (colle:conj 'a '(b c))
           '(a b c)))
  (should (cl-equalp
           (colle:conj 'a nil)
           '(a)))
  (should (cl-equalp
           (colle:conj 'a [b c])
           [a b c]))
  (should (cl-equalp
           (colle:conj 'a [])
           [a])))

(ert-deftest colle-tests-foldr ()
  (should (cl-equalp
           (colle:foldr (pcase-lambda (a x)
                            (* a x))
                        1 '(1 2 3))
           6))

  (should (cl-equalp
           (colle:foldr (pcase-lambda (a x)
                            (* a x))
                        1 [1 2 3])
           6))
  (cl-letf ((coll '(1 2 3 4)))
    (should (= (colle:foldr #'+ 0 coll) 10))
    (should (= (colle:foldr #'+ 5 coll) 15)))
  (cl-letf ((coll '()))
    (should (eq (colle:foldr #'+ 0 coll) 0))
    (should (eq (colle:foldr #'+ 7 coll) 7))))


(ert-deftest colle-tests-foldr1 ()
  (should (cl-equalp
           (colle:foldr1 (pcase-lambda (a x)
                             (* a x))
                         '(1 2 3))
           6))

  (should (cl-equalp
           (colle:foldr1 (pcase-lambda (a x)
                             (* a x))
                         [1 2 3])
           6)))

(ert-deftest colle-tests-foldl ()
  (should (cl-equalp
           (colle:foldl (pcase-lambda (a x)
                            (* a x))
                        1
                        '(1 2 3))
           6))

  (should (cl-equalp
           (colle:foldl (pcase-lambda (a x)
                            (* a x))
                        1
                        [1 2 3])
           6)))



(ert-deftest colle-tests-foldl1 ()
  (should (cl-equalp
           (colle:foldl1 (pcase-lambda (a x)
                             (* a x))
                         '(1 2 3))
           6))

  (should (cl-equalp
           (colle:foldl1 (pcase-lambda (a x)
                             (* a x))
                         [1 2 3])
           6)))



(ert-deftest colle-tests-map ()
  (should (cl-equalp
           (colle:map
            (lambda (x)
              (* x x))
            '(1 2 3))
           '(1 4 9)))
  (should (cl-equalp
           (colle:map
            (lambda (x) (* x x))
            [1 2 3])
           [1 4 9])))

(ert-deftest colle-tests-remove ()
  (should (cl-equalp
           (colle:remove
            (lambda (x) (zerop x))
            '(0 1 2 3 0 4 0 5))
           '(1 2 3 4 5)))
  (should (cl-equalp
           (colle:remove
            (lambda (x) (zerop x))
            [0 1 2 3 0 4 0 5])
           [1 2 3 4 5]))
  (cl-letf ((coll '(6 7 8 9 10)))
    (should (equal (colle:remove #'evenp coll) '(7 9)))
    (should (equal (colle:remove #'oddp coll) '(6 8 10)))
    (should (cl-equalp (colle:remove (lambda (elt) nil) coll) coll)))
  (cl-letf ((coll '()))
    (should (equal (colle:remove #'evenp coll) '()))))

(ert-deftest colle-tests-filter ()
  (should (cl-equalp
           (colle:filter
            (lambda (x) (zerop x))
            '(0 1 2 3 0 4 0 5))
           '(0 0 0)))
  (should (cl-equalp
           (colle:filter
            (lambda (x) (zerop x))
            [0 1 2 3 0 4 0 5])
           [0 0 0]))
  (cl-letf ((coll '(6 7 8 9 10)))
    (should (equal (colle:filter #'evenp coll) '(6 8 10)))
    (should (equal (colle:filter #'oddp coll) '(7 9)))
    (should (equal (colle:filter (lambda (elt) nil) coll) '())))
  (cl-letf ((coll '()))
    (should (equal (colle:filter #'evenp coll) '()))))

(ert-deftest colle-tests-find ()
  (should (cl-equalp
           (colle:find
            (lambda (x) (evenp x))
            '(1 2 3 0 4 0 5))
           2))
  (should (cl-equalp
           (colle:find
            (lambda (x) (zerop x))
            [1 2 3 0 4 0 5])
           0)))

;;; colle-tests.el ends here

