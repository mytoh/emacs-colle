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
            (lambda (x) (eq x 0))
            '(0 1 2 3 0 4 0 5))
           '(1 2 3 4 5)))
  (should (cl-equalp
           (colle:remove
            (lambda (x) (eq x 0))
            [0 1 2 3 0 4 0 5])
           [1 2 3 4 5])))

;;; colle-tests.el ends here

