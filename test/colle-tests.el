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
           '(a))))

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


;;; colle-tests.el ends here

