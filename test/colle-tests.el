;;; colle-tests --- colle-tests -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(require 'ert)
(require 'colle)

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

  )

(ert-deftest colle-tests-map ()
  (should (cl-equalp
           (colle:map
            (lambda (x) (* x x))
            [1 2 3])
           [1 4 9])))


;;; colle-tests.el ends here

