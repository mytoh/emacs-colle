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
           6)))


;;; colle-tests.el ends here

