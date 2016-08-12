;;; colle-tests --- colle-tests -*- lexical-binding: t; coding: utf-8; -*-

;;; Commentary:

;;; Code:

(when (featurep 'colle)
  (unload-feature 'colle 'force))
(require 'ert)
(require 'colle)

(ert-delete-all-tests)

(ert-deftest colle-tests-nth ()
  (cl-letf* ((l (number-sequence 0 10))
             (v (cl-coerce l 'vector)))
    (should (cl-equalp
             (colle:first l) (colle:first v)))
    (should (cl-equalp
             (colle:second l) (colle:second v)))
    (should (cl-equalp
             (colle:third l) (colle:third v)))
    (should (cl-equalp
             (colle:fourth l) (colle:fourth v)))
    (should (cl-equalp
             (colle:fifth l) (colle:fifth v)))
    (should (cl-equalp
             (colle:sixth l) (colle:sixth v)))
    (should (cl-equalp
             (colle:seventh l) (colle:seventh v)))
    (should (cl-equalp
             (colle:eighth l) (colle:eighth v)))
    (should (cl-equalp
             (colle:ninth l) (colle:ninth v)))
    (should (cl-equalp
             (colle:tenth l) (colle:tenth v)))
    ))

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
  (cl-letf* ((coll '(1 2 3 4))
             (collv (seq-into coll 'vector)))
    (should (= (colle:foldr #'+ 0 coll) 10))
    (should (= (colle:foldr #'+ 5 coll) 15))
    (should (= (colle:foldr #'+ 0 collv) 10))
    (should (= (colle:foldr #'+ 5 collv) 15)))
  (cl-letf* ((coll '())
             (collv (seq-into coll 'vector)))
    (should (eq (colle:foldr #'+ 0 coll) 0))
    (should (eq (colle:foldr #'+ 7 coll) 7))
    (should (eq (colle:foldr #'+ 0 collv) 0))
    (should (eq (colle:foldr #'+ 7 collv) 7))))


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
  (cl-letf* ((coll '(6 7 8 9 10))
             (collv (seq-into coll 'vector)))
    (should (equal (colle:remove #'evenp coll) '(7 9)))
    (should (equal (colle:remove #'oddp coll) '(6 8 10)))
    (should (cl-equalp (colle:remove (lambda (elt) nil) coll) coll))
    (should (equal (colle:remove #'evenp collv) [7 9]))
    (should (equal (colle:remove #'oddp collv) [6 8 10]))
    (should (cl-equalp (colle:remove (lambda (elt) nil) collv) collv)))
  (cl-letf* ((coll '())
             (collv (seq-into coll 'vector)))
    (should (equal (colle:remove #'evenp coll) '()))
    (should (equal (colle:remove #'evenp collv) []))))

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
  (cl-letf* ((coll '(6 7 8 9 10))
             (collv (seq-into coll 'vector)))
    (should (equal (colle:filter #'evenp coll) '(6 8 10)))
    (should (equal (colle:filter #'oddp coll) '(7 9)))
    (should (equal (colle:filter (lambda (elt) nil) coll) '()))
    (should (equal (colle:filter #'evenp collv) [6 8 10]))
    (should (equal (colle:filter #'oddp collv) [7 9]))
    (should (equal (colle:filter (lambda (elt) nil) collv) [])))
  (cl-letf* ((coll '())
             (collv (seq-into coll 'vector)))
    (should (equal (colle:filter #'evenp coll) '()))
    (should (equal (colle:filter #'evenp collv) []))))

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

(ert-deftest colle-tests-length ()
  (should (eq 0 (colle:length ())))
  (should (eq 0 (colle:length [])))
  (should (eq 3 (colle:length '(1 2 3))))
  (should (eq 3 (colle:length '[1 2 3]))))

(ert-deftest colle-tests-index ()
  (cl-letf ((xl '(0 1 2 3))
            (xv [0 1 2 3]))
    (should (eq 0 (colle:index 0 xl)))
    (should (eq 1 (colle:index 1 xl)))
    (should (eq 0 (colle:index 0 xv)))
    (should (eq 1 (colle:index 1 xv)))))

(ert-deftest colle-tests-head ()
  (should (eq 0 (colle:head '(0 1 2 3))))
  (should (eq 0 (colle:head [0 1 2 3]))))

(ert-deftest colle-tests-tail ()
  (should (cl-equalp '(1 2 3) (colle:tail '(0 1 2 3))))
  (should (cl-equalp [1 2 3] (colle:tail [0 1 2 3]))))

(ert-deftest colle-tests-last^ ()
  (should (cl-equalp [:just 3] (colle:last^ '(0 1 2 3))))
  (should (cl-equalp [:just 3] (colle:last^ [0 1 2 3]))))

(ert-deftest colle-tests-concat ()
  (should (cl-equalp (colle:concat () '(:a :b :c)) '(:a :b :c)))
  (should (cl-equalp (colle:concat '(:a :b :c) ()) '(:a :b :c)))
  (should (cl-equalp (colle:concat [] [:a :b :c]) [:a :b :c]))
  (should (cl-equalp (colle:concat [:a :b :c] []) [:a :b :c]))
  (should (cl-equalp (colle:concat '(:a) '(:b :c)) '(:a :b :c)))
  (should (cl-equalp (colle:concat [:a] [:b :c]) [:a :b :c])))

(ert-deftest colle-tests-do ()
  (should (cl-letf ((coll nil))
            (colle:do (x [a b c d])
                   (if (eq x 'b)
                       (setq coll (append coll (list :t)))
                     (setq coll (append coll (list :f)))))
            (cl-equalp '(:f :t :f :f)coll))))

;;; colle-tests.el ends here

