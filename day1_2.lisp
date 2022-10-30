(in-package cl-user)

(defpackage day1-2
  (:use cl))

(in-package day1-2)
(defparameter *measurements*
  (mapcar (lambda (x) (parse-integer x :junk-allowed t))
	  (uiop:split-string
	   (uiop:read-file-string
	    (merge-pathnames "input.txt"
			     (uiop:getcwd)))
	   :separator '(#\newline))))

(defun window-sum (lst)
  (when (third lst)
    (apply #'+ (subseq lst 0 3))))

(defun solution (lst)
  (loop for l on lst
	for sum = (window-sum l)
	and pre = nil then sum
	while (third l)
	count (when pre (> sum pre))))

(solution *measurements*)
