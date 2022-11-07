(in-package cl-user)

(defpackage aoc.day3.2
  (:use cl aoc))

(in-package aoc.day3.2)

(defparameter *input* (load-lines "day3_input.txt"))

(defun choose-next (cmp ones zeros pref)
  (let ((len1 (length ones))
        (len0 (length zeros)))
    (cond ((= len0 len1) (case pref
                           (:0 zeros)
                           (:1 ones)))
          (t (if (apply cmp (list len0 len1))
                 zeros
                 ones)))))

(defun find-number (number-list cmp pos pref)
  (let ((len (length number-list)))
    (cond ((= len 1) (first number-list))
          ((= len 0) nil)
          (t (loop for num-str in number-list
                   for digit = (char num-str pos)
                   when (char= digit #\0)
                     collect num-str into zeros
                   when (char= digit #\1)
                     collect num-str into ones
                   finally
                      (return (find-number (choose-next cmp
                                                        ones
                                                        zeros
                                                        pref)
                                           cmp
                                           (1+ pos)
                                           pref)))))))

(defun solution (lst)
  (* (parse-integer (find-number lst #'> 0 :1) :radix 2)
     (parse-integer (find-number lst #'< 0 :0) :radix 2)))
