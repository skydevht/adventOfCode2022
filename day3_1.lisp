(in-package cl-user)

(defpackage aoc.day3.1
  (:use cl aoc))

(in-package aoc.day3.1)

(defparameter *input* (load-lines "day3_input.txt"))

(defun gamma-and-epsilon (zero-cnt one-cnt)
  (flet ((reduce-fn (a b)
           (concatenate 'string
                        (if (numberp a) (write-to-string a)
                            a)
                        (write-to-string b))))
    (loop for zero-col in zero-cnt
          for one-col in one-cnt
          when (> zero-col one-col)
            collect 0 into gamma-lst
            and collect 1 into epsilon-lst
          else
            collect 1 into gamma-lst
            and collect 0 into epsilon-lst
          finally (return (values (reduce #'reduce-fn gamma-lst)
                                  (reduce #'reduce-fn epsilon-lst))))))

(defun solution (lst)
  (multiple-value-bind (gamma epsilon) (loop for bin-str in lst
                                             with zero-cnt = (make-sequence 'list (length (first lst)) :initial-element 0)
                                             with one-cnt = (copy-seq zero-cnt)
                                             do (dotimes (i (length bin-str))
                                                  (let ((bin-char (elt bin-str i)))
                                                    (if (char= bin-char #\1)
                                                        (incf (elt one-cnt i))
                                                        (incf (elt zero-cnt i)))))
                                             finally (return (gamma-and-epsilon zero-cnt one-cnt)))
    (* (parse-integer gamma :radix 2)
       (parse-integer epsilon :radix 2))))
