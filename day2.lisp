(in-package cl-user)

(defpackage day2
  (:use cl))

(in-package day2)

(defparameter *commands*
  (uiop:read-file-lines (merge-pathnames "day2_input.txt" (uiop:getcwd))))

(defun solution (lst)
   (let ((data (reduce (lambda (acc el)
            (let* ((command (uiop:split-string el))
               (action (first command))
               (amount (parse-integer (second command))))
              (cond ((equal action "forward") (incf (gethash :hz acc 0) amount))
                ((equal action "up") (decf (gethash :dp acc 0) amount))
                ((equal action "down") (incf (gethash :dp acc 0) amount))))
            acc)
          lst
          :initial-value (make-hash-table))))
     (* (gethash :hz data 1) (gethash :dp data 1))))
