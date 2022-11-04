(in-package cl-user)

(defpackage day2_2
  (:use cl))

(in-package day2_2)

(defparameter *commands*
  (uiop:read-file-lines (merge-pathnames "day2_input.txt" (uiop:getcwd))))

(defun calculate-position-and-depth (lst)
  (loop for command-str in lst
        for command = (uiop:split-string command-str)
        for action = (first command) and amount = (parse-integer (second command))
        when (equal action "down")
          sum amount into aim
        when (equal action "up")
          sum (- amount) into aim
        when (equal action "forward")
          sum amount into hz-position
          and sum (* amount aim) into depth
        finally (return (values hz-position depth))))

(defun solution (lst)
  (multiple-value-bind (pos dp) (calculate-position-and-depth lst)
    (* pos dp)))
