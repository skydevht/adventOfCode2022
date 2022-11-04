(in-package aoc)

(defun load-lines (path)
  (uiop:read-file-lines (merge-pathnames path (uiop:getcwd))))