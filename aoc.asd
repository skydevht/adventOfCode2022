(defsystem "aoc21"
  :components ((:file "package")
               (:file "util" :depends-on ("package"))
               (:file "day3_1" :depends-on ("util"))))
