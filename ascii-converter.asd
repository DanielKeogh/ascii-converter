;;;; ascii-converter.asd

(asdf:defsystem #:ascii-converter
  :description "Turn images into ASCII art"
  :license "MIT"
  :depends-on
  :serial t
  :components ((:file "package")
               (:file "ascii-converter")))

