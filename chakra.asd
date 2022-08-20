;;;; chakra.asd

(asdf:defsystem #:chakra
  :description "Entity Component system"
  :license "MIT public license"
  :serial t
  :components ((:file "package")
               (:file "chakra"))
  :depends-on (#:iterate #:alexandria))

