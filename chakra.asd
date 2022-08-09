;;;; chakra.asd

(asdf:defsystem #:chakra
  :description "Entity Component system"
  :license "MIT public license"
  :serial t
  :components ((:file "package")
               (:file "generics")
               (:file "utils")
               (:file "component")
               (:file "system")
               (:file "world")
               (:file "chakra"))
  :depends-on (#:iterate #:alexandria))

