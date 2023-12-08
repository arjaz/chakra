;;;; package.lisp

(defpackage #:chakra
  (:use #:cl #:iter)
  (:export
   #:world
   #:component
   #:query
   #:resource
   #:system
   #:defsystem
   #:make-entity
   #:remove-entity
   #:remove-entities
   #:tick-event
   #:add-component
   #:add-components
   #:remove-component
   #:remove-components
   #:add-system
   #:add-systems
   #:remove-system
   #:remove-systems
   #:add-resource
   #:add-resources
   #:remove-resource
   #:remove-resources))
