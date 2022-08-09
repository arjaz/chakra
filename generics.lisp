(in-package :chakra)

(defgeneric make-entity (world)
  (:documentation "Returns the unique id of the new entity."))
(defgeneric remove-entity (world entity-id)
  (:documentation "Removes the entity's components and sets the id up for reuse."))

(defgeneric add-component (world entity-id component))
(defgeneric remove-component (world entity-id component-type))

(defgeneric add-system (world system))
(defgeneric remove-system (world system-type))

(defgeneric update-world (world dt)
  (:documentation "Update all the systems in the world."))
