(in-package :chakra)

(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :type hash-table)
   (entity-ids
    :initform (make-array 256 :fill-pointer 0 :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :type array
    :accessor world-entities
    :documentation "Implemented this way so that I can loop through
    all entities.")
   (systems
    :initform (make-hash-table)
    :type hash-table
    :accessor systems))
  (:documentation "Handles the entities and the systems."))

(defun make-world ()
  "Returns a new world instance."
  (make-instance 'world))

(defmethod make-entity ((world world))
  (with-slots (entity-ids entity-components) world
    (iter (for i from 0 below (length entity-ids))
      ;; empty e found - set filled
      (when (= (aref entity-ids i) 0)
        (setf (aref entity-ids i) 1
              ;; initialize the entity's components
              (components world i) (make-hash-table))
        (leave i))
      ;; if nothing found - extend array
      (finally (vector-push-extend 1 entity-ids)
               ;; initialize the entity's components
               (setf (components world i) (make-hash-table))
               (return i)))))

(defmethod remove-entity ((world world) entity-id)
  (with-slots (entity-components entity-ids systems) world

    ;; check that entity exists
    (when (zerop (aref entity-ids entity-id))
      (warn "Entity ~a not found. Nothing removed." entity-id)
      (return-from remove-entity nil))

    ;; remove entity from entity-components
    (remhash entity-id entity-components)

    ;; free entity id
    (setf (aref entity-ids entity-id) 0)

    ;; remove entity from all systems
    (iter (for (st s) in-hashtable systems)
      (remhash entity-id (system-entities s)))))

(defun remove-entities (world &rest entities)
  (iter (for e in entities)
    (remove-entity world e)))

(defun is-entity-p (world entity-id)
  (when (and world entity-id)
    (= (aref (world-entities world) entity-id) 1)))

(defmethod add-component ((world world) entity-id component)
  (with-slots (entity-components entity-ids systems) world
    (let ((type (type-of component))
          (ec (components world entity-id)))
      ;; check if component of same type is already there
      (when (in-hash-table-p type ec)
        (warn "Entity ~a already has component of type ~a, replacing." entity-id type))
      (setf (gethash type ec) component)

      ;; add entity to any systems
      ;; system-type, system
      (iter (for (st s) in-hashtable systems)
        (system-add-entity s entity-id ec)))))

(defun add-components (world entity-id &rest components)
  (iter (for c in components)
    (add-component world entity-id c)))

(defmethod remove-component ((world world) entity-id component)
  (let ((type (type-of component))
        (components (components world entity-id)))
    ;; remove the component from components hash-table
    (remhash type components)

    ;;remove entity from systems for entity does not have the
    ;;relevant components
    (with-slots (systems) world
      ;; system-type, system
      (iter (for (st s) in-hashtable systems)
        (unless (system-satisfies-components-p components s)
          (system-remove-entity s entity-id))))))

(defun remove-components (world entity-id &rest components)
  (iter (for c in components)
    (remove-component world entity-id c)))

(defun update-system (world system)
  (iter (for (entity-id n) in-hashtable (system-entities system))
    ;; TODO: process the (not component) part as well
    (let ((args
            (concatenate 'list
                         (list world entity-id)
                         (iter (for c in (direct-system-dependencies system))
                           (collect (gethash c (components world entity-id)))))))
      (apply (system-tick system) args))))

(defmethod update-world ((world world) dt)
  (with-slots (systems) world
    (iter (for (system-type system) in-hashtable systems)
      (update-system world system))))

(defmethod add-system ((world world) system)
  (with-slots (systems entity-ids) world
    (setf (gethash (type-of system) systems) system)

    ;; add entities to the system
    (iter (for e from 0 below (length entity-ids))
      (when (= (aref entity-ids e) 1)
        (system-add-entity system e (components world e))))))

(defun add-systems (world &rest systems)
  (iter (for s in systems)
    (add-system world s)))

(defmethod remove-system ((world world) system)
  (with-slots (systems) world
    (remhash (type-of system) systems)))

(defun remove-systems (world &rest systems)
  (iter (for s in systems)
    (remove-system world s)))

(defmethod get-system ((world world) (system-type symbol))
  (gethash system-type (systems world)))

(defmethod clear-entities ((world world))
  (with-slots (entity-ids) world
    (iter (for id from 0 below (length entity-ids))
      (when (= (aref entity-ids id) 1)
        (remove-entity world id)))))

(defmethod clear-world ((world world))
  "Clear entities, components, and systems."
  (with-slots (entity-components entity-ids systems) world
    (setf entity-components (make-hash-table)
          systems (make-hash-table))
    (iter (for i in-vector entity-ids)
      (setf i 0))))
