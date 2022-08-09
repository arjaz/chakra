(in-package #:chakra)

(defclass system ()
  ((dependencies
    :initarg :dependencies
    :type list
    :accessor dependencies)
   (entities
    :initarg :entities
    :type hash-table
    :accessor system-entities)
   (tick
    :initarg :tick
    :type function
    :accessor system-tick))
  (:default-initargs
   :dependencies nil
   :entities (make-hash-table)))

;; TODO: document dependencies
(defmacro defsystem (name (&rest dependencies) &body body)
  `(progn
     (defun ,name (world id ,@(remove-if-not #'symbolp dependencies))
       (declare (ignorable world id))
       ,@body)
     (defclass ,name (system) ()
       (:default-initargs
        :dependencies ',dependencies
        :tick (symbol-function (quote ,name))))))

(defmethod clear-system ((system system))
  (setf (system-entities system) (make-hash-table)))

(defun system-add-entity (system entity-id components)
  "If the entity has the correct components, add it into the system."
  (if (system-satisfies-components-p components system)
      (setf (gethash entity-id (system-entities system)) 1)
      (warn "The entity ~a doesn't satisfy the dependencies ~a of the system ~a~%" entity-id (dependencies system) system)))

(defun system-add-entities (system &rest entities)
  "Adds (entity-id components) into SYSTEM."
  (iter (for (e ec) in entities)
    (system-add-entity system e ec)))

(defun system-remove-entity (system entity-id)
  (remhash entity-id (system-entities system)))

(defun system-remove-entities (system &rest entities)
  (iter (for e in entities)
    (system-remove-entity system e)))
