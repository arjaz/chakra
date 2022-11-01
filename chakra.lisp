;;;; chakra.lisp

(in-package #:chakra)

(defun build-var (var)
  "Creates a list for slot VAR, with initform nil, :accessor VAR, and :initarg :VAR."
  (list var
        :initform nil
        :accessor (intern (string var))
        :initarg (intern (string var) :keyword)))

(defun build-varlist (varlist)
  "Takes a list and creates a list of slot lists."
  (iter (for var in varlist)
    (collect (build-var var))))

(defmacro defcomponent (name (&rest slots))
  "Makes a basic class. The accessors are declared for the slots, with the same name."
  `(defclass ,name ()
     (,@(build-varlist slots))))

(defmacro defresource (name (&rest slots))
  "Makes a basic class. The accessors are declared for the slots, with the same name."
  `(defclass ,name ()
     (,@(build-varlist slots))))

;; TODO: rename fields
(defclass world ()
  ((entity-components
    :initform (make-hash-table)
    :type hash-table
    :accessor entity-components
    :documentation "A hash table from the entity id to its components - hash tables from the component types to their data.")
   (entity-ids
    :initform (make-array 256 :fill-pointer 0
                              :adjustable t
                              :element-type 'bit
                              :initial-element 0)
    :type array
    :accessor entity-ids
    :documentation "An array of all entity ids.")
   (systems
    :initform (make-hash-table)
    :type hash-table
    :accessor systems
    :documentation "A hash table from the system type to the system object.")
   (resources
    :initform (make-hash-table)
    :type hash-table
    :accessor resources
    :documentation "A hash table from resource type to its value."))
  (:documentation "Handles the entities and the systems. Hash tables are used to enforce uniqueness."))

(defclass system ()
  ((queries
    :initarg :queries
    :type list
    :accessor system-queries
    :documentation "The list of queries of the given system.
A query is a list where the first element is the name,
and the rest are either component names or lists of two elements of form (not component-name).")))

(defun query-positive-dependencies (query)
  "Extract the positive dependencies of the QUERY."
  (remove-if-not #'symbolp (rest query)))

(defun query-negative-dependencies (query)
  "Extract the negative dependencies of the QUERY."
  (mapcar #'second (remove-if #'symbolp (rest query))))

(defgeneric tick-system-fn (system))
(defmacro defsystem (name (&rest queries) &body body)
  (let ((world-name (first queries))
        (query-names (iter (for query in (rest queries))
                       (collect (first query)))))
    `(progn
       (defun ,name (,world-name ,@query-names) ,@body)
       (defclass ,name (system) ()
         (:default-initargs
          :queries ',(rest queries)))
       (defmethod tick-system-fn ((s ,name))
         (declare (ignore s))
         (symbol-function (quote ,name))))))

(defun in-hash-table-p (key hash-table)
  ;; gethash returns 2 values, the value,
  ;; and a boolean saying whether the key is in the hash table or not
  ;; (it's zero based indexing, so nth-value 1)
  (nth-value 1 (gethash key hash-table)))

(defun negative-dependencies-satisfied-p (world entity query)
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-negative-dependencies query))
      (when (in-hash-table-p component-type components)
        (leave))
      (finally (return t)))))

(defun positive-dependencies (world entity query)
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-positive-dependencies query))
      (alexandria:if-let (component (gethash component-type components))
        (collect component into collected-components)
        (leave))
      (finally (return (values collected-components t))))))

(defun entity-defined-p (world entity)
  (not (zerop (aref (entity-ids world) entity))))

(defun query-components (world query)
  "Extract the list with the data of matching components based on the QUERY.
The second value indicates whether the query was succesful."
  (iter (for entity from 0 below (length (entity-ids world)))
    (when (and (entity-defined-p world entity)
               (negative-dependencies-satisfied-p world entity query))
      (alexandria:when-let (components (positive-dependencies world entity query))
        (collect (append (list entity) components) into collected-components)))
    (finally (return (values collected-components t)))))

(defun query-entity-components (world entity query)
  "Extract the list with the data of matching components based on the QUERY.
The second value indicates whether the query was successful."
  (when (negative-dependencies-satisfied-p world entity (append (list 'ignore) query))
    (positive-dependencies world entity (append (list 'ignore) query))))

(defun make-entity (world)
  (iter (for entity from 0 below (length (entity-ids world)))
    ;; found an empty slot in the entities array - use it
    (unless (entity-defined-p world entity)
      (setf (aref (entity-ids world) entity) 1
            ;; initialize the entity's components
            (gethash entity (entity-components world)) (make-hash-table))
      (leave entity))
    ;; if no empty slots found - extend the entities array
    (finally (vector-push-extend 1 (entity-ids world))
             ;; initialize the entity's components
             (setf (gethash entity (entity-components world)) (make-hash-table))
             (return entity))))

(defun remove-entity (world entity)
  (when (zerop (aref (entity-ids world) entity))
    (warn "Entity ~a not found, nothing removed.~%" entity)
    (return-from remove-entity nil))
  (remhash entity (entity-components world))
  (setf (aref (entity-ids world) entity) 0))

(defun remove-entities (world entity &rest entities)
  (remove-entity world entity)
  (iter (for e in entities)
    (remove-entity world e)))

(defun cartesian-product (l)
  "Compute the n-cartesian product of a list of sets (each of them represented as list)."
  (if (null l)
      (list nil)
      (loop for x in (car l)
            nconc (loop for y in (cartesian-product (cdr l))
                        collect (cons x y)))))

(defun update-system (world system)
  (let ((queried-data (iter (for query in (system-queries system))
                        (collect (query-components world query)))))
    ;; TODO: skip if any two of them are the same
    (iter (for args in (cartesian-product queried-data))
      (apply (tick-system-fn system) world args))))

(defun add-component (world entity component)
  (let ((component-type (type-of component))
        (e-components (gethash entity (entity-components world))))
    (when (in-hash-table-p component-type e-components)
      (warn "Entity ~a already has the component of type ~a, replacing it.~%" entity component-type))
    (setf (gethash component-type e-components) component)))

(defun add-components (world entity component &rest components)
  (add-component world entity component)
  (iter (for c in components)
    (add-component world entity c)))

(defun remove-component (world entity component-type)
  (let ((e-components (gethash entity (entity-components world))))
    (remhash component-type e-components)))

(defun remove-components (world entity component &rest components)
  (remove-component world entity component)
  (iter (for c in components)
    (remove-component world entity c)))

(defun add-system (world system)
  (setf (gethash (type-of system) (systems world)) system))

(defun add-systems (world system &rest systems)
  (add-system world system)
  (iter (for s in systems)
    (add-system world s)))

(defun remove-system (world system)
  (remhash (type-of system) (systems world)))

(defun remove-systems (world system &rest systems)
  (remove-system world system)
  (iter (for s in systems)
    (remove-system world s)))

(defun get-system (world system-type)
  (gethash system-type (systems world)))

(defun get-resource (world resource-type)
  (gethash resource-type (resources world)))

(defun add-resource (world resource)
  (setf (gethash (type-of resource) (resources world)) resource))

(defun add-resources (world resource &rest resources)
  (add-resource world resource)
  (iter (for r in resources)
    (add-resource world r)))

(defun remove-resource (world resource)
  (remhash (type-of resource) (resources world)))

(defun remove-resources (world resource &rest resources)
  (remove-resource world resource)
  (iter (for r in resources)
    (remove-resource world r)))

;; exports all symbols in package
;; seems reckless, but convenient
(let ((pack (find-package :chakra)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))




