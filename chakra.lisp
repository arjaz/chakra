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
    :documentation "A hash table from the event type to the systems - hash tables from the system type to the system object.")
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
  "Extract the dependencies of the QUERY which must be present."
  (remove-if-not #'symbolp (rest query)))

(defun query-negative-dependencies (query)
  "Extract the dependencies of the QUERY which must not be present."
  (mapcar #'second (remove-if #'symbolp (rest query))))

(defgeneric tick-system-fn (system))
(defmacro defsystem (name (&rest queries) &body body)
  "Create a system with a NAME.
Creates a class to store the QUERIES, and a function to run the BODY.
The first argument of the function must be the world, and the second must be the event."
  (let ((world-name (first queries))
        (event-name (second queries))
        (query-names (iter (for query in (rest (rest queries)))
                       (collect (first query)))))
    `(progn
       ;; TODO: query-names destructuring
       (defun ,name (,world-name ,event-name ,@query-names) ,@body)
       (defclass ,name (system) ()
         (:default-initargs
          :queries ',(rest (rest queries))))
       (defmethod tick-system-fn ((s ,name))
         (declare (ignore s))
         (symbol-function (quote ,name))))))

(defun in-hash-table-p (key hash-table)
  "T if the KEY is in the HASH-TABLE."
  (nth-value 1 (gethash key hash-table)))

(defun negative-dependencies-satisfied-p (world entity query)
  "Check if the negative component dependencies of the QUERY which must not be present in the ENTITY
are indeed not associated with that ENTITY in the given WORLD."
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-negative-dependencies query))
      (when (in-hash-table-p component-type components)
        (leave))
      (finally (return t)))))

(defun positive-dependencies (world entity query)
  "Check if the positive component dependencies of the QUERY which must be present in the ENTITY
are indeed associated with that ENTITY in the given WORLD."
  (let ((components (gethash entity (entity-components world))))
    (iter (for component-type in (query-positive-dependencies query))
      (alexandria:if-let (component (gethash component-type components))
        (collect component into collected-components)
        (leave))
      (finally (return (values collected-components t))))))

(defun entity-defined-p (world entity)
  "Check if the ENTITY is present in the WORLD."
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
  "Inserts a new empty entity into the WORLD and returns it."
  (flet ((initialize-components (entity)
           (setf (gethash entity (entity-components world)) (make-hash-table))))
    (iter (for entity from 0 below (length (entity-ids world)))
      ;; found an empty slot in the entities array - use it
      (unless (entity-defined-p world entity)
        (setf (aref (entity-ids world) entity) 1)
        (initialize-components entity)
        (leave entity))
      ;; if no empty slots found - extend the entities array
      (finally (vector-push-extend 1 (entity-ids world))
               (initialize-components entity)
               (return entity)))))

(defun remove-entity (world entity)
  "Removes the given ENTITY from the WORLD and clears out all associated components."
  (when (zerop (aref (entity-ids world) entity))
    (warn "Entity ~a not found, nothing removed.~%" entity)
    (return-from remove-entity nil))
  (remhash entity (entity-components world))
  (setf (aref (entity-ids world) entity) 0))

(defun remove-entities (world &rest entities)
  "Removes all the ENTITIES from the WORLD and clears out all associated components"
  (iter (for e in entities)
    (remove-entity world e)))

(defun cartesian-product (l)
  "Compute the n-cartesian product of a list of sets (each of them represented as list)."
  (if (null l)
      (list nil)
      ;; TODO: iter
      (loop for x in (car l)
            nconc (loop for y in (cartesian-product (cdr l))
                        collect (cons x y)))))

(defun tick-system (world event system)
  "Tick the SYSTEM of the WORLD with the passed event.
Runs the system against all components matching the query of the SYSTEM."
  (let ((queried-data (iter (for query in (system-queries system))
                        (collect (query-components world query)))))
    ;; TODO: skip if any two of them are the same
    (iter (for args in (cartesian-product queried-data))
      (apply (tick-system-fn system) world event args))))

(defun tick-event (world event)
  "Tick all systems subscribed to the EVENT in the WORLD."
  (iter (for (system-type system) in-hashtable (gethash event (systems world)))
    (tick-system world event system)))

(defun add-component (world entity component)
  "Adds a COMPONENT to the ENTITY in the WORLD."
  (let ((component-type (type-of component))
        (e-components (gethash entity (entity-components world))))
    (when (in-hash-table-p component-type e-components)
      (warn "Entity ~a already has the component of type ~a, replacing it.~%" entity component-type))
    (setf (gethash component-type e-components) component)))

(defun add-components (world entity &rest components)
  "Adds the COMPONENTS to the ENTITY in the WORLD."
  (iter (for c in components)
    (add-component world entity c)))

(defun remove-component (world entity component-type)
  "Removes the COMPONENT-TYPE from the ENTITY in the WORLD."
  (let ((e-components (gethash entity (entity-components world))))
    (remhash component-type e-components)))

(defun remove-components (world entity &rest components)
  "Removes all COMPENENTS from the ENTITY in the WORLD."
  (iter (for c in components)
    (remove-component world entity c)))

(defun add-system (world event system)
  "Creates a SYSTEM in the WORLD fired after the EVENT."
  (unless (in-hash-table-p event (systems world))
    (setf (gethash event (systems world)) (make-hash-table)))
  (setf (gethash (type-of system) (gethash event (systems world))) system))

(defun add-systems (world event &rest systems)
  "Add all SYSTEMS to the WORLD fired after the EVENT.."
  (iter (for s in systems)
    (add-system world event s)))

(defun remove-system (world event system)
  "Removes the SYSTEM bound to the EVENT from the WORLD."
  (when (in-hash-table-p event (systems world))
    (remhash (type-of system) (gethash event (systems world)))))

(defun remove-systems (world event &rest systems)
  "Removes all SYSTEMS bound to the EVENT from the world"
  (iter (for s in systems)
    (remove-system world event s)))

(defun get-system (world event system-type)
  "Gets the system object by its type and event from the WORLD."
  (gethash system-type (gethash event (systems world))))

(defun get-resource (world resource-type)
  "Gets the resource object by its type from the WORLD."
  (gethash resource-type (resources world)))

(defun add-resource (world resource)
  "Adds the RESOURCE to the WORLD."
  (setf (gethash (type-of resource) (resources world)) resource))

(defun add-resources (world &rest resources)
  "Adds all RESOURCES to the WORLD."
  (iter (for r in resources)
    (add-resource world r)))

(defun remove-resource (world resource)
  "Removes the RESOURCE from the WORLD."
  (remhash (type-of resource) (resources world)))

(defun remove-resources (world &rest resources)
  "Removes all REOUSRCES from the WORLD."
  (iter (for r in resources)
    (remove-resource world r)))

;; exports all symbols in package
;; seems reckless, but convenient
(let ((pack (find-package :chakra)))
  (do-all-symbols (sym pack)
    (when (eql (symbol-package sym) pack)
      (export sym))))




