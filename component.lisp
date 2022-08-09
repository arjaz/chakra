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
