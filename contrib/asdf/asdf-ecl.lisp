;;; Copyright (c) 2005, Michael Goffioul (michael dot goffioul at swing dot be)
;;;
;;;   This program is free software; you can redistribute it and/or
;;;   modify it under the terms of the GNU Library General Public
;;;   License as published by the Free Software Foundation; either
;;;   version 2 of the License, or (at your option) any later version.
;;;
;;;   See file '../../Copyright' for full details.
;;;
;;; ECL SPECIFIC OPERATIONS FOR ASDF
;;;

(in-package :asdf)

;;;
;;; COMPILE-OP / LOAD-OP
;;;
;;; We change these operations to produce both system and FASL files.
;;;

(defmethod initialize-instance :after ((instance compile-op) &key &allow-other-keys)
  (setf (slot-value instance 'system-p) t))

(defmethod output-files ((o compile-op) (c cl-source-file))
  (list (compile-file-pathname (component-pathname c) :type :object)))

(defmethod perform ((o load-op) (c cl-source-file))
  (loop for i in (input-files o c)
	collect (let ((output (compile-file-pathname i)))
		  (c:build-fasl output :lisp-files (list i))
		  (load output))))

(defmethod output-files ((o load-op) (c cl-source-file))
  (loop for i in (input-files o c)
	collect (compile-file-pathname i :type :fasl)))

;;;
;;; BUNDLE-OP
;;;

(defclass bundle-op (operation)
  ((type :reader bundle-op-type)
   (monolithic :initform nil :reader bundle-op-monolithic)
   (name-suffix :initarg :name-suffix :initform nil)
   (build-args :initarg :args :initform nil :accessor bundle-op-build-args)))

(defclass fasl-op (bundle-op)
  ((type :initform :fasl)))

(defclass lib-op (bundle-op)
  ((type :initform :lib)))

(defclass dll-op (bundle-op)
  ((type :initform :dll)))

(defclass monolithic-bundle-op (bundle-op)
  ((monlithic :initform t)))

(defclass monolithic-fasl-op (fasl-op monolithic-bundle-op) ())

(defclass monolithic-lib-op (lib-op monolithic-bundle-op)
  ((type :initform :lib)))

(defclass monolithic-dll-op (dll-op monolithic-bundle-op)
  ((type :initform :dll)))

(defclass program-op (monolithic-bundle-op)
  ((type :initform :program)))

(defmethod initialize-instance :after ((instance bundle-op) &rest initargs
				       &key (name-suffix nil name-suffix-p)
				       &allow-other-keys)
  (unless name-suffix-p
    (setf (slot-value instance 'name-suffix)
	  (if (bundle-op-monolithic instance) "-mono" "")))
  (setf (bundle-op-build-args instance)
	(remove-keys '(type monolithic name-suffix)
		     (slot-value instance 'original-initargs))))

(defun gather-components (op-type system &key gather-all include-self)
  ;; This function creates a list of components, matched together with an
  ;; operation. This list may be restricted to sub-components of SYSTEM if
  ;; GATHER-ALL = NIL (default), and it may include the system itself.
  (let ((operation (make-instance op-type)))
    (append
     (loop for (op . component) in (traverse (make-instance 'load-op :force t) system)
	when (and (typep op 'load-op)
		  (or gather-all (eq (component-system component) system))
		  (or (eq op-type 'compile-op) (typep component 'system)))
	collect (progn
		  (when (eq component system) (setf include-self nil))
		  (cons operation component)))
     (and include-self (list (cons operation system))))))

;; BUNDLE-SUB-OPERATIONS
;;
;; Builds a list of pairs (operation . component) which contains all the
;; dependencies of this bundle. This list is used by TRAVERSE and also
;; by INPUT-FILES. The dependencies depend on the strategy, as explained
;; below.
;;
(defgeneric bundle-sub-operations (operation component))
;;
;; First we handle monolithic bundles. These are standalone systems
;; which contain everything, including other ASDF systems required
;; by the current one. A PROGRAM is always monolithic.
;;
;; MONOLITHIC SHARED LIBRARIES, PROGRAMS, FASL
;;
;; Gather the static libraries of all components.
;; They will be linked together into the resulting target.
;; Incude own system.
;;
(defmethod bundle-sub-operations ((o monolithic-bundle-op) c)
  (gather-components 'lib-op c :gather-all t :include-self t))
;;
;; MONOLITHICH STATIC LIBRARIES
;;
;; Gather the object files of all systems and subsystems.
;;
(defmethod bundle-sub-operations ((o monolithic-lib-op) c)
  (gather-components 'compile-op c :gather-all t))
;;
;; Now we analyze non-monolithic versions. They are not standalone
;; but we do not care about the dependencies, except in the case of
;; shared libraries, that must be linked against the shared libraries
;; they depend on.
;;
;; SHARED LIBRARIES
;;
;; Gather the dynamically linked libraries of all components.
;; They will be linked into this new shared library, together
;; with the object files of this module.
;;
(defmethod bundle-sub-operations ((o dll-op) c)
  (let* ((all-dlls (gather-components 'dll-op c :gather-all t))
	 (all-but-us (delete c all-dlls :key #'cdr)))
    (append all-but-as (gather-components 'compile-op c :gather-all nil))))
;;
;; STATIC LIBRARIES AND OTHERS
;;
;; We do not care about other modules, but just provide our
;; own compiled files.
;;
(defmethod bundle-sub-operations ((o bundle-op) c)
  (gather-components 'compile-op c :gather-all nil))

(defmethod component-depends-on ((o bundle-op) (c system))
  (loop for (op . dep) in (bundle-sub-operations o c)
     when (typep dep 'system)
     collect (list (class-name (class-of op))
		   (component-name dep))))

(defmethod input-files ((o bundle-op) (c system))
  (loop for (sub-op . sub-c) in (bundle-sub-operations o c)
     nconc (output-files sub-op sub-c)))

(defmethod output-files ((o bundle-op) (c system))
  (let ((name (concatenate 'base-string (component-name c)
			   (slot-value o 'name-suffix))))
    (list (merge-pathnames (compile-file-pathname name :type (bundle-op-type o))
			   (component-relative-pathname c)))))

(defmethod output-files ((o fasl-op) (c system))
  (loop for file in (call-next-method)
     collect (make-pathname :type "fasb" :defaults file)))

(defmethod perform ((o bundle-op) (c t))
  t)

(defmethod operation-done-p ((o bundle-op) (c source-file))
  t)

(defmethod perform ((o bundle-op) (c system))
  (let* ((object-files (input-files o c))
	 (output (first (output-files o c))))
    (apply #'c::builder (bundle-op-type o) output :lisp-files object-files
	   (bundle-op-build-args o))))

(defun select-operation (monolithic type)
  (ecase type
    ((:dll :shared-library)
     (if monolithic 'monolithic-dll-op 'dll-op))
    ((:lib :static-library)
     (if monolithic 'monolithic-lib-op 'lib-op))
    ((:fasl)
     (if monolithic 'monolithic-fasl-op 'fasl-op))
    ((:program)
     'program-op)))
    

(defun make-build (system &rest args &key (monolithic nil) (type :fasl) &allow-other-keys)
  (apply #'operate (select-operation monolithic type)
	 system
	 (remove-keys '(monolithic type) args)))

;;
;; LOAD-FASL-OP
;;
;; This is like ASDF's LOAD-OP, but using monolithic fasl files.
;;

(defclass load-fasl-op (operation) ())

(defmethod component-depends-on ((o load-fasl-op) (c system))
  (subst 'load-fasl-op 'load-op
	 (subst 'fasl-op 'compile-op
		(component-depends-on (make-instance 'load-op) c))))

(defmethod input-files ((o load-fasl-op) (c system))
  (output-files (make-instance 'fasl-op) c))

(defmethod perform ((o load-fasl-op) (c t))
  nil)

(defmethod perform ((o load-fasl-op) (c system))
  (load (first (input-files o c))))

(export '(make-build load-fasl-op))
(push '("fasb" . si::load-binary) si::*load-hooks*)