;;; Copyright 2021 Google LLC
;;;
;;; Use of this source code is governed by an MIT-style
;;; license that can be found in the LICENSE file or at
;;; https://opensource.org/licenses/MIT.

(in-package #:grpc.test)

;;; A suite to contain all other test suites so there's an easy entry point to run all tests.
(clunit:defsuite root-suite ())

(defun run-all ()
  "Run all tests."
  (clunit:run-suite 'root-suit :signal-condition-on-fail t))

(defun %generate-temporary-binding-name (binding)
  "Generates the temporary binding name to store the original function."
  (cons (car binding) (gensym)))

(defun %let-binding-expr (name)
  "Returns the LET binding that binds the original function to the temporary function name."
  (destructuring-bind (function-name . temporary-name) name
    `(,temporary-name (symbol-function ',function-name))))

(defun %set-mock-binding (binding)
  "Saves the mock function body to the symbol-function."
  (destructuring-bind (function-name &rest function-expr) binding
    `(setf (symbol-function ',function-name) (lambda ,@function-expr))))

(defun %reset-original-function-binding (name)
  "Rebind the original function to the symbol-function."
  (destructuring-bind (function-name . temporary-name) name
    `(setf (symbol-function ',function-name) ,temporary-name)))

(defmacro with-mocked-functions (bindings &body body)
  "Mocks the functions defined in BINDINGS before executing BODY."
  (let ((names (mapcar #'%generate-temporary-binding-name bindings)))
    `(let (,@(mapcar #'%let-binding-expr names))
       ,@(mapcar #'%set-mock-binding bindings)
       (prog1 (progn ,@body)
         ,@(mapcar #'%reset-original-function-binding names)))))
