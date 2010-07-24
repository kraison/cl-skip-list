(in-package #:cl-skip-list)

(defmacro while (test &rest body)
  `(loop until (not ,test) do
	,@body))

(defgeneric less-than (x y)
  (:documentation "Generic less-than operator.  Allows comparison of apples and oranges.")
  (:method ((x symbol) (y symbol))    (string< (symbol-name x) (symbol-name y)))
  (:method ((x symbol) (y string))    (string< (symbol-name x) y))
  (:method ((x symbol) (y number))    (string< (symbol-name x) (write-to-string y)))
  (:method ((x number) (y number))    (< x y))
  (:method ((x number) (y symbol))    (string< (write-to-string x) (symbol-name y)))
  (:method ((x number) (y string))    (string< (write-to-string x) y))
  (:method ((x string) (y string))    (string< x y))
  (:method ((x string) (y symbol))    (string< x (symbol-name y)))
  (:method ((x string) (y number))    (string< x (write-to-string y))))

(defun get-prop (plist prop)
  (cond ((null plist) nil)
	((eql (car plist) prop)
	 (cadr plist))
	(t (get-prop (cddr plist) prop))))
