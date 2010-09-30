(in-package :cl-skip-list)

(defconstant +skip-node-deleted+ 4)
(defconstant +skip-node-timestamp+ 5)

(defmacro skip-node-deleted? (node)
  `(svref (the simple-vector ,node) +skip-node-deleted+))

(defmacro skip-node-timestamp (node)
  `(svref (the simple-vector ,node) +skip-node-timestamp+))

(defun make-skip-pq-node (key value size &key initial-element timestamp)
  (let ((node (make-array 6 :initial-element initial-element)))
    (setf (svref node +skip-node-key+)        key
	  (svref node +skip-node-value+)      value
	  (svref node +skip-node-forward+)    (make-array size :initial-element nil)
	  (svref node +skip-node-level+)      size
	  (svref node +skip-node-deleted+)    nil
	  (svref node +skip-node-timestamp+)  (or timestamp (gettimeofday)))
    node))

(defun make-skip-pq (&key (key-equal #'=) (value-equal #'equal) (comparison #'<)
		     (head-value most-negative-fixnum))
  (make-skip-list 
   :head (make-skip-pq-node head-value nil +max-level+ :timestamp most-positive-fixnum)
   :key-equal key-equal
   :value-equal value-equal
   :comparison comparison
   :duplicates-allowed? t
   :node-fn #'make-skip-pq-node))

(defmethod skip-pq-search ((sl skip-list) key id)
  (let ((start-node (skip-list-head sl)))
    (let ((x start-node) (y nil) 
	  (left-list  (make-array +max-level+ :initial-element nil))
	  (right-list (make-array +max-level+ :initial-element nil)))
      (loop for level from (1- (skip-node-level start-node)) downto 0 do
	   (loop
	      (setq y (mcas-read (skip-node-forward x) level))
	      (cond ((null y)
		     (return))
		    ((funcall (skip-list-comparison sl) key (skip-node-key y))
		     (return))
		    ((and (funcall (skip-list-key-equal sl) key (skip-node-key y))
			  (eql id (skip-node-deleted? y)))
		     (return))
		    ((funcall (skip-list-key-equal sl) key (skip-node-key y))
		     (return))
		    (t
		     (setq x y))))
	   (setf (svref left-list  level) x
		 (svref right-list level) y))
      (values left-list right-list))))

(defmethod skip-pq-add ((sl skip-list) key value)
  (skip-list-add sl key value))

(defmethod skip-pq-delete ((sl skip-list) node)
  "Delete a key or k/v pair from the skip pq.  If no value is specified and duplicates are
allowed, it will delete the first key it finds."
  (multiple-value-bind (left-list right-list) 
      (skip-pq-search sl (skip-node-key node) (skip-node-deleted? node))
    (let ((match-node (svref right-list 0)))
      (cond ((null match-node)
	     nil)
	    ((not (and (funcall (skip-list-key-equal sl) 
				(skip-node-key match-node) (skip-node-key node))
		       (eql sb-thread:*current-thread* (skip-node-deleted? match-node))))
	     nil)
	    (t
	     (let ((old-value (mcas-read match-node +skip-node-value+)))
	       (mcas-successful?
		(with-mcas (:equality #'equal 
				      :success-action 
				      #'(lambda () 
					  (sb-ext:atomic-decf (skip-list-length sl))))
		  (loop for i from 0 to (1- (skip-node-level match-node)) do
		       (let ((next-node (mcas-read (skip-node-forward match-node) i)))
			 (if (and next-node
				  (funcall (skip-list-comparison sl) 
					   (skip-node-key next-node)
					   (skip-node-key match-node))) 
			     nil
			     (progn
			       (mcas-set (skip-node-forward (svref left-list i)) i
					 match-node
					 next-node)
			       (mcas-set (skip-node-forward match-node) i
					 next-node
					 (svref left-list i))))))
		  (mcas-set match-node +skip-node-value+ old-value nil)))))))))

(defmethod delete-min ((sl skip-list))
  (let ((start-time (gettimeofday)))
    (let ((x (skip-list-head sl)) (y nil))
      (loop
	 (setq y (mcas-read (skip-node-forward x) 0))
	 (cond ((null y) 
		(return-from delete-min nil))
	       ((and (>= start-time (skip-node-timestamp y))
		     (null (cas (svref y +skip-node-deleted+) nil sb-thread:*current-thread*)))
		(let ((k (skip-node-key y)) (v (skip-node-value y)))
		  (skip-pq-delete sl y)
		  (return-from delete-min (list k v))))
	       (t 
		(setq x y))))
      nil)))

#|
(defun pq-test ()
  (let ((sl (make-skip-pq)))
    (dotimes (j 4)
      (dotimes (i 5)
	(skip-list-add sl i (code-char i))))
    (format t "~A~%" (skip-list-to-list sl))
;    (map-skip-list #'(lambda (k v) (format t "~A: ~A~%" k v)) sl)
;    (let ((c (skip-list-range-cursor sl 33 126)))
;      (do ((i (sl-cursor-next c) (sl-cursor-next c)))
;	  ((null i))
;	(format t "~A~%" i)))
    (format t "~A~%" sl)
    (dotimes (i 5)
      (dotimes (j 4)
	(format t "~A~%" (delete-min sl))
	(format t "~A~%" (skip-list-to-list sl))
	(format t "~A~%" sl)))))
  
;    (skip-list-to-list sl)))
|#
