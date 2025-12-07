(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

; HASHMAP to put matrix coordinate as key and true as value. empty coordinates ignored
(defparameter *manifold-map* (make-hash-table))
(defparameter *tahchyon-map* (make-hash-table))

(defun split-step (state split-counter size)
  	; (format t "split-step: ~a~%" (loop for key being the hash-keys of state collect key))
    (let ((new-state (make-hash-table)) (counter split-counter))
	    (loop
			for tachyon being the hash-keys of state
			if (gethash tachyon *manifold-map*)
				do (setf (gethash (+ tachyon (complex 1 0)) new-state) t)
	   			and do (setf (gethash (+ tachyon (complex -1 0)) new-state) t)
	      		and do (setf counter (+ 1 counter))
	      		; and do (remhash tachyon state)
	  		else 
	  			do (setf (gethash tachyon new-state) t)
      		)
            
	    ; (format t "split-step - split-counter: ~a~%" counter)
	    (downward-step new-state counter size)
	    )
  )


(defun downward-step (state split-counter size)
  	(if (every #'(lambda (num) (> (imagpart num) size)) (loop for key being the hash-keys of state collect key))	
  		split-counter
	  	(let ((new-state (make-hash-table)))
	        (loop
				for tachyon being the hash-keys of state
    			as new-position = (+ tachyon (complex 0 1))
    			do (setf (gethash new-position new-state) t)
		  		)
         	(split-step new-state split-counter size)
	     	)
       )
    )

(defparameter *weight-map* (make-hash-table))

(defun timeline-step (point size)
  	"Recursive"
   	; (format t "timeline-step ~a~%" point)
   	(let ((weight (gethash point *weight-map*)))
	   	(if (not (null weight))
	   		weight
	     	(if (gethash point *manifold-map*)
	     		(setf (gethash point *weight-map*) (+
			        (timeline-step (+ point (complex -1 0)) size)
		            (timeline-step (+ point (complex 1 0)) size)
	            ))
	       		(setf (gethash point *weight-map*) (timeline-step (+ point (complex 0 1)) size))
	            )
	        )
	    )
  	)

(defun main ()
	(format t "~a~%" "--- Day 7: Laboratories ---")

	(let* ((matrix (get-file "day07.txt"))
       		(size (length matrix))
         	(start-tachyon 0)
         )

		(loop
			for row in matrix
			for row-index from 0
		    do (loop
					for col in (coerce row 'list)
					for col-index from 0
					if (equal col #\^)
						do (setf (gethash (complex col-index row-index) *manifold-map*) t)
      				if (equal col #\S)
						do (progn 
				           (setf (gethash (complex col-index row-index) *tahchyon-map*) t)
      					   (setf start-tachyon (complex col-index row-index)))
					)
			)
		
 			(format t "    Part 1: ~a~%" (downward-step *tahchyon-map* 0 size))
    		(loop
        		for i from 0 to size
          		do (setf (gethash (complex i size) *weight-map*) 1 ))
    		(format t "    Part 2: ~a~%" (timeline-step start-tachyon size))
		)
 
		
	)

(main)
; 234 too low
; 1276 too low

; (format t "~a~%" (loop for key being the hash-keys of *manifold-map* collect key))
; (format t "~a~%" (loop for key being the hash-keys of *tahchyon-map* collect key))
