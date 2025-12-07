(defun get-file (filename)
	(with-open-file (stream filename)
		(loop
		    for line = (read-line stream nil)
			while line
			collect line)
 		)
    )


(defun main ()
	(format t "~a~%" "--- Day 8: ---")		
	)

(main)