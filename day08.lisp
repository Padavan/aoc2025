(defun get-file (filename)
	(with-open-file (stream filename)
		(loop
		    for line = (read-line stream nil)
			while line
			collect line)
 		)
    )

(defun split-by-comma (string)
	(loop
	    for i = 0 then (1+ j)
		as j = (position #\, string :start i)
		collect (subseq string i j)
		while j
    	)
 	)


(defun main ()
	(format t "~a~%" "--- Day 8: Playground ---")
 	(let ((boxes-list (mapcar #'(lambda (cur) (split-by-comma cur)) (get-file "day08.txt"))))
 		(format t "boxes-list: ~a~%" boxes-list)
	    )
	)

(main)