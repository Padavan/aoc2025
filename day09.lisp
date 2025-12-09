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

(defun get-area (a b)
	(*
		 (1+ (abs (- (car b) (car a))))
		 (1+ (abs (- (cdr b) (cdr a))))
		)
	)

(defun compare-area (a b)
	(> (cdr a) (cdr b)))

(defun main ()
	(format t "~a~%" "--- Day 9: Movie Theater ---")
	(let*
	    ((points
			(mapcar
				#'(lambda (cur) (cons
                      (parse-integer (first (split-by-comma cur)))
                      (parse-integer (second (split-by-comma cur)))
                      ) 
                      )
				(get-file "day09.txt")
				))
		(size (length points)))
     
     	(let* ((point-to-point-area-list
	     	(loop
				for i from 0 to (- size 1)
				append (loop
					for j from (1+ i) to (1- size)
	             		collect (cons (complex i j) (get-area (nth i points) (nth j points)))
	             		)
					)
				)
            (sorted-point-to-point-area-list (sort point-to-point-area-list 'compare-area))
            )
        
        	(format t "    Part 1: ~a~%" (cdr (first sorted-point-to-point-area-list)))
        	
        	; (format t "~a~%" sorted-point-to-point-area-list)
        )
     
     	; (format t "~a~%" points)
		)
	)

(main)

(format t "test1 ~a~%" (get-area (cons 2 5) (cons 11 1)))