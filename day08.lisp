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

(defun get-distance (a b)
	(+
		(expt (- (first b)  (first a)) 2 )
		(expt (- (second b)  (second a)) 2 )
		(expt (- (third b)  (third a)) 2 )
		)
	)

(defun compare-distance (a b)
	(< (cdr a) (cdr b)))

(defun compare-length (a b)
	(< (length b) (length a)))

(defparameter *junction-list* (list))


(defun mergegroups (arr)
  	; (format t "mergegroups arr: ~a~%" arr)
  	(loop
  		for i from 0 to (- (length arr) 1)
  		do (loop
  			for j from (1+ i) to (- (length arr) 1)
        		; do (format t "intersection arr: ~a~%" (intersection (nth i arr) (nth j arr)))
        		if (> (length (intersection (nth i arr) (nth j arr))) 0)
        		do (setf (nth i arr) (union (nth i arr) (nth j arr)))
        		and do (delete-if-not #'null arr :start j :count 1)
        		)
  		)
  	arr
  	)

(defun group-distance (arr)
  	; (format t "group-distance arr: ~a~%" arr)
  	(let ((groups (list (list (realpart (car (first arr))) (imagpart (car (first arr))) )) ))
  		(loop
  			for i from 1 to 999
  			as left = (realpart (car (nth i arr)))
  			as right = (imagpart (car (nth i arr)))
  			as found = 0
  			; do (format t "left ~a~%" left)
  			; do (format t "right ~a~%" right)
  			; do (format t "cycle ~a~%" groups)
  			do (loop
  				for group in groups
  				if (and (member left group) (member right group))
  				do (setf found 1)
            	else if (member left group)
                	do (push right (cdr (last group)))
	            	and do (setf found 1)
           	    else if (member right group)
               	    do (push left (cdr (last group)))
	                and do (setf found 1)
                )
    		if (= 0 found)
    		do (push (list left right) (cdr (last groups)))
            
            do (setf groups (mergegroups groups))
            )

    	(setf groups (sort groups 'compare-length))
    	(apply #'* (list (length (first groups)) (length (second groups)) (length (third groups))))
    	)
  	)

(defun group-distance-2 (arr)
  	; (format t "group-distance arr: ~a~%" arr)
  	(let ((groups (list (list (realpart (car (first arr))) (imagpart (car (first arr))) )) ))
	  	(loop
	  		for i from 1
	  		as left = (realpart (car (nth i arr)))
	  		as right = (imagpart (car (nth i arr)))
	  		as found = 0
		  
	        do (loop
	        	for group in groups
	        	if (and (member left group) (member right group))
		        	do (setf found 1)
	        	else if (member left group)
	            	do (push right (cdr (last group)))
	            	and do (setf found 1)
	       	    else if (member right group)
	           	    do (push left (cdr (last group)))
	                and do (setf found 1)
	            )
			if (= 0 found)
			do (push (list left right) (cdr (last groups)))
	        
	        do (setf groups (mergegroups groups))
	        if (and (= 1 (length groups)) (= 1000 (length (first groups))))
	        return (values left right)
	        )
	  	)
  	)

(defun main ()
	(format t "~a~%" "--- Day 8: Playground ---")
	(let*
	    ((boxes-list
			(mapcar
				#'(lambda (cur) (mapcar #'parse-integer (split-by-comma cur)))
				(get-file "day08.txt")
				))
		(size (length boxes-list)))
		(let* ((arr
			(loop
				for i from 0 to (- size 1)
				append (loop
					for j from (1+ i) to (1- size)
                 		collect (cons (complex i j) (get-distance (nth i boxes-list) (nth j boxes-list)))
                 		)
				)
			)
        	(sorted-arr (sort arr 'compare-distance)))

    		(format t "    Part 1: ~a~%" (group-distance sorted-arr))
			(multiple-value-bind (c1 c2) (group-distance-2 sorted-arr)
				(format t "    Part 2: ~a~%" (* (first (nth c1 boxes-list)) (first (nth c2 boxes-list))))
			)
			)
		)
	)

; TODO learn proper algorithm
(main)

; (format t "~a~%" (get-distance '(0 0 0) '(2 2 2)))