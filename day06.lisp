(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

(defun split-by-space (text)
  	"ugly ass"
  	; (format t "split-by-space: ~a~%" (length text))
	(loop
		for i = 0 then (1+ j)
  		with text-length = (length text)
		as j = (position #\space text :start i)
		; do (format t "---~a~%" "")
		; do (format t "i ~a~%" i)
		; do (format t "j ~a~%" j)
        ; do (format t "shit ~a~%" (if (null j) (length text) j))
		while (and (< i text-length))
	    ; do (format t "subseq ~a~%" (subseq text i j))
		if (not (equal (char text i) #\space))
			collect (subseq text i j)
		while j
	   )
	 	
	)

(defun part1 (column-data)
	(let ((result 0)
		(operations (car (last column-data))))
		(loop
			for col from 0 to (- (length operations) 1)
			if (equal (nth col operations) "*")
			do (setf result
				(+ result (
					reduce #'(lambda (acc cur-row) (* acc (parse-integer (nth col cur-row))))
					(butlast column-data)
					:initial-value 1
					)))
			else
			do (setf result
				(+ result (
					reduce #'(lambda (acc cur-row) (+ acc (parse-integer (nth col cur-row))))
					(butlast column-data)
					:initial-value 0      
					)))
			)
		result
		)
	)

(defun collect-operation-data (text)
	(let ((inverse-char-list (coerce text 'list)) (j 0))
	  	(loop
	     	for char in inverse-char-list
	     	for i from 0
       		if (not (equal (nth i inverse-char-list) #\space))
       			collect (cons j i) and do (setf j (+ 2 i))
       		)
	    )
	)

(defun get-number (arr)
	"get number from array of spaces and digits"
 	; (format t "get-number ~a~%" arr)
 	(parse-integer
	    (coerce 
	    	(loop
		    	for char in arr
		    	if (not (equal char #\space))
					collect char
		     	)
      		'string
        	)
     	)
	)

(defun calculate (operations operation-data column-data)
  	"both args already reversed"
   	(let ((final-result 0))
	  	(loop
			for number-col in operations
			for number-col-idx from 0
			as operation-pair = (nth number-col-idx operation-data)
	     	do	(let ((result-between (if (equal number-col "*") 1 0)))
					(loop
			    		for digit-col-idx from 0 to (- (cdr operation-pair) (car operation-pair))
			      		if (equal number-col "*")
		         			do (let ((num (get-number (mapcar #'(lambda (row) (char (nth number-col-idx row) digit-col-idx)) column-data))))
		         				; (format t "number: ~a~%" num)
		         				(setf result-between (* result-between num))
	                   			)
		            	else
		         			do (let ((num (get-number (mapcar #'(lambda (row) (char (nth number-col-idx row) digit-col-idx)) column-data))))
		         				; (format t "number: ~a~%" num)
		         				(setf result-between (+ result-between num))
	                   			)
			      		)
					(setf final-result (+ final-result result-between))
			        )
			)
    		final-result
    	)
  
    )


(defun part2 (input)
 	(let* ((operations (reverse (car (last input))))
        (operation-data (collect-operation-data operations))
        (column-data (
            mapcar #'(lambda (str) 
	            (mapcar #'(lambda (cur) (subseq (reverse str) (car cur) (1+ (cdr cur)))) operation-data)                      
                                              ) (butlast input)))
        )
    	(calculate (split-by-space operations) operation-data column-data)
	    )
  
  )


(defun main ()
	(format t "~a~%" "--- Day 6: Trash Compactor ---")
	(let* ((input (get-file "day06.txt"))
        (column-data (mapcar #'(lambda (str) (split-by-space str)) input)))
   		(format t "    Part 1: ~a~%" (part1 column-data))
		(format t "    Part 2: ~a~%" (part2 input))
	    )
	)

(main)
; (format t "~a~%" (split-by-space "  1 2  3  4"))
; (format t "subseq test ~a~%" (subseq "1234567" 3 nil))
; (format t "===~a~%" (split-by-space "  1 2  3  45"))
; (format t "===~a~%" (split-by-space "  1 2  3  45 "))
; get-number
; (format t "===~a~%" (get-number '(#\space #\space #\1 #\space #\space #\2 #\space)))