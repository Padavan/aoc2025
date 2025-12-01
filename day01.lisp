(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

(defun parse-line (results line)
	(let ((direction (subseq line 0 1)))
		(let ((diff (parse-integer (subseq line 1))))
			(let ((next-counter 0))
				(if (equal direction "R")
					(setf next-counter (+ (car results) diff))
					(setf next-counter (- (car results) diff))
					)
				(if (= (rem next-counter 100) 0)
					(cons next-counter (+ 1 (cdr results)))
					(cons next-counter (cdr results))
					)
				)
			)
		)
	)

(defun part1 ()
	(format t "    Part 1: ~a~%"
		(cdr 
			(reduce #'(lambda (acc cur) (parse-line acc cur)) (get-file "day01.txt") :initial-value (cons 50 0))
			)
		)
	)

(defun main ()
 	(format t "~a~%" "--- Day 1: Secret Entrance ---")
	(part1)
	)

(main)