(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

(defun split-to-numbers (text)
	(mapcar #'(lambda (a) (parse-integer (string a))) (coerce text 'list))
	)

(defun find-max-in-the-row (row)
	(let ((result (cons (first row) 0)))
		(loop for item in row
			for index from 0
			if (> item (car result))
			do (setf result (cons item index))
			)

		result
		)
	)

(defun rec-find-max-in-row (arr result-arr lower-limit upper-limit)
	"recursevely find largest number in arr and put in the result array moving lower-limit based on previous finded items and moving upper-limit based on which digit we searching"
	(if (> upper-limit (length arr))
		result-arr
		(let* ((current-max (find-max-in-the-row (subseq arr lower-limit upper-limit)))
			(new-result-arr (append result-arr (list (car current-max)))))
		(rec-find-max-in-row arr new-result-arr (+ 1 (+ lower-limit (cdr current-max))) (+ 1 upper-limit))
		)
		)
	)


(defun handle-max-joltage (battery-arr)
	(let* ((starting-upper-limit (- (length battery-arr) 11))
		(max-joltage-arr (rec-find-max-in-row battery-arr '() 0 starting-upper-limit))
		(final-number 0))
	(loop for item in (reverse max-joltage-arr)
		for index from 0
		do (setf final-number (+ final-number (* (expt 10 index) item)))
		)
	final-number
	)

	)

(defun handle-battery-bank (battery-arr)
	(let* ((first-number-pair (find-max-in-the-row (butlast battery-arr)))
		(second-number-pair (find-max-in-the-row (subseq battery-arr (+ (cdr first-number-pair) 1)))))
	(+ (* 10 (car first-number-pair)) (car second-number-pair))
	)
	)


(defun main ()
	(format t "~a~%" "--- Day 3: Lobby ---")
	(format t "    Part 1: ~a~%"
		(reduce
			#'(lambda (acc cur) (+ acc (handle-battery-bank (split-to-numbers cur))))
			(get-file "day03.txt")
			:initial-value 0)
		)
	(format t "    Part 2: ~a~%"
		(reduce 
			#'(lambda (acc cur) (+ acc (handle-max-joltage (split-to-numbers cur))))
			(get-file "day03.txt")
			:initial-value 0)
		)
	)

(main)
; (format t "~a~%" (split-to-numbers "1234"))
; (format t "~a~%" (remove-smallest '(9 8 7 6 5 4 3 2 1 1 1 1 1 1 1) 1))
; (format t "~a~%"(find-max-in-the-row '(9 8 7 6 5 4 3 2 1 1 1 1 1 1 1)))
