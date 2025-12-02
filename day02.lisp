(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

(defun split-by-comma (string)
	(loop for i = 0 then (1+ j)
		as j = (position #\, string :start i)
		collect (subseq string i j)
		while j))

(defun split-by-dash (string)
	(loop for i = 0 then (1+ j)
		as j = (position #\- string :start i)
		collect (subseq string i j)
		while j))

(defun count-digits (num digits)
	(if (< num 1)
		digits
		(count-digits (/ num 10) (+ 1 digits))
		)
	)

(defun check-number (num)
	"Checking if sequence repeat TWICE"
	(let ((digits (count-digits num 0)))
		(multiple-value-bind (quotient remainder) (floor num (expt 10 (/ digits 2)))
			(if (and (evenp digits)(= quotient remainder)) t nil)
			)
		)
	)

(defun get-pattern-lenght-list (digit-lenght &optional pattern-list)
	"Get available pattern-length based on number of digits"
	(reduce
		#'(lambda (acc cur)
			(multiple-value-bind (quotient remainder) (floor digit-lenght cur)
				(if (and (= 0 remainder) (> quotient 1))
					(append acc (list cur))
					acc
					)
				)
			)
		'(1 2 3 4 5 6 7 8 9)
		:initial-value '()
		)
	)


(defun rec-dividing (num arr len)
	"Get small numbers with lenght len from number num"
	(if (= num 0)
		arr
		(multiple-value-bind (quotient remainder) (floor num (expt 10 len))
			; (format t "quotient ~a~%" quotient)
			; (format t "remainder ~a~%" remainder)
			(rec-dividing quotient (append arr (list remainder)) len)
			)
		)
	)

(defun check-elements-equal (arr)
	"Return t if all array items are equal"
	; (format t "check-elements-equal ~a~%" arr)
	(reduce
		#'(lambda (acc cur)
			(and acc (= cur (first arr)))
			)
		arr
		:initial-value t
		)
	)

(defun check-number-n-patterns (num)
	"Checkinf if sequence repeat n-times"
	(let* ((digits (count-digits num 0))
		(pattern-length-arr (get-pattern-lenght-list digits)))
	(reduce
		#'(lambda (acc cur-length)
				; (format t "lambda ~a~%" acc)
				(or acc (check-elements-equal (rec-dividing num '() cur-length)))
				)
		pattern-length-arr
		:initial-value nil
		)
	)

	)


(defun step-through-range (current stop sum fn-check)
	(if (> current stop)
		sum
		(if (funcall fn-check current)
			(step-through-range (+ 1 current) stop (+ sum current) fn-check)
			(step-through-range (+ 1 current) stop sum fn-check)
			)
		)
	)

(defun parse-range (acc range fn-check)
	"iterate through ranges"
	(let ((start (parse-integer (first (split-by-dash range))))
		(end (parse-integer(second (split-by-dash range)))))
	(+ acc (step-through-range start end 0 fn-check))
	)
	)


(defun main ()
	(format t "~a~%" "--- Day 2: Gift Shop ---")
	(format t "    Part 1: ~a~%"
		(reduce #'(lambda (acc cur) (parse-range acc cur #'check-number)) (split-by-comma (first (get-file "day02.txt"))) :initial-value 0)
		)
	(format t "    Part 2: ~a~%"
		(reduce #'(lambda (acc cur) (parse-range acc cur #'check-number-n-patterns)) (split-by-comma (first (get-file "day02.txt"))) :initial-value 0)
		)
	)

(main)
