(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

(defun split-by-dash (string)
	(loop for i = 0 then (1+ j)
		as j = (position #\- string :start i)
		collect (subseq string i j)
		while j))

(defun find-index-empty-string (arr)
	(let ((result -1))
		(loop
			for item in arr
			for index from 0
			do (if (equal item "") (setf result index))
			)
		result
		)
	)

(defun is-in-range (range target)
	(if (and (>= target (car range)) (<= target (cdr range)))
		t
		)
	)

(defun is-in-some-range (ranges target)
	(reduce
		#'(lambda (acc cur-range) (or acc (is-in-range cur-range target)))
		ranges
		:initial-value nil
		)
	)


(defun part1 (fresh-ranges ingredients)
	"check how many ingredients fall in ranges"
	(reduce
		#'(lambda (acc cur-ingr) (+ acc (if (is-in-some-range fresh-ranges cur-ingr) 1 0)))
		ingredients
		:initial-value 0
		)
	)

(defun is-overlap (range-a range-b)
	(and (<= (car range-a) (cdr range-b)) (<= (car range-b) (cdr range-a)))
	)

(defun merge-ranges (range-a range-b)
	(cons
		(min (car range-a) (car range-b))
		(max (cdr range-a) (cdr range-b))
		)
	)

(defun crutch-append (value list)
	"will append with nil if requred"
    (if (null list)
        (list value)
        (cons (first list) (crutch-append value (rest list)))))

(defun count-ranges (ranges)
	(reduce
		#'(lambda (acc cur) (+ 1(+ acc (- (cdr cur) (car cur)))))
		ranges
		:initial-value 0
		)
	)

(defun push-range-into-arr (arr target-range)
	"Recursive. Check if range overlap with ranges from arr.
	If it does, merge ranges and try to insert merged range in array.
	If it's not, append new range and return new array"
	(if (null arr)
		(list target-range)
		(let ((copy-arr arr))
			(loop
				for range in arr
				for index from 0
				if (is-overlap range target-range) do (
					let ((arr-without-cur-range (remove range arr)))
						(setf copy-arr (push-range-into-arr arr-without-cur-range (merge-ranges range target-range)))
						(return)
					)
				else do
					(setf copy-arr (crutch-append target-range arr))
				)
				copy-arr
			)
		)
	)


(defun part2 (fresh-ranges)
	"Merge ranges and count"
	(count-ranges
		(reduce
			#'(lambda (acc cur) (push-range-into-arr acc cur))
			fresh-ranges
			:initial-value '()
			)
		)
	)

(defun main ()
	(format t "~a~%" "--- Day 5: Cafeteria ---")
	(let* (
		(input (get-file "day05.txt"))
		(empty-line-index (find-index-empty-string input))
		(fresh-ranges (mapcar
			#'(lambda (range-string)
				(cons (parse-integer (first (split-by-dash range-string))) (parse-integer (second (split-by-dash range-string)))))
			(subseq input 0 empty-line-index)))
		(ingredients-ids (mapcar 
			#'parse-integer (subseq input (+ 1 empty-line-index)))))

		(format t "    Part 1: ~a~%" (part1 fresh-ranges ingredients-ids))
		(format t "    Part 2: ~a~%" (part2 fresh-ranges))
		)

	)

(main)
; (format t "is-overlap ~a~%" (if (is-overlap (cons 112031242283664 117313135215556) (cons 113586476470816 119749061364358)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 1 6) (cons 5 10)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 1 3) (cons 3 10)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 3 10) (cons 1 3)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 1 10) (cons 3 5)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 3 5) (cons 1 10)) "true" "false"))
; (format t "is-overlap ~a~%" (if (is-overlap (cons 1 3) (cons 4 5)) "true" "false"))
; (format t "count-ranges ~a~%" (count-ranges (list (cons 3 5))))



