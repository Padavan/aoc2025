(defun get-file (filename)
	(with-open-file (stream filename)
		(loop for line = (read-line stream nil)
			while line
			collect line)))

; HASHMAP to put matrix coordinate as key and true as value. empty coordinates ignored
(defparameter *point-map* (make-hash-table))

(defun get-adj-positions (point)
	"Point is complex number. Return list of adjacent positions"
	(let (
		(N  (+ point #C( 0  1)))
		(NW (+ point #C(-1  1)))
		(NE (+ point #C( 1  1)))
		(E  (+ point #C( 1  0)))
		(W  (+ point #C(-1  0)))
		(S  (+ point #C( 0 -1)))
		(SE (+ point #C( 1 -1)))
		(SW (+ point #C(-1 -1)))
		)

	(list N NE E SE S SW W NW)
	)
	)

(defun is-paper-accessable (point)
	"Check if paper have less than 4 neibour paper"
	(let ((neibours-count (reduce
		#'(lambda (acc cur) (+ acc (if (gethash cur *point-map*) 1 0)))
		(get-adj-positions point)
		:initial-value 0
		)))
	(if (< neibours-count 4) t nil)
	)
	)

(defun count-hasmap-valid-keys (kv)
	(let ((counter 0))
		(loop
			for point being the hash-keys of kv
			do (if (gethash point kv) (setf counter (+ counter 1)))
			)
		counter
		)
	)

(defun rec-remove-paper (point-map)
	(let ((before-removal (count-hasmap-valid-keys point-map)))
		(loop
			for point being the hash-keys of *point-map*
			do (if (is-paper-accessable point) (remhash point point-map))
			)

		(if (= (count-hasmap-valid-keys point-map) before-removal)
			point-map
			(rec-remove-paper point-map)
			)
		)

	)

(defun main ()
	(format t "~a~%" "--- Day 3: Lobby ---")

	(let ((matrix (get-file "day04.txt")))

		(loop
			for row in matrix
			for row-index from 0
			do (loop
				for col in (coerce row 'list)
				for col-index from 0
				do (if
					(equal col #\@)
					(setf (gethash (complex col-index row-index) *point-map*) t)
					)
				)
			)
		)

	; PART 1
	(let ((paper-counter 0))
		(loop
			for point being the hash-keys of *point-map*
			do (if (is-paper-accessable point) (setf paper-counter (+ paper-counter 1)))
			)
		(format t "    Part 1: ~a~%" paper-counter)
		)

	; PART 2
	(let* (
		(start-paper-counter (count-hasmap-valid-keys *point-map*))
		(point-map-at-end (rec-remove-paper *point-map*)))

		(format t "    Part 2: ~a~%"
			(- start-paper-counter (count-hasmap-valid-keys point-map-at-end))
			)
		)

	)


(main)

; (format t "~a~%" (gethash '#C(0 0) *point-map*)) ; should be nil for test
; (format t "~a~%" (gethash (complex 3 0) *point-map*)) ;
; (format t "~a~%" (gethash #C(3 0) *point-map*))
; (format t "~a~%" (get-adj-positions #C(3 0)))
; (format t "~a~%" (loop for key being the hash-keys of hash-table collect key))
