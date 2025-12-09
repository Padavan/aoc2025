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

(defun part-1 (points)
  (let* ((size (length points)) (point-to-point-area-list
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
  )
  )


(defun is-area-overap-segment (point-a point-b segment)
  (let ((s-start (first segment))
    (s-end (second segment))
        ; area rectangle coords
        (min_x (min (car point-a) (car point-b)))
        (max_x (max (car point-a) (car point-b)))
        (min_y (min (cdr point-a) (cdr point-b)))
        (max_y (max (cdr point-a) (cdr point-b)))
        (result nil))

        ; assume line semgen is horizontal
        (if (and
            (< (cdr s-start) max_y)
            (> (cdr s-start) min_y)
            (or
                (and (>= (car s-start) max_x) (< (car s-end) max_x))
                (and (> (car s-start) min_x) (<= (car s-end) min_x))
                (and (< (car s-start) max_x) (>= (car s-end) max_x))
                (and (<= (car s-start) min_x) (> (car s-end) min_x))
                )
            ) (setf result t))
        
        ; assume line segment is vertical 
        (if (and
            (< (car s-start) max_x)
            (> (car s-start) min_x)
            (or
                (and (>= (cdr s-start) max_y) (< (cdr s-end) max_y))
                (and (> (cdr s-start) min_y) (<= (cdr s-end) min_y))
                (and (< (cdr s-start) max_y) (>= (cdr s-end) max_y))
                (and (<= (cdr s-start) min_y) (> (cdr s-end) min_y))
                )
            ) (setf result t))
        
        ; ()
        ; (format t "   ~a~%" result)
        result
        )
  )

(defun part-2 (points)
    "list of dots (cons)"
    (let* (
        (size (length points)) (point-to-point-area-list
            (loop
                for i from 0 to (- size 1)
                append (loop
                    for j from (1+ i) to (1- size)
                    collect (cons (complex i j) (get-area (nth i points) (nth j points)))
                    )
                ))
        (sorted-point-to-point-area-list (sort point-to-point-area-list 'compare-area))
        (segments (loop
            for i from 0 to (- size 1)
            for j = (- size 1) then (1- i)
            collect (list (nth i points) (nth j points))
            )
        )
        )
            ; (format t "segments ~a~%" points)
            ; (format t "sorted-point-to-point-area-list ~a~%" sorted-point-to-point-area-list)
            ; (format t "segments ~a~%" segments)
            (format t "    Part 2: ~a~%"
                (loop
                    for i from 0 to (1- (length sorted-point-to-point-area-list))
                    as left = (nth (realpart (car (nth i sorted-point-to-point-area-list))) points)
                    as right = (nth (imagpart (car (nth i sorted-point-to-point-area-list))) points)
                    if (not (some #'(lambda (cur) (is-area-overap-segment left right cur)) segments))
                    return (get-area left right)
                    ))
            )
    
    )


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
       )
       (part-1 points)
       (part-2 points)
       
       
     	; (format t "~a~%" points)
      )
	)

; TODO watch proper algorithm
(main)


; (format t "test1 ~a~%" (get-area (cons 2 5) (cons 11 1)))
; (format t "test1 ~a~%" (is-area-overap-segment (cons 2 5) (cons 11 1) (list (cons 7 3) (cons 7 1))))
