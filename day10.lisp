(defun get-file (filename)
    (with-open-file (stream filename)
        (loop
            for line = (read-line stream nil)
            while line
            collect line)
        )
    )

(defun split-by-comma (string)
    (loop for i = 0 then (1+ j)
        as j = (position #\, string :start i)
        collect (subseq string i j)
        while j))

(defun update-arr (arr schema)
    "add 1 to arr according to schema"
    ; (format t "update-arr arr ~a~%" arr)
    ; (format t "update-arr schema ~a~%" schema)
    ; (let ((new-arr arr))
    ;     (loop
    ;         for num in schema
    ;         as cur = (nth num new-arr)
    ;         do (setf (nth num new-arr) (1+ cur))
    ;         )
    ;     new-arr
    ;     )
    (loop
        for i from 0 to (1- (length arr))
        as j = (if (member i schema) 1 0)
        collect (+ (nth i arr) j)
        )
    )

(defun is-array-valid (arr target-joultage)
    (every #'<= arr target-joultage)
    )

(defun count-joultage (schematics target-joultage)
    "count clicks to enable joultage parts"
    ; (format t "schematics ~a~%" schematics)
    ; (format t "target-joultage: ~a~%" target-joultage)

    (let* (
        (size (length target-joultage))
        (initial-state (cons (make-list size :initial-element 0) 0))
        (cache (list (car initial-state)))
        )

        ; (format t "initial-state ~a~%" initial-state)
        ; (format t "size ~a~%" size)

        (let ((count (loop
            with queue = (list initial-state)
            as dequeued-item = (first queue)
            repeat 5
            ; do (format t "dequeued-item: ~a~%" dequeued-item)
            ; do (format t "queue ~a~%" queue)
            do (format t "cache ~a~%" cache)
            if (equal target-joultage (car dequeued-item))
                return (cdr dequeued-item)
            ; return 0
            do (loop
                for schema in schematics
                as old-arr = (car dequeued-item)
                as old-count = (cdr dequeued-item)
                as new-arr = (update-arr old-arr schema)
                ; (is-array-valid new-arr target-joultage)
                ; do (format t "--new-arr ~a~%" new-arr)
                ; do (format t "--target-joultage ~a~%" target-joultage)
                ; do (format t "--~a~%" (if (member new-arr cache :test #'equal) "cached" "notcached"))
                ; do (format t "--condition2: ~a~%" (if (is-array-valid new-arr target-joultage) "true" "false"))
                if (and (not (member new-arr cache :test #'equal)) (is-array-valid new-arr target-joultage))
                    do (setf queue (append queue (list (cons new-arr (1+ old-count)))))
                    and do (push new-arr cache)
                )
            do (setf queue (remove-if (constantly t) queue :count 1))
            )))
        (format t "count: ~a~%" count)
        (if (null count) 0 count)

        ))
  )

(defun get-number-from-schematics (schematic)
    (reduce
        #'(lambda (acc cur) (+ acc (expt 2 cur)))
        schematic
        :initial-value 0    
        )
    )

(defun count-min-click (light rank schematics)
    "count minimal clicks"
    ; (format t "light ~a~%" light)
    ; (format t "rank ~a~%" rank)
    ; (format t "schematics ~a~%" schematics)
    (let ((initial (cons 0 0)) ; const number which represent state of lights, and count
        (numbers (mapcar #'get-number-from-schematics schematics))
        (cache (list 0))
        ; (target-rem (loop for i from 0 to rank sum (expt 2 i)))
        )
      
        ; (format t "light ~a~%" light) 
        ; (format t "numbers ~a~%" numbers)
        ; (format t "initial ~a~%" initial) 
      
        (let ((count (loop
            with queue = (list initial)
            as dequeued-item = (first queue)
            ; repeat 4
            ; with dequeued-item = (remove-if (constantly t) queue :count 1)
            ; do (format t "queue ~a~%" queue)
            ; do (format t "dequeued-item: ~a~%" dequeued-item)
            ; do (format t "cache ~a~%" cache) 
            ; do (format t "rem ~a~%" (rem (car dequeued-item) (expt 2 rank)))
            if (= light (car dequeued-item))
                return (cdr dequeued-item)
            ; do (format t "length ~a~%" (length queue))
            do (loop
                 for item in numbers
                 as old-number = (car dequeued-item)
                 as old-count = (cdr dequeued-item)
                 as new-number = (logxor old-number item)
                 ; do (format t "item ~a~%" item)
                 if (not (member new-number cache))
                     do (setf queue (append queue (list (cons new-number (1+ old-count)))))
                     and do (push new-number cache)
            )
            do (setf queue (remove-if (constantly t) queue :count 1))
            ; do (delete dequeued-item queue)
            
            
            )))
            ; (format t "count: ~a~%" count)
        count)
      
        )
    )

(defun parse-line (line)
  "from line like [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
  to something idk"
  (let (
        (light 0)
        (char-list (coerce line 'list))
        (schematics (list '(nil)))
        (joultage (list '(nil)))
        (rank 0)
        (light-end (position #\] line))
        (joultage-start (position #\{ line))
        (line-lenght (length line))
        )

        (loop
            for light-item in (subseq char-list 1 light-end)
            for j from 0
            ; do (format t "~a~%" (reverse (subseq char-list 1  light-end)))
            if (equal light-item #\#)
                do (setf light (+ light (expt 2 j)))
                and do (setf rank light-end)
            )
        
        (setf joultage (mapcar #'parse-integer (split-by-comma (subseq line (1+ joultage-start) (1- line-lenght)))))
        
        (let*
          ((schematics-str (subseq line (1+ light-end) joultage-start)))
          
        (setf schematics (loop
            for i = (position '#\( schematics-str :start 0)
            then (+ j 2) 
            as j = (position '#\)  schematics-str :start i)
            while j
            collect (mapcar #'parse-integer (split-by-comma (subseq schematics-str (1+ i) j)))
            ))
            ; (format t "schematics-str ~a~%" schematics-str)
        )
                              
        ; (format t "light ~19,,' ,4B~%" light)
        ; (format t "light ~a~%" light)
        ; (format t "rank ~a~%" rank)
        ; (format t "joultage ~a~%" joultage)
        ; (format t "schematics ~a~%" schematics)
        (values light rank schematics joultage)
       )
    )

(defun main ()
    (format t "~a~%" "--- Day 10:  ---")
    (let ((input (get-file "day10.txt")))
        ; (format t "    Part 1: ~a~%" (reduce
            ; #'(lambda (acc line) (multiple-value-bind (light rank schematics joultage) (parse-line line)
            ;   (+ acc (count-min-click light rank schematics))))
            ; input
            ; :initial-value 0))

        (format t "    Part 2: ~a~%" (reduce
            #'(lambda (acc line) (multiple-value-bind (light rank schematics joultage) (parse-line line)
                (+ acc (count-joultage schematics joultage))))
            (list (first input))
            :initial-value 0))
          
        )
    )

(main)

; 415 too low

; (format t "~a~%"  (rem #b10101 #b10101))


; (format t "~a~%"  #b0110)
; (format t "~a~%"  15)
; (format t "light ~19,,' ,4B~%" 15)
; (format t "~a~%"  6)
; (format t "light ~19,,' ,4B~%" 6)
; (format t "light ~19,,' ,4B~%" 5)
; (format t "light ~19,,' ,4B~%" 1)
; (format t "light ~19,,' ,4B~%" (logxor 0 5))
; (format t "~a~%" (logxor 3 (logxor 0 5)))
; (format t "~a~%" (if (equal '(0 1 2) '(0 2 2)) "true" "false"))
; (format t "~a~%" (update-arr '(0 0 0 0 0) '(2 3)))
; (format t "~a~%" (if (is-array-valid '(1 2 3 4 6) '(6 6 6 6 6)) "true" "false"))
; (format t "~a~%" (member '(0 0 0) (list '(0 0 0)) :test #'equal))
; (format t "~a~%" (member '(0 0 1) (list '(0 0 0)) :test #'equal))
; (format t "~a~%"  #b110)
; (format t "~a~%"  #b1110)
