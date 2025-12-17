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

(defun compare-length (a b)
    (> (length b) (length a)))


(defun gauss-this-shit (matrix-old result-old)
    ; (format t "---gauss this matrix--- ~a~%" "")
    ; (format t "result-old ~a~%" result-old)
    ; (print-matrix matrix-old)
    (let* (
        (matrix (copy-list matrix-old))
        (result (copy-list result-old))
        (height (length matrix))
        (widht (length (first matrix)))
        (independant (list))
        (dependant (list))
        )

        (loop
            for k from 0 to (1- widht)
            with row = 0
            as k-row = (loop
                for j from row to (1- height)
                as value = (nth k (nth j matrix))
                if (not (= 0 value))
                    return j
                )

            ; do (format t "k: ~a~%" k)
            ; do (format t "k-row: ~a~%" k-row)
            ; do (print-matrix matrix)
            ; do (format t "k-row ~a~%" k-row)
            ; as diaonal-element = (nth k (nth k matrix))
            ; do (format t "diagonal-element ~a~%" diagonal-element)
            ; do (format t "K ~a~%" k)
            ; find with non zero k element below row and swap
            ; do (loop)
            ; if (= 0 diagonal-element)
            ;     do (format t "swap ~a~%" k)
            ;     do (loop for j from (1+ k) to (- (length matrix) 1)
            ;         ; do (format t "j: ~a~%" j)
            ;         ; do (format t "puk: ~a~%" (nth k (nth j matrix)))
            ;         if (not (= (nth k (nth j matrix)) 0))
            ;             do (rotatef (nth k matrix) (nth j matrix))
            ;             and do (rotatef (nth k result) (nth j result))
            ;             and return nil
            ;     )
            if (null k-row)
                do (push k independant)
                ; and do (setf k (1+ k))
                ; and do (format t "~a~%" "--- SKIP k-row null")
            else
                ; do (format t "~a~%" "ELSE")
                ; Swap
                do (rotatef (nth row matrix) (nth k-row matrix))
                ; and do (format t "swap ~a~%" "")
                and do (rotatef (nth row result) (nth k-row result))

                ; Division
                and do (let ((divider (nth k (nth row matrix))))
                    ; (format t "divider ~a~%" divider)
                    (if (not (= 0 divider))
                        (progn
                            ; (format t "DIVISION ~a~%" divider)
                            (setf (nth row matrix) (mapcar #'(lambda (cur) (/ cur divider)) (nth row matrix)))
                            (setf (nth row result) (/ (nth row result) divider))
                            )
                        )
                    )

                ; and do (format t "~a~%" "before substraction")
                ; and do (format t "~a~%" result)
                ; and do (print-matrix matrix)
                ; Subtraction
                and do (loop
                    for j from 0 to (1- height)
                    as col-under = (nth k (nth j matrix))
                    ; do (format t "diff ~a~%" diff) 
                    ; do (format t "row-to-substract-from ~a~%" row-to-substract-from)
                    if (not (= j row))
                        do (setf (nth j matrix) (mapcar #'- (nth j matrix) (mapcar #'(lambda (cur) (* col-under cur)) (nth row matrix))))
                        and do (setf (nth j result) (- (nth j result) (* col-under (nth row result))))
                    )

                ; and do (format t "~a~%" "puk")
                and do (push k dependant)
                and do (setf row (1+ row))

            ; do (print-matrix matrix)

            )


            ; (format t "dependant ~a~%" dependant)
            ; (format t "independant ~a~%" independant)
            (values matrix result dependant independant)
        )
    )

(defun cars (matrix)
  "Return a list with all the cars of the lists in matrix"
  (if (null matrix)
      nil
      (cons (car (car matrix)) (cars (cdr matrix)))))

(defun cdrs (matrix)
  "Return a list with all the cdrs of the lists in matrix"
  (if (null matrix)
      nil
      (cons (cdr (car matrix)) (cdrs (cdr matrix)))))

(defun transpose (matrix)
  "Transpose matrix"
  (cond ((null matrix) nil)
        ((null (car matrix)) nil)
        (t (cons (cars matrix) (transpose (cdrs matrix))))))

(defun print-matrix (matrix)
    (format t "~a~%" "---")
    (loop for row in matrix
        do (format t "~a~%" row))
    (format t "~a~%" "---")
    )

(defun get-matrix-from-schematics (schematics size)
    (loop
        for schema in schematics
        collect (loop 
            for i from 0 to (- size 1)
            if (member i schema)
                collect 1
            else
                collect 0
            )
        )
    )

(defun get-no-basis-vars (matrix)
    (let* ((height (length matrix))
        (widht (length (first matrix)))
        (freevars (loop
            for col from 0 to (1- widht)
            if (not (equal 1 (nth col (nth col matrix))))
                collect col
            )
        )
        (basisvars (loop
            for col from 0 to (1- widht)
            if (equal 1 (nth col (nth col matrix)))
                collect col
            )
        ))

        (values freevars basisvars)
        )
    )

(defun max-item (list)
  (loop for item in list
        maximizing item))

(defun valid-press (press)
    (and (>= press 0) (integerp press)))

(defun solve-matrix (matrix matrix-result-old independant values dependant)
    (let* ((height (length matrix))
        (widht (length (first matrix)))
        (answers (make-list widht))
        (matrix-result (copy-list matrix-result-old)))




        ; (print-matrix matrix)
        ; (format t "matrix-result ~a~%" matrix-result)
        ; (format t "answers before ~a~%"  answers)
        ; (format t "independant ~a~%" independant)
        ; (format t "values ~a~%" values)
        ; (format t "dependant ~a~%" dependant)
        ; (if (>= 0 (length independant))
            (loop
                for col in independant
                for k in values
                do (setf (nth col answers) k)
                do (loop
                    for row from 0 to (1- height)
                    as value = (nth col (nth row matrix))
                    as row-result = (nth row matrix-result)
                    ; do (format t "col value ~a~%" value)
                    ; do (format t "col k ~a~%" k)
                    ; do (format t "row result value ~a~%" row-result)
                    if (not (= value 0))
                        do (setf (nth row matrix-result) (- row-result (* value k)))
                        ; and do (format t "---DIFF ~a~%" (- row-result (* value k)))
                        ; and do (setf (nth col (nth row matrix)) 0)
                        ; and do (format t "~a~%" 2)
                    )
                )
            ; )

        ; (print-matrix matrix)
        ; (format t "matrix-result ~a~%" matrix-result)
        ; (format t "dependant ~a~%" dependant)

        (loop
            for var in dependant
            ; as var-value = (nth var matrix-result)
            ; do (format t "var ~a~%" var)
            do (loop
                for row from 0 to (1- height)
                as value = (nth var (nth row matrix))
                as row-result = (nth row matrix-result)
                ; do (format t "row ~a~%" row)
                ; do (format t "var ~a~%" var)
                if (not (= 0 value)) 
                    ; do (setf (nth row matrix-result) (- row-result var-value))
                    do (setf (nth var answers) row-result)
                ; do (setf (nth var (nth row matrix)) 0)
                )

            )

        ; (format t "matrix-result after: ~a~%" matrix-result)
        ; (format t "~a~%" "==")
        ; (format t "~a~%" "==")
        ; (print-matrix matrix)
        ; (format t "matrix-result ~a~%" matrix-result)
        ; (format t "answers ~a~%" answers)

        (let ((equation-validation (loop
            for row from 0 to (1- height)
            ; do (format t "~a~%" "---")
            collect (=
                (nth row matrix-result-old)
                (loop
                    for col from 0 to (1- widht)
                    as col-value = (nth col (nth row matrix))
                    as var-value = (nth col answers)
                    ; do (format t "col-value ~a~%" col-value)
                    ; do (format t "var-value ~a~%" var-value)
                    sum (* col-value var-value)
                    )
                )
            )))

            ; (format t "~a~%" equation-validation)


            ; (format t "answers valid?: ~a~%" (if (every #'valid-press answers) "valid" "invalid"))
            (if (and (every #'valid-press answers) (every #'(lambda (v) (not (null v))) equation-validation))
                (progn
                    ; (format t "answers ~a~%" answers)
                    ; (format t "answers ~a~%" (apply #'+ answers))

                    (apply #'+ answers)
                    )
                nil
                )
            )
        ) 
    )


(defun dfs (index max freevars matrix matrix-result values basisvars)
    (let ((size-freevars (length freevars))
        (answer-count 99999))

        (if (= index size-freevars)
            (let ((count (solve-matrix matrix matrix-result freevars values basisvars)))
                (if (not (null count))
                    (setf answer-count count)
                    )
                )
            (loop
                for i from 0 to max
                ; for i from 0 to 1
                as next-values = (append values (list i))
                do (let ((count (dfs (1+ index) max freevars matrix matrix-result next-values basisvars)))
                        ; (format t "recieved count ~a~%" count)
                        (setf answer-count (min count answer-count))
                        )
                )
            )


            answer-count
        )
    )

(defun count-joultage (schematics target)
    "count clicks to enable joultage parts"
    ; (format t "schematics ~a~%" schematics)
    ; (format t "target-joultage: ~a~%" target)

    (let* (
          (joultage-size (length target))
          ; (multipliers (make-list (length schematics) :initial-element nil))
          (matrix (get-matrix-from-schematics schematics joultage-size))
          (max-press (max-item target))

          )

          ; (multiple-value-bind (max-multipliers deps-list) (get-max-multipliers schematics target-joultage)
            ; )
      
        ; (format t "target ~a~%" target)
        ; (format t "first matrix ~a~%" "")
        ; (print-matrix matrix)
        ; (format t "rotated matrix ~a~%" (transpose matrix))
        (multiple-value-bind (gauss-matrix matrix-result dependant independant) (gauss-this-shit (transpose matrix) target)
            ; (format t "gauss-matrix ~a~%" gauss-matrix)
            ; (format t "~a~%" "gauss matrix:")
            ; (print-matrix gauss-matrix)
            ; (format t "matrix-result ~a~%" matrix-result)

            ; (multiple-value-bind (freevars basisvars) (get-no-basis-vars gauss-matrix)
            (dfs 0 max-press independant gauss-matrix matrix-result (list) dependant))
        )
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
    (let ((initial (cons 0 0)) ; const number which represent state of lights, and count
        (numbers (mapcar #'get-number-from-schematics schematics))
        (cache (list 0)))

        (let ((count (loop
            with queue = (list initial)
            as dequeued-item = (first queue)
            if (= light (car dequeued-item))
                return (cdr dequeued-item)
            do (loop
                for item in numbers
                as old-number = (car dequeued-item)
                as old-count = (cdr dequeued-item)
                as new-number = (logxor old-number item)
                if (not (member new-number cache))
                    do (setf queue (append queue (list (cons new-number (1+ old-count)))))
                    and do (push new-number cache)
                )
            do (setf queue (remove-if (constantly t) queue :count 1))
            )))

            count
            )
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
        )
                           
                           ; (sort schematics 'compare-length)   
        (values light rank schematics joultage)
       )
    )

(defun main ()
    (format t "~a~%" "--- Day 10: Factory ---")
    (let ((input (get-file "day10.txt")))
        (format t "    Part 1: ~a~%" (reduce
            #'(lambda (acc line) (multiple-value-bind (light rank schematics joultage) (parse-line line)
              (+ acc (count-min-click light rank schematics))))
            input
            :initial-value 0))

        (format t "    Part 2: ~a~%" (reduce
            #'(lambda (acc line) (multiple-value-bind (light rank schematics joultage) (parse-line line)
                (+ acc (count-joultage schematics joultage))))
            input
            :initial-value 0))
          
        )
    )

(main)

; 415 too low
; z3?

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
; (format t "update-arr ~a~%" (update-arr '(1 2 3 4 5) '(1 3) 3))
; (format t "get-no-basis-vars ~a~%" (get-no-basis-vars (list '(1 0 0 1 -1 -2) '(0 1 0 1 0 -1) '(0 0 1 0 0 1) '(0 0 0 0 1 1))))

; (format t "~a~%" (solve-matrix (list '(1 0 0 1 0 -1) '(0 1 0 0 0 1) '(0 0 1 1 1 0) '(0 0 0 0 1 1)) (list 2 5 4 3) (list 5 3) (list 1 2) (list 4 2 1 0)))

    ; matrix matrix-result-old freevars values basisvars

; (multiple-value-bind (a b) (gauss-this-shit (list '(0 0 0 0 1 1) '(0 0 1 0 1 0) '(0 1 0 1 0 1) '(1 0 1 1 0 0)) (list 3 5 4 7))
;     (print-matrix a)
;     (format t "~a~%" b)

;     )


; (multiple-value-bind (a b) (gauss-this-shit (transpose (list
;   '(0 1 1 1 1 1 1)
;   '(1 0 1 0 1 0 0)
;   '(1 1 1 0 1 0 0)
;   '(0 0 0 1 0 1 1)
;   '(1 0 1 1 1 1 1)
;   '(1 0 0 1 0 1 1)
;   '(1 1 1 1 0 1 1)
;   '(0 0 0 0 1 0 0)
;   '(1 1 1 1 0 1 0)
; )) '(77 44 66 199 46 199 190))
;     (print-matrix a)
;     ; (format t "~a~%" b)

;     )


; '(0 1 0 0 1 0 0 0 1 0)
; '(1 1 0 1 1 1 1 1 1 0)
; '(0 1 0 1 1 0 0 0 0 1)
; '(1 1 1 1 0 0 0 1 0 1)
; '(0 1 0 1 1 0 1 1 1 1)
; '(1 0 1 0 1 1 1 0 1 0)
; '(1 0 0 1 1 1 0 0 0 1)
; '(0 0 1 0 0 0 1 1 0 0)
; '(1 1 1 0 1 1 1 1 1 1)
; '(1 1 0 0 0 1 0 0 0 0)
; '(0 1 1 0 0 0 0 0 1 0)
; '(0 0 0 0 0 0 0 0 1 0)
; '(0 0 1 1 1 1 1 1 1 1)
