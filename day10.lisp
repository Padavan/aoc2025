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

(defun update-arr (arr schema times)
    "add 1 to arr according to schema"
    (loop
        for i from 0 to (1- (length arr))
        as j = (if (member i schema) times 0)
        collect (+ (nth i arr) j)
        )
    )

; (defun is-array-valid (arr target-joultage)
;     (every #'<= arr target-joultage)
;     )

(defun check-equation (multipliers equations target-joultage)
    (let ((final t))
        (loop for equation in equations
                for j from 0
                as right-result = (nth j target-joultage)
                as left-result = (reduce #'+ (mapcar #'(lambda (cur) (nth cur multipliers)) equation))
                as result-equal = (= left-result right-result)
                do (setf final (and final result-equal))
                )
        final
      )
    )

(defun traverse (index multipliers equations schematics target-joultage joultage)
          ; (format t "=====index: ~a~%" index)
         ; (format t "multipliers ~a~%" multipliers)
    (let* ((size (length schematics))
          ; (schema-limits (nth index equations))
          (current-schema (nth index schematics))
          (diff (mapcar #'- target-joultage joultage))
          (limits (list))
          )
          ; (upper-limit (loop for l in current-schema minimize (nth l diff)))
            
            
            (loop for equation in equations
                for j from 0
                as result = (nth j target-joultage)
                as contain-index = (member index equation)
                as contain-nil = (some #'null (mapcar #'(lambda (cur) (nth cur multipliers)) equation))
                if (and contain-index contain-nil)
                    do (setf limits (loop for m from 0 to result collect m))
                    ; and do (format t "ifnil ~a~%" (loop for m from 0 to result collect m))
                else if contain-index
                    do (setf limits (list (- result (reduce #'+ (mapcar #'(lambda (cur) (nth cur multipliers)) equation)))))
                )

            ; (format t "limits ~a~%" limits)
            
            (if (= index size)
                (if (check-equation multipliers equations target-joultage)
                    (progn
                      ; (format t "~a~%" "FINAL")
                      ; (format t "multipliers ~a~%" multipliers)
                      0)
                    (progn
                      ; (format t "joultage: ~a~%" joultage)
                      ; (format t "target  : ~a~%" target-joultage)
                      ; (format t "lastindex reached ~a~%" multipliers)
                      nil)
                    )
                
                      (loop
                        for press in limits
                        ; as next-joutage = (update-arr joultage current-schema press)
                        as next-multipliers = (loop for m in multipliers for i from 0 if (= index i) collect press else collect m)
                        as count = (traverse (1+ index) next-multipliers equations schematics target-joultage joultage)
                        ; as count = nil
                        ; do (format t "press: ~a~%" press)
                        if (not (null count))
                            return (+ press count)
                        )
                    )
                
                    
                    
                    ; for d in (nth j equations)
                    ; if (member index d)
          ; )
          ; (format t "upper-limit ~a~%" upper-limit)
          ; (format t "joultage: ~a~%" joultage)
        ; (if (= index 3)
        ;    (progn
        ;      (format t "upper-limit: ~a~%" upper-limit
        ;           )
        ;     ))
          ; (format t "multipliers ~a~%" multipliers)
             ; (format t "deps-list ~a~%" deps-list)
          ; (format t "max-multipliers ~a~%" max-multipliers) 
          ; (format t "diff: ~a~%" diff)
          ; (format t "upper-limit: ~a~%" upper-limit)
          ; (format t "size: ~a~%" size)
        ; (if (= index size)
        ;     (if (equal target-joultage joultage)
        ;         (progn
        ;           ; (format t "~a~%" "FINAL")
        ;           ; (format t "multipliers ~a~%" multipliers)
        ;           0)
        ;         (progn
        ;           ; (format t "joultage: ~a~%" joultage)
        ;           ; (format t "target  : ~a~%" target-joultage)
        ;           ; (format t "lastindex reached ~a~%" multipliers)
        ;           nil)
        ;         )
        ;     (loop
        ;         for press from 0 to upper-limit
        ;         as next-joutage = (update-arr joultage current-schema press)
        ;         as next-multipliers = (loop for m in multipliers for i from 0 if (= index i) collect press else collect m)
        ;         as count = (traverse (1+ index) next-multipliers max-multipliers deps-list schematics target-joultage next-joutage)
        ;         ; as count = nil
        ;         ; do (format t "press: ~a~%" press)
        ;         if (not (null count))
        ;             return (+ press count)
        ;         )
        ;     )
        
            
        )
    )

(defun get-max-multipliers (schematics target-joultage)
    (let (
        (max-multipliers (make-list (length schematics) :initial-element 1000))
        (dep-list (make-list (length target-joultage)))
        )
            (loop
                for joultage in target-joultage
                for i from 0
                do (loop
                    for schema in schematics
                    for schema-index from 0
                    as current-max = (nth schema-index max-multipliers)
                    as is-schema-included = (member i schema)
                    if is-schema-included
                        do (setf (nth schema-index max-multipliers) (min current-max joultage)) 
                        and do (setf (nth i dep-list) (adjoin schema-index (nth i dep-list) ))
                        ; and do (loop
                        ;     for s in schematics
                        ;     for j from 0
                        ;     if (member i s)
                        ;         do (setf (nth i (nth schema-index dep-list)) (adjoin j (nth i (nth schema-index dep-list)) ))
                        ;         ; and do (setf (nth schema-index dep-list) (push schema-index (nth schema-index dep-list)) )
                        ;         ; do (setf (nth i (nth schema-index dep-list)) (adjoin j (nth i (nth schema-index dep-list)) ))
                        ; )
                    )
            
                )
        
        (values max-multipliers dep-list)
    )
  )

(defun count-joultage (schematics target)
    "count clicks to enable joultage parts"
    (format t "schematics ~a~%" schematics)
    (format t "target-joultage: ~a~%" target-joultage)

    (let* (
          (joultage-size (length target-joultage))
          (multipliers (make-list (length schematics) :initial-element nil))
          (matrix (loop
                for schema in schematics
                collect (loop 
                    for i from 0 to (- joultage-size 1)
                    if (member i schema)
                        collect 1
                    else
                        collect 0
                    ))))

          ; (multiple-value-bind (max-multipliers deps-list) (get-max-multipliers schematics target-joultage)
            ; )
      
        (format t "target ~a~%" target)
        (format t "matrix ~a~%" matrix)
      
      ; (format t "dep-list ~a~%" deps-list)
      ; (format t "max-multipliers ~a~%" max-multipliers)
            ; (let ((count (traverse 0 multipliers deps-list schematics target-joultage (make-list joultage-size :initial-element 0))))
              
              ; (format t "FINAL: ~a~%" count)
              ; count
            0
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
                              
        (values light rank (sort schematics 'compare-length) joultage)
       )
    )

(defun main ()
    (format t "~a~%" "--- Day 10: Factory ---")
    (let ((input (get-file "day10.txt")))
        ; (format t "    Part 1: ~a~%" (reduce
            ; #'(lambda (acc line) (multiple-value-bind (light rank schematics joultage) (parse-line line)
            ;   (+ acc (count-min-click light rank schematics))))
            ; input
            ; :initial-value 0))

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
