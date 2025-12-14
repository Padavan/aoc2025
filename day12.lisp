(defun get-file (filename)
    (with-open-file (stream filename)
        (loop
            for line = (read-line stream nil)
            while line
            collect line)
        )
    )

(defun split-by (char line)
    (loop for i = 0 then (1+ j)
        as j = (position char line :start i)
        collect (subseq line i j)
        while j))

(defparameter *details-map* (make-hash-table))
(defparameter *area-map* (make-hash-table))

(defun count-diez (str)
  (reduce #'(lambda (acc cur) (+ acc (if (equal #\# cur) 1 0))) (coerce str 'list) :initial-value 0)
  )

(defun main ()
    (format t "~a~%" "--- Day 12: Reactor ---")
    (let* (
        (input (get-file "day12.txt"))
        (separator (position "" input :from-end t :test #'equal))

        (result 0)
        )
      
      
        (loop
          for line in (subseq input 0 separator)
          for i from 0
          as current = (floor i 5)
          do (format t "current ~a~%" current)
          if (= 1 (rem i 5))
            do (setf (gethash current *details-map*) (list line))
            and do (setf (gethash current *area-map*) (count-diez line))
          if (= 2 (rem i 5))
            do (setf (gethash current *details-map*) (append (gethash current *details-map*) (list line)))
            and do (setf (gethash current *area-map*) (+ (gethash current *area-map*) (count-diez line)))
          if (= 3 (rem i 5))
            do (setf (gethash current *details-map*) (append (gethash current *details-map*) (list line)))
            and do (setf (gethash current *area-map*) (+ (gethash current *area-map*) (count-diez line)))
            
          
          )

        (loop
            for line in (subseq input (1+ separator))
            as colon = (position #\: line)
            as dimensions = (cons
                (parse-integer (first (split-by #\x (subseq line 0 colon))))
                (parse-integer (second (split-by #\x (subseq line 0 colon))))
                )
            as units = (mapcar #'parse-integer (split-by #\Space (subseq line (+ colon 2))))
            ; (mapcar #'parse-integer 
            ; do (format t "colon ~a~%" colon)
            as area = (* (car dimensions) (cdr dimensions))
            ; do (format t "dimensions ~a~%" dimensions)
            ; do (format t "units: ~a~%" units)
            ; as unitsarea = (reduce #'(lambda (acc cur) (+ acc (* (parse-integer cur) 9))) units :initial-value 0)
            as unitsarea = (loop
                             for unit-count in units
                             for i from 0
                             sum (* (gethash i *area-map*) unit-count)
                            )
            do (format t "area: ~a~%" area)
            do (format t "unitsarea: ~a~%" unitsarea)
            if (> area unitsarea)
                do (setf result (1+ result))
            )
        
    
    
    ; (separator)

        (format t "    Part 1:~a~%" result)
        )
    )

(main)
; 419 too low
; 437 too low
; 2353

(format t "~a~%" (loop for key being the hash-values of *details-map* collect key))
(format t "~a~%" (loop for key being the hash-values of *area-map* collect key))
