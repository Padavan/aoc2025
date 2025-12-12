(defun get-file (filename)
    (with-open-file (stream filename)
        (loop
            for line = (read-line stream nil)
            while line
            collect line)
        )
    )

(defun split-by-space (string)
    (loop for i = 0 then (1+ j)
        as j = (position #\space string :start i)
        collect (subseq string i j)
        while j))


(defparameter *device-map* (make-hash-table))
; (defparameter *visited-map* (make-hash-table))


(defun traverse (device)
    "return visited"
    ; (format t "~a~%" device)
    (format t "traverse ~a~%" device)

    (let ((device-outputs (gethash device *device-map*))
          (result 0)
          )
        (format t "device-outputs ~a~%" device-outputs)
        (format t "~a~%" (if (equal device "out") "true" "false"))
        (if (equal device 'out)
            (setf result)
            (setf result (reduce
                #'(lambda (acc next) (+ acc (traverse (next))))
                device-outputs
                :initial-value 0
                )
            ))
      
        (format t "result ~a~%" result)
        result
      )
  )

(defun part-1 ()
  (traverse "you")
)

(defun main ()
    (format t "~a~%" "--- Day 11: Reactor ---")
    (let ((input (get-file "day11.txt")))
        (loop
            for line in input
            as device = (subseq line 0 3)
            as outputs = (split-by-space (subseq line 5))
            do (format t "~a~%" (type-of device))
            do (format t "~a~%" outputs)
            do (setf (gethash device *device-map*) outputs)
            (format t "~a~%" "---puk")
            )
        )
        ; (format t "~a~%" (gethash 'you *device-map*))
        (format t "    Part 1:~a~%" (part-1))
    )

(main)
(format t "~a~%" (loop for key being the hash-keys of *device-map* collect (type-of key)))
(format t "outputs ~a~%" (gethash "you" *device-map*))
; (SIMPLE-ARRAY CHARACTER (3))
