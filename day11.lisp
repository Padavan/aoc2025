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


(defparameter *device-map* (make-hash-table :test 'equal))
(defparameter *visited-map* (make-hash-table :test 'equal))

(defun traverse (device end)
    "return visited"
    (let ((device-outputs (gethash device *device-map*))
          (result 0)
          )
        (if (equal device end)
            (setf result 1)
            (setf result (reduce
                #'(lambda (acc next) (+ acc (traverse next end)))
                device-outputs
                :initial-value 0
                )
            ))
        result
      )
    )

(defun traverse-fixed (device end path)
    "return visited"

    (let ((device-outputs (gethash device *device-map*))
      (result 0)
      (updated-path (copy-list path))
      )
    
    (if (gethash device *visited-map*)
        (setf result (gethash device *visited-map*))
        (if (equal device end)
            (setf result 1)
            (progn
                (push device updated-path)
                (setf result (reduce
                    #'(lambda (acc next) (+ acc (traverse-fixed next end updated-path)))
                    device-outputs
                    :initial-value 0
                    ))
                )
            )
        )
    
    (setf (gethash device *visited-map*) result)
    result
    )
    )

(defun clean-vis ()
    (loop
            for item being the hash-keys of *visited-map*
            do (remhash item *visited-map*)
            ))

(defun main ()
    (format t "~a~%" "--- Day 11: Reactor ---")
    (let ((input (get-file "day11.txt")))
        (loop
            for line in input
            as device = (subseq line 0 3)
            as outputs = (split-by-space (subseq line 5))
            do (setf (gethash device *device-map*) outputs)
            )
        )

    (format t "    Part 1:~a~%" (traverse "you" "out"))

    (let ((svr-to-fft (traverse-fixed "svr" "fft" (list nil))))
    (clean-vis)
        (let ((fft-to-dac (traverse-fixed "fft" "dac" (list nil))))
            (clean-vis)
            (let ((dac-to-out (traverse-fixed "dac" "out" (list nil))))
                (clean-vis)
                (format t "    Part 2:~a~%" (* svr-to-fft fft-to-dac dac-to-out))
                )
            )
        )
    )

(main)
