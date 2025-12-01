(defun solution-1 (filename)
  (let ((pos 50)
		(times 0))
	(dolist (line (uiop:read-file-lines filename))
	  (let ((lr (char line 0))
			(n (read-from-string (subseq line 1))))
		(case lr
		  ((#\l #\L) (setq pos (mod (- (+ pos 100) n) 100)))
		  ((#\r #\R) (setq pos (mod (+ pos n) 100)))
		  (otherwise
		   (error "bro what kind of instruction is ~A : ~A ~%" lr n)))
		(when (= pos 0) (incf times))))
	times))


(defun solution-2 (filename)
  (let ((pos 50)
		(times 0))
	(dolist (line (uiop:read-file-lines filename))
	  (let ((op
			  (case (char line 0)
				((#\l #\L) 'sub)
				((#\r #\R) 'add)
				(otherwise
				 (error "bro what kind of instruction is ~A~%" (char line 0)))))
			(n (read-from-string (subseq line 1)))
			(oldpos pos))

		;; note: we don't do modulo shit now
		;; if pos was shifted from 50 to 200, it will be at 250
		;; this is so we can use it to count loops
		;; (we will modulo it later)
		(setq pos (if (eq op 'add) (+ pos n) (- pos n)))

		;; count number of times you reached 0 in the current move
		(let ((times-zero (cond
							((= pos 0) 1)
							;; reached 0 without loops

							((> pos 0) (truncate pos 100))
							;; overshot (and thus counted) 0 pos/100 times

							((< pos 0) (+ (truncate (abs pos) 100)
										  (if (= oldpos 0) 0 1)
										  ))
							;; undershot (and thus counted) 0 |pos|/100 times
							;; add 1 if oldpos is not 0, as it means you had
							;; to reach 0 first to then undershoot it |pos|/100
							;; times
							)))
		  (incf times times-zero))

		(setq pos (mod pos 100))
		))
	times))
