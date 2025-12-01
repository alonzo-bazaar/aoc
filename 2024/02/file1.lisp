(defun allpairs (report pred)
  (cond ((null (cdr report)) t)
        ((funcall pred (car report) (cadr report))
         (allpairs (cdr report) pred))
        (t nil)))

(defun small-delta-p (report)
  (allpairs report (lambda (x y) (>= 3 (abs (- x y))))))

(defun safep (report)
  (and (small-delta-p report)
       (or (allpairs report #'<)
           (allpairs report #'>))))

;; po' più reversed del dovuto, capisco
;; ma non avevo lo sbatti :(
(defun rev-skipping-index (lst skipped)
  (let ((res nil))
  (do ((i 0 (1+ i))
       (lst lst (cdr lst)))
       ((null lst) res)
    (setf res (if (= i skipped)
                  res
                  (cons (car lst) res))))))

(defun rev-range (n)
  (let ((res nil))
    (dotimes (x n)
      (push x res))
    res))
 
(defun damped-candidates (lst)
  (mapcar
   (lambda (x) (rev-skipping-index lst x))
   (rev-range (length lst))))

(defun safedamp-p (repot)
  (or (safep repot)
      (some #'safep (damped-candidates repot))))

(count-if #'safedamp-p *reports*)
  
