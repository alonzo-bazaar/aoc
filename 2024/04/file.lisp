;; horsizontal appearences
(defun horizontal-instances (substr str)
  (labels ((rec (i acc)
             (cond ((> (+ i (length substr)) (length str))
                    acc)
                   ((equal substr (subseq str i (+ i (length substr))))
                    (rec (1+ i) (1+ acc)))
                   (t (rec (1+ i) acc)))))
    (rec 0 0)))

(defun xmas-in-row (row)
  (+ (horizontal-instances "XMAS" row)
     (horizontal-instances "SAMX" row)))

;; vertical appearences
(defun vertical-instances (str rows)
  "the length of str should be equal to the number of elements in rows"
  (labels ((good-column-p (i)
             (dotimes (j (length str))
               (unless (equal (char str j) (char (aref rows j) i))
                 (return-from good-column-p nil)))
             t)
           (rec (i acc)
             (cond ((= i (length (aref rows 0)))
                    acc)
                   ((good-column-p i)
                    (rec (1+ i) (1+ acc)))
                   (t
                    (rec (1+ i) acc)))))
    (rec 0 0)))

(defun xmas-in-columns (rows)
  (+ (vertical-instances "XMAS" rows)
     (vertical-instances "SAMX" rows)))


;; diagonals
(defun diagonal-instances-nw-sw (str rows)
  "the length of str should be equal to the number of elements in rows"
  (labels ((good-diagonal-sw-p (i)
             (dotimes (j (length str))
               (unless (equal (char str j)
                              (char (aref rows j) (+ i j)))
                 (return-from good-diagonal-sw-p nil)))
             t)
           (good-diagonal-nw-p (i)
             (dotimes (j (length str))
               (unless (equal (char str j)
                              (char (aref rows (- (length rows) 1 j)) (+ i j)))
                 (return-from good-diagonal-nw-p nil)))
             t)
           (rec (i acc)
             (cond ((> i (- (length (aref rows 0)) (length str)))
                    acc)
                   ((good-diagonal-nw-p i)
                    (if (good-diagonal-sw-p i)
                        (rec (1+ i) (+ 2 acc))
                        (rec (1+ i) (1+ acc))))
                   ((good-diagonal-sw-p i)
                    (rec (1+ i) (1+ acc)))
                   (t
                    (rec (1+ i) acc)))))
    (rec 0 0)))

(defun diagonal-both (str rev rows)
  (labels ((good-diagonal-sw-p (i str)
             (dotimes (j (length str))
               (unless (equal (char str j)
                              (char (aref rows j) (+ i j)))
                 (return-from good-diagonal-sw-p nil)))
             t)
           (good-diagonal-nw-p (i str)
             (dotimes (j (length str))
               (unless (equal (char str j)
                              (char (aref rows (- (length rows) 1 j)) (+ i j)))
                 (return-from good-diagonal-nw-p nil)))
             t)
           (goodp (i)
             (or (and (good-diagonal-nw-p i str)
                      (good-diagonal-sw-p i str))

                 (and (good-diagonal-nw-p i rev)
                      (good-diagonal-sw-p i str))

                 (and (good-diagonal-nw-p i str)
                      (good-diagonal-sw-p i rev))

                 (and (good-diagonal-nw-p i rev)
                      (good-diagonal-sw-p i rev))))
           (rec (i acc)
             (cond ((> i (- (length (aref rows 0)) (length str)))
                    acc)
                   ((goodp i) (rec (1+ i) (+ 1 acc)))
                   (t (rec (1+ i) acc)))))
    (rec 0 0)))

(defun x-mas-in-diagonals (rows)
  (diagonal-both "MAS" "SAM" rows))

(defun xmas-in-diagonals (rows)
  (+ (diagonal-instances-nw-sw "XMAS" rows)
     (diagonal-instances-nw-sw "SAMX" rows)))

(defun rotate-array (arr new)
  "adds new at the end of the array and removes the first element
from the array, this function modifies the original array"
  (dotimes (x (1- (length arr)))
    (setf (aref arr x) (aref arr (1+ x))))
  (setf (aref arr (1- (length arr))) new))

(defun solution (filename) 
  (let ((total 0)
        (arr (make-array 4 :element-type 'string :initial-element "")))
    (with-open-file (fs filename
                        :direction :input)
      ;; file assumed to have more than 4 lines
      
      (dotimes (i 4)
        (let ((line (read-line fs)))
          (setf (aref arr i) line)
          (incf total (xmas-in-row line))))

      (incf total (xmas-in-columns arr))
      (incf total (xmas-in-diagonals arr))

      (do ((line (read-line fs nil nil)
                 (read-line fs nil nil)))
          ((null line))
        (incf total (xmas-in-row line))
        (rotate-array arr line)
        (incf total (xmas-in-columns arr))
        (incf total (xmas-in-diagonals arr)))

      total
      )))

(defun solution-x (filename)
  (let ((total 0)
        (arr (make-array 3 :element-type 'string :initial-element "")))
    (with-open-file (fs filename
                        :direction :input)
      ;; file assumed to have more than 4 lines
      
      (dotimes (i 3)
        (let ((line (read-line fs)))
          (setf (aref arr i) line)))

      (incf total (x-mas-in-diagonals arr))

      (do ((line (read-line fs nil nil)
                 (read-line fs nil nil)))
          ((null line))
        (rotate-array arr line)
        (incf total (x-mas-in-diagonals arr)))

      total)))
                         
; (solution "input")    
