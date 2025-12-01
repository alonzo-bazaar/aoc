;; scan file
;; find 'mul('
;; if 1-3 numbers + comma + 1-3 numbers + )
;; add that
;; else
;; go on

;; utilities
(defun arr-str-eq (arr str)
  "returns wether an array and a string describe the same sequence of characters"
  (when (= (length arr) (length str))
    (dotimes (i (length arr))
      (unless (char= (aref arr i) (char str i))
        (return-from arr-str-eq nil))))
  t)

(defun rotate-array (arr new)
  "adds new at the end of the array and removes the first element from the array, this function modifies the original array"
  (dotimes (x (1- (length arr)))
    (setf (aref arr x) (aref arr (1+ x))))
  (setf (aref arr (1- (length arr))) new))

;; reading array
(defun read-n-in-array (is nchars)
  "reads n characters from stream and puts them in array, if eof is met returns nil, otherwise it returns the array"
  (let ((res (make-array nchars :element-type 'character)))
    (dotimes (x nchars)
      (let ((c (read-char is nil)))
        (if c (setf (aref res x) (read-char is))
            (return-from read-n-in-array nil))))
    res))

(defun scan-until-string (is str)
  "return nil if the scan did not go well, returns t if the scan went well, and stream is now still and past the read"
  (uiop:if-let ((first-n (read-n-in-array is (length str))))
    (do ((x first-n (uiop:if-let ((ch (read-char is nil)))

        
(defun scan-numbers (is)
  "two numbers, separated by a comma, with a paren at the end, nothing else in between
find them, if they're there, multiply them
if there's nothing, return nil"
  (labels ((num-and-over (acc left proper-ender)
             (if (<= 0 left)
                 nil
                 (let ((ch (read-char nil is)))
                   (cond ((null ch) nil)
                         ((char= ch proper-ender) acc)
                         ((digit-char-p ch) (num-and-over
                                             (+ (* acc 10) (digit-char-p ch))
                                             (1- left)
                                             proper-ender))
                         (t nil))))))
    (let* ((one (num-and-over 0 3 #\,))
           (two (and one (num-and-over 0 3 #\)))))
      (and one two (* one two)))))
  
(defun add-read-numbers (is acc)
  (if (scan-until-string is "mul(")
      (uiop:if-let ((x (scan-numbers is)))
        (progn (print x) (terpri) (add-read-numbers is (+ x acc))))
      acc))

(with-open-file (is "input"
                    :direction :input)
  (add-read-numbers is 0))

