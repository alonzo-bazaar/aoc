(defun string-split-on (str fn &optional (start 0) (acc nil))
  (let* ((first (position-if (complement fn) str :start start))
         (last (when first (position-if fn str :start first))))
    (cond
      ((and first last)
       (string-split-on str fn last (cons (subseq str first last) acc)))
      (first
       (nreverse (cons (subseq str first) acc)))
      (t
       (nreverse acc)))))

(defparameter *five-kind* 0)
(defparameter *four-kind* 1)
(defparameter *full-house* 2)
(defparameter *three-kind* 3)
(defparameter *two-pair* 4)
(defparameter *one-pair* 5)
(defparameter *high-card* 6)

(defun better-type-p (rank1 rank2)
  (< rank1 rank2))

(defun cards-type (s)
  (declare (string s))
  (let ((amounts nil))
    (sequence:dosequence (c s)
      (uiop:if-let (bind (assoc c amounts))
        (incf (cdr bind))
        (push (cons c 1) amounts)))
    ;; (print amounts)
    (flet ((find-amount (n)
             (position-if (lambda (x) (= (cdr x) n)) amounts)))
      (cond ((find-amount 5) *five-kind*)
            ((find-amount 4) *four-kind*)
            ((find-amount 3) (if (find-amount 2)
                                 *full-house*
                                 *three-kind*))
            ((find-amount 2) (if (= (length amounts) 3)
                                 *two-pair*
                                 *one-pair*))
            (t *high-card*)))))

(let ((card-list '(#\A
                   #\K #\Q #\J
                   #\T #\9 #\8 #\7 #\6 #\5 #\4 #\3 #\2)))
  (defun better-card-p (card1 card2)
    (dolist (card card-list)
      (cond ((char= card card2) (return-from better-card-p nil))
            ((char= card card1) (return-from better-card-p t))
            (t nil)))))

(defstruct hand cards hand-type)

(defun hand-from-cards (cards)
  (make-hand :cards cards
             :hand-type (cards-type cards)))

(defun stronger-hand-p (hand1 hand2)
  (or (better-type-p (hand-hand-type hand1) (hand-hand-type hand2))
      (better-cards-p (hand-hand-type hand1) (hand-hand-type hand2))))

(defun prob1-main (filename)
  (let ((acc nil))
  (with-open-file (s filename :direction :input)
    (do ((line (read-line s nil nil) (read-line s nil nil)))
        ((null line))
      (let ((card-bid (string-split-on line #'sb-unicode:whitespace-p)))
        (push (cons (hand-from-cards (car card-bid))
                    (read-from-string (cadr card-bid)))
              acc)))
      
