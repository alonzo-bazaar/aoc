(defclass wire ()
  ((value :initarg :value
          :accessor wire-value
          :type character
          :initform #\n
          :documentation "value held by the wire, (because enums are for losers), #\1 means the wire's holding a 1, #\0 means it's holding a 0, #\n means it currently holds no value")
   (input-to :initarg :input-to
             :accessor wire-input-to
             :type list
             :initform '()
             :documentation "list of gates to which the wire is input, whether it's input1 or input2 is the gate's business, but the gate needs to know when the wire updates, to update its own value")))

(defclass gate ()
  ((input1 :initarg :input1
           :accessor gate-input1
           :type wire)
   (input2 :initarg :input2
           :accessor gate-input2
           :type wire)
   (output :initarg :gate-output
           :accessor :gate-value
           :type wire)))

(defclass and-gate (gate) ())
(defclass or-gate (gate) ())
(defclass xor-gate (gate) ())

(defmethod wire-add-input ((w wire) (input gate))
  (push (wire-input-to w) input))

(defmethod wire-unset-p ((w wire)) (char= (wire-value w) #\n))
(defmethod wire-set-p ((w wire)) (not (wire-unset-p w)))

(defmethod wire-true-p ((w wire)) (char= (wire-value w) #\1))
(defmethod wire-false-p ((w wire)) (char= (wire-value w) #\0))

(defmethod wire-update ((w wire) (c character))
  (setf (wire-value w) c)
  (dolist (g (wire-input-to w))
    (gate-maybe-update g)))

(defun gate-make (class input1-wire input2-wire output-wire)
  (declare (wire input1-wire input2-wire output-wire))
  (let ((gate (make-instance class :input1 input1-wire
                                   :input2 input2-wire
                                   :output output-wire)))
    (wire-add-input input1-wire gate)
    (wire-add-input input2-wire gate)
    gate))

(defun and-gate-make (input1-wire input2-wire output-wire)
  (gate-make 'and-gate input1-wire input2-wire output-wire))

(defun or-gate-make (input1-wire input2-wire output-wire)
  (gate-make 'or-gate input1-wire input2-wire output-wire))

(defun xor-gate-make (input1-wire input2-wire output-wire)
  (gate-make 'xor-gate input1-wire input2-wire output-wire))

(defmethod gate-maybe-update ((g gate))
  (when (and (wire-set-p (gate-input1 g))
             (wire-set-p (gate-input2 g)))
    (wire-update (gate-output g) (gate-compute-output g))))

(defmethod gate-compute-output ((g and-gate))
  (if (and (wire-true-p (gate-input1 g))
           (wire-true-p (gate-input2 g)))
      #\1
      #\0))

(defmethod gate-compute-output ((g or-gate))
  (if (or (wire-true-p (gate-input1 g))
          (wire-true-p (gate-input2 g)))
      #\1
      #\0))

(defmethod gate-compute-output ((g xor-gate))
  (if (eq (wire-true-p (gate-input1 g))
          (wire-true-p (gate-input2 g)))
      #\0
      #\1))

(defvar wire-registry (make-hash-table :test 'equal))
(defvar z-wire-registry (make-hash-table :test 'equal))

(defun reset-registries ()
  (setf wire-registry (make-hash-table :test 'equal))
  (setf z-wire-registry (make-hash-table :test 'equal)))

(defun register-wire (name wire)
  (declare (string name) (wire wire))
  (setf (gethash name wire-registry) wire)
  (when (char= (char name 0) #\z)
    (setf (gethash name z-wire-registry) wire)))

(defun register-wire-value (name value)
  (declare (string name) (character value))
  (let ((wire (make-instance 'wire :value value)))
    (setf (gethash name wire-registry) wire)
    (when (char= (char name 0) #\z)
      (setf (gethash name z-wire-registry) wire))))

(defun get-wire (name)
  (multiple-value-bind
        (wire present) (gethash name wire-registry)
    (if present
        wire
        (let ((fresh-wire (make-instance 'wire :value #\n)))
          (register-wire name fresh-wire)
          fresh-wire))))

(defun get-z-wires-alist ()
  (let ((acc nil)) 
    (with-hash-table-iterator (get-z z-wire-registry)
      (labels ((add (&optional key val)
                 (when key
                   (push (cons (string-downcase key)
                               (- (char-int val) (char-int #\0)))
                         acc)
                   (multiple-value-call #'add (get-z)))))
        (multiple-value-call #'add (get-z))))
    (sort acc (lambda (x y) (string< (cdr x) (cdr y))))))

(defun get-z-val ()
  (let ((val 0)
        (step 1))
    (dolist (elt (get-z-wires-alist))
      (incf val (* step (cdr elt)))
      (setf step (* 2 step)))
    val))

(ql:quickload :cl-utilities)

(defun solution (filename)
  (with-open-file (fs filename :direction :input)
    (do ((line (read-line fs nil nil)
               (read-line fs nil nil)))
        ((= 0 (length line))) ;; empty line separates initvals from gates
      (let ((wire-name (subseq line 0 (position #\: line)))
            (wire-val (char line (1- (length line)))))
        (register-wire-value wire-name wire-val)))

    (do ((line (read-line fs nil nil)
               (read-line fs nil nil)))
        ((null line))
      (let* ((elts (cl-utilities:split-sequence #\Space line
                                                :remove-empty-subseqs t))
             (input1 (nth 0 elts))
             (method (string-downcase (nth 1 elts)))
             (input2 (nth 2 elts))
             (output (car (last elts))))
        (case (method)
          ("and" (and-gate-make (get-wire input1)
                                (get-wire input2)
                                (get-wire output)))
          ("or" (or-gate-make (get-wire input1)
                              (get-wire input2)
                              (get-wire output)))
          ("xor" (xor-gate-make (get-wire input1)
                                (get-wire input2)
                                (get-wire output))))
          (otherwise (error "unrecognized wire type" method))))
        
