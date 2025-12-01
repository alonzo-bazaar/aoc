(defstruct guard
  x
  y
  direction)

(defun guard-next-position (guard)
  (case (guard-direction guard)
    (up (values (guard-x guard) (1- (guard-y guard))))
    (down (values (guard-x guard) (1+ (guard-y guard))))
    (left (values (1- (guard-x guard)) (guard-y guard)))
    (right (values (1+ (guard-x guard)) (guard-y guard)))))

(defun guard-move (guard newx newy)
  (setf (guard-x guard) newx)
  (setf (guard-y guard) newy))

(defun direction-rotate (direction)
  "return the direction rotated by 90 degrees to the right"
  (case direction
    (up 'right)
    (down 'left)
    (left 'up)
    (right 'down)))

(defun guard-rotate (guard)
  (setf (guard-direction guard)
        (direction-rotate (guard-direction guard))))

(defun map-position-occupied-p (x y map)
  (char= (char (aref map y) x) #\#))

(defun map-position-out-bounds-p (x y map)
  (or (< x 0)
      (< y 0)
      (>= x (length (aref map 0)))
      (>= y (length map))))

(defun update (guard map)
  "updates the guard according to the map, then returns the guard status"
  (multiple-value-bind (x y) (guard-next-position guard)
    (cond
        ((map-position-out-bounds-p x y map) 'outside)
        ((map-position-occupied-p x y map) (guard-rotate guard) 'rotated)
        (t (guard-move guard x y) 'moved))))

(defun guard-tour-count (guard map stepset)
  (declare (hash-table stepset)) ;;idk how to make sets in common lisp :(
  "takes the guard on a tour of the map, and returns the number distinct position the guard visited before going outside"
  (case (update guard map)
    (outside
     (hash-table-count stepset))
    (rotated
      (guard-tour-count guard map stepset))
     (moved
      (setf (gethash (cons (guard-x guard) (guard-y guard)) stepset) t)
      (setf (aref (aref map (guard-y guard)) (guard-x guard)) #\X)
      (guard-tour-count guard map stepset))))

(defun read-input (filename)
  (let ((map (make-array 0 :adjustable t :fill-pointer 0))
        (guard (make-guard :x 0 :y 0 :direction 'up)))
    (with-open-file (fs filename :direction :input)
      (do ((line (read-line fs nil nil) (read-line fs nil nil))
           (y 0 (1+ y)))
          ((null line) (values map guard))
        (vector-push-extend line map)
        (uiop:if-let ((x (position #\^ line)))
          (progn
            (setf (guard-x guard) x)
            (setf (guard-y guard) y)))))))

(defun main (filename)
  (multiple-value-bind (map guard) (read-input filename)
    (let ((stepset (make-hash-table :test 'equalp)))
      (setf (gethash (cons (guard-x guard) (guard-y guard)) stepset) t)
      (setf (aref (aref map (guard-y guard)) (guard-x guard)) #\X)
      (guard-tour-count guard map stepset))))

(defun guard-stuckable-p (guard map)
  ;; no obstacle in front (we can put one)
  ;; and rotating to the right leads to doing an eternal rectangle
  (and (available-after-move-p x y
                               map
                               (guard-direction guard))
       (leads-to-rectangle-p x y
                             map
                             (direction-rotate (guard-direction guard)))))

(defmacro when-let ((sym val) &rest body)
  `(let ((,sym ,val))
     (when ,sym
       ,@body)))

(defun next-turn-coordinate (x y map direction)
  (let ((xstep (case direction ((up down) 0) (left -1) (right 1)))
        (ystep (case direction ((up -1) (down 1) (left right 0))))) 
    (cazzo-duro x y xstep ystep map)))

(defun cazzod-duro (x y xstep ystep map)
  (let ((x (+ x xstep))
        (y (+ y ystep)))
    (cond ((map-position-out-bounds-p x y map) nil)
          ((map-position-occupied-p x y map) (cons x y))
          (t (cazzo-duro x y xstep ystep map)))))

(defun leads-to-rectangle (x y map direction)
  (let ((curr-x x) (curr-y y) (curr-dir direction))
    (when-let (b (next-turn-coordinate curr-x curr-y map curr-dir))
      (setf curr-x (car a))
      (setf curr-y (cdr a))
      (setf curr-dir (direction-rotate curr-dir))

      (when-let (c (next-turn-coordinate curr-x curr-y map curr-dir))
        (setf curr-x (car a))
        (setf curr-y (cdr a))
        (setf curr-dir (direction-rotate curr-dir))

        (when-let (d (next-turn-coordinate curr-x curr-y map curr-dir))
          (setf curr-x (car a))
          (setf curr-y (cdr a))
          (setf curr-dir (direction-rotate curr-dir))

          (going-straight-meets-p curr-x  curr-y
                                x y
                                map direction))))))

(defun going-straight-meets-p (fromx fromy tox toy map direction)
  (let ((xstep (case direction ((up down) 0) (left -1) (right 1)))
        (ystep (case direction ((up -1) (down 1) (left right 0))))) 
    (or (and (= fromx tox) (fromy toy)) 
        (cazzo-duro-meets-p x y tox toy xstep ystep map))))

(defun cazzod-duro-meets-p (x y tox toy xstep ystep map)
  (let ((x (+ x xstep))
        (y (+ y ystep)))
    (cond ((or (map-position-out-bounds-p x y map)
               (map-position-occupied-p x y map))
           nil)
          ((and (= x tox) (= y toy))
           t)
          (t
           (cazzo-duro-meets-p x y tox toy xstep ystep map)))))

(defun guard-tour-count-stuckable (guard map count)
  (case (update guard map)
    (outside count)
    ((moved rotated)
     (guard-tour-count-stuckable guard map
                                 (if (guard-stuckable-p guard map)
                                     (1+ count)
                                     count)))))
  
(defun guard-tour-trace (guard map maptrace)
  "traces a guard's tour over the whole map"
  (case (update guard map)
    (outside
     maptrace)
    ((moved rotated)
     (push (guard-direction guard)
           (aref maptrace (guard-y guard) (guard-x guard)))
      (guard-tour-trace guard map maptrace))))

(defun available-after-move-p (x y map move)
  (flet ((free-in-bounds (x y)
           (and (not (map-position-out-bounds-p x y map))
                (not (map-position-occupied-p x y map)))))
  (case move
    (up (free-in-bounds x (1- y)))
    (down (free-in-bounds x (1+ y)))
    (left (free-in-bounds (1- x) y))
    (right (free-in-bounds (1+ x) y)))))
