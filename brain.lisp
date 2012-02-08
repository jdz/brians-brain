(in-package :brians-brain)

(defun make-brain (w h)
  (make-array (list h w) :element-type '(integer 0 2)))

(defun make-initialised-brain (w h)
  (let ((cells (make-brain w h))
        (mid (floor w 2)))
    (setf (aref cells 0 mid) 1)
    (setf (aref cells 0 (1+ mid)) 1)
    cells))

(defun rules (state neighbours)
  (case state
    (1 2)
    (2 0)
    (t (if (= 2 (count 1 neighbours)) 1 0))))

(defun neighbours (cells x y)
  (let* ((mx (1- (array-dimension cells 1)))
         (my (1- (array-dimension cells 0)))
         (l (if (zerop x) mx (1- x)))
         (r (if (= x mx) 0 (1+ x)))
         (u (if (zerop y) my (1- y)))
         (d (if (= y my) 0 (1+ y))))
    (mapcar (lambda (x y)
              (aref cells y x))
            (list l x r l r l x r)
            (list u u u y y d d d))))

(defun evolve (src)
  (let* ((w (array-dimension src 1))
         (h (array-dimension src 0))
         (dst (make-brain w h)))
    (loop
      for j below h
      do (loop
           for i below w
           do (setf (aref dst j i)
                    (funcall 'rules (aref src j i) (neighbours src i j)))))
    dst))

(defun simulate (steps initial)
  (loop with brain = initial
        repeat steps
        do (setf brain (funcall 'evolve brain))
        finally (return brain)))

(defun benchmark ()
  (format *trace-output* "Benchmarking on ~A ~A~%"
          (lisp-implementation-type)
          (lisp-implementation-version))
  ;; Warmup.
  (simulate 10000 (make-initialised-brain 16 16))
  (loop for (w h i) in '((32    32  32768)
                         (64    64  8192)
                         (128  128  2048)
                         (256  256  512)
                         (512  512  128)
                         (1024 1024 32)
                         (2048 2048 8)
                         (4096 4096 2))
        do (let ((initial (make-initialised-brain w h)))
             (format *trace-output* "*** ~Dx~D ~D iteration~:P ***~%" w h i)
             (time (simulate i initial))
             (finish-output *trace-output*)))
  (values))
