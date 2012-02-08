(in-package :brians-brain)

(defclass bb (glut:window)
  ((cells :accessor cells-of :initarg :cells))
  (:default-initargs
   :title "Brian's Brain in CL"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w bb))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (let ((cells (cells-of w)))
    (gl:ortho 0 (array-dimension cells 1)  0 (array-dimension cells 0) -1 1)))


(defun render-cell (x y cell)
  (flet ((draw-cell (x y)
           (gl:with-pushed-matrix
             (gl:translate x y 0)
             (gl:with-primitive :polygon
               (gl:vertex 0.1 0.1 0)
               (gl:vertex 0.9 0.1 0)
               (gl:vertex 0.9 0.9 0)
               (gl:vertex 0.1 0.9 0)))))
    (case cell
      (:on (gl:color 1 1 1)
       (draw-cell x y))
      (:dying (gl:color 0.5 0.5 0.5)
       (draw-cell x y)))))


(defmethod glut:display ((w bb))
  (gl:clear :color-buffer)
  (let* ((cells (cells-of w))
         (w (array-dimension cells 1))
         (h (array-dimension cells 0)))
    (loop
      for j below h
      do (loop
           for i below w
           do (render-cell i j (aref cells j i)))))
  (glut:swap-buffers))


(defmethod glut:idle ((w bb))
  (setf (cells-of w) (evolve (cells-of w)))
  (glut:post-redisplay))

(defun run (w h ww wh)
  (glut:display-window
   (make-instance 'bb
                  :cells (make-initialised-brain w h)
                  :width ww
                  :height wh)))
