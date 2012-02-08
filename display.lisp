(in-package :brians-brain)

(defclass bb (glut:window)
  ((cells :accessor cells-of :initarg :cells)
   (brain-width :accessor brain-width-of :initarg :brain-width)
   (brain-height :accessor brain-height-of :initarg :brain-height))
  (:default-initargs
   :title "Brian's Brain in CL"
   :mode '(:double :rgb)))

(defmethod glut:display-window :before ((w bb))
  (gl:clear-color 0 0 0 0)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (brain-width-of w)  0 (brain-height-of w) -1 1))


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
      (1 (gl:color 1 1 1)
       (draw-cell x y))
      (2 (gl:color 0.5 0.5 0.5)
       (draw-cell x y)))))


(defmethod glut:display ((win bb))
  (gl:clear :color-buffer)
  (let* ((cells (cells-of win))
         (w (brain-width-of win))
         (h (brain-height-of win)))
    (loop
      for j below h
      do (loop
           for i below w
           do (render-cell i j (aref cells (+ (* j w) i))))))
  (glut:swap-buffers))


(defmethod glut:idle ((w bb))
  (setf (cells-of w)
        (evolve (cells-of w)
                (brain-width-of w)
                (brain-height-of w)))
  (glut:post-redisplay))

(defun run (w h ww wh)
  (glut:display-window
   (make-instance 'bb
                  :cells (make-initialised-brain w h)
                  :brain-width w
                  :brain-height h
                  :width ww
                  :height wh)))
