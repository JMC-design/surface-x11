(defpackage :surface-x11 
  (:use :cl :xwindows :surface :tag))
(in-package :surface)

(defun get-x11-surface (&key (width 10) (height 10) (depth 24) (location '(0 . 0)))
  (let ((win (case depth 
	       (24 (xwindows:get-window
		    :width width
		    :height height
		    :x (car location)
		    :y (cdr location)
		    :depth depth
		    :background 0
		    :border 0))
	       (32 (xwindows:get-window
		    :visual xwindows:*visual32*
		    :width width
		    :height height
		    :x (car location)
		    :y (cdr location)
		    :depth depth
		    :colormap xwindows:*colormap32*
		    :background 0
		    :border 0 ))))) ;border needs to be non nil or match error occurs
    (tag:tag win (cons :gcontext (case depth (24 xwindows:*gcontext*) (32 xwindows:*gcontext32*))))
    win)) 

(setf (getf *available-surfaces* :xlib) #'get-x11-surface)

(defmethod surface-depth ((win xlib:window))
  (xlib:drawable-depth win))

(defmethod prepare-surface ((win xlib:window))
  (let* ((pixmap (update-pixmap win)))
    (tag:tag win (cons :pixmap pixmap))
    (tag:tag win (cons :gcontext (case (surface-depth win)(24 xwindows:*gcontext*)(32 xwindows:*gcontext32*))))
    (tag:tag win (cons :backbuffer pixmap))
    ;(xlib:window-event-mask window) (eval xevents::*standard-view-events*) ; fix this eval
    (xlib:clear-area win)
    (xlib:display-force-output xwindows:*display*)))

(defmethod network-update ((win xlib:window))
  (let* ((width (xlib:drawable-width win))
	 (height (xlib:drawable-height win))
	 (pixmap (getf (xlib:window-plist win) :pixmap))
	 (gc (getf (xlib:window-plist win) :gcontext)))
    (xlib:clear-area win)
    (xlib:copy-area pixmap gcontext 0 0 width height win 0 0 )
    (xlib:map-window win)    
    (xlib:display-force-output xwindows:*display*)))

(defmethod local-update ((win xlib:window))
  (let* ((width (xlib:drawable-width win))
	 (height (xlib:drawable-height win))
	 (pixmap (getf (xlib:window-plist win) :pixmap))
	 (gc (getf (xlib:window-plist win) :gcontext))
	; (win-pic (window->picture win))
	; (bb-pic (window->picture pixmap))
	 )
    (xlib:clear-area win)(xlib:map-window win)    
   ; (xlib:render-composite 1 bb-pic :none win-pic 0 0 0 0 0 0 width height)
    (xlib:copy-area pixmap gc 0 0 width height win 0 0 )
    (xlib:map-window win)
    (xlib:display-force-output xwindows:*display*)))

(defmethod visible?((win xlib:window)) (if (eql (xlib:window-map-state win) :viewable) t nil))
(defmethod map-surface ((win xlib:window)) (xlib:map-window win) (xlib:display-force-output (xlib:drawable-display win)))
(defmethod unmap-surface ((win xlib:window)) (xlib:unmap-window win))
(defmethod resize-surface ((win xlib:window) size)
  (let ((height (cdr size))
	(width (car size))
	(old-height (xlib:drawable-height win))
	(old-width (xlib:drawable-width win)))
    (when (/= height old-height)
     (setf (xlib:drawable-height win) height))
    (when (/= width old-width)
      (setf (xlib:drawable-width win) width))
    (update-pixmap win)
    (xlib:display-force-output xwindows:*display*)))
      
(defmethod destroy-surface ((win xlib:window))
  (ignore-errors (let ((pixmap (getf (xlib:window-plist win) :pixmap)))
		   (when pixmap (xlib:free-pixmap pixmap))
		   (xlib:destroy-window win)))
  ) ;add check for pixmaps, images, pictures, or just find a way to deal
					; with them all at once.
(defmethod move ((win xlib:window) location)
  (let ((x (car location))
	(y (cdr location)))
    (setf (xlib:drawable-x win) x)
    (setf (xlib:drawable-y win) y)
    (xlib:display-force-output xwindows::*display*)))

(defun update-pixmap (window)
  (declare (type xlib:window window))
  (let* ((gc (getf (xlib:window-plist window) :gcontext))
	 (old-pixmap (getf (xlib:window-plist window) :pixmap))
	 (height (xlib:drawable-height window))
	 (width (xlib:drawable-width window))
	 (depth (xlib:drawable-depth window))
	 (temp-pixmap (xlib:create-pixmap :width width
					  :height height
					  :depth depth
					  :drawable window)))
    ;(xlib:copy-area  )					;clear pixmap goes here
    (if old-pixmap
	(progn(xlib:copy-area old-pixmap gc 0 0 width height temp-pixmap 0 0)
	      (xlib:create-pixmap :pixmap old-pixmap :width width :height height :depth depth :drawable window)
	      (xlib:copy-area temp-pixmap gc 0 0 width height old-pixmap 0 0))
	(setf (getf (xlib:window-plist window) :pixmap) temp-pixmap))))

(defun update-image (window)
  (declare (type xlib:window window))
  (let* ((image (window->image window)))
    (setf (getf (xlib:window-plist window) :image) image)))

(defun drawable->image (window &optional (x 0) (y 0) (width (xlib:drawable-width window))
			       (height (xlib:drawable-height window)))
  (xlib:get-image window :x x :y y :width width :height height))

(defmethod get-data ((drawable xlib:drawable))
  (xlib::image-x-data (window->image drawable)))

(defun window->picture (window)
 (xlib:render-create-picture window))

;;; figure out dispatching on type
(defmethod display ((array array) (win xlib:window))
  (let* ((bb (tag:get-tag win :backbuffer))
	 (gc (tag:get-tag win :gcontext))
	 (size (destructuring-bind (a b &rest c)(array-dimensions array)(declare (ignore c))(cons b a))) 
	 (width (car size))
	 (height (cdr size))
	 (image	(xlib:create-image :width width :height height :data array)))
    (resize-surface win size )
    (xlib:put-image bb gc image :x 0 :y 0))
  (map-surface win))
