(defpackage :surface-x11 
  (:use :cl :surface :ximage))
(in-package :surface-x11)

;;;Surface interface
(defun get-x11-pixmap (&key (width 10) (height 10) (depth 24) location override) (declare (ignore location override))
  (xlib:create-pixmap :width width :height height :depth depth :drawable xwindows:*root-window*))
(defun get-x11-surface (&key (width 10) (height 10) (depth 24) (location '(0 . 0)) (override :on))
  (let* ((win (case depth 
		(24 (xwindows:get-window
		     :width width
		     :height height
		     :x (car location)
		     :y (cdr location)
		     :depth depth
		     :background 0
		     :border 0
		     :override-redirect override))
	       (32 (xwindows:get-window
		    :visual xwindows:*visual32*
		    :width width
		    :height height
		    :x (car location)
		    :y (cdr location)
		    :depth depth
		    :colormap xwindows:*colormap32*
		    :background 0
		    :override-redirect override
		    :border 0 ))))) ;border needs to be non nil or match error occurs because uses depth of root
    win)) 

(setf (getf *available-surfaces* :x-pixmap) #'get-x11-pixmap)
(setf (getf *available-surfaces* :x-window) #'get-x11-surface)

(defmethod surface-depth ((surface xlib:drawable))
  (xlib:drawable-depth surface))

(defmethod prepare-surface ((win xlib:window))
  (let ((backbuffer (window->pixmap win)))
    (tag:add win (cons :backbuffer backbuffer))
    (tag:add win (cons :gcontext (case (surface-depth win)(24 xwindows:*gcontext*)(32 xwindows:*gcontext32*))))
    (unless *network*
      (tag:add win (cons :bb-picture (xlib:render-create-picture backbuffer)))
      (tag:add win (cons :picture (xlib:render-create-picture win))))
    (xlib:clear-area win)
    (xlib:display-force-output xwindows:*display*)))

(defun update-backbuffer (window)
  (declare (type xlib:window window))
  (let* ((gc (or (tag:get window :gcontext)
		(case (surface-depth window)(24 xwindows:*gcontext*)(32 xwindows:*gcontext32*))))
	(old-bb (tag:get window :backbuffer))
	(height (xlib:drawable-height window))
	(width (xlib:drawable-width window))
	(depth (xlib:drawable-depth window))
	(temp-pixmap (xlib:create-pixmap :width width
					 :height height
					 :depth depth
					 :drawable window)))
    (xlib:draw-rectangle temp-pixmap gc 0 0 width height t)
    (xlib:copy-area old-bb gc 0 0 width height temp-pixmap 0 0)
    (xlib:create-pixmap :pixmap old-bb :width width :height height :depth depth :drawable window)
    (xlib:copy-area temp-pixmap gc 0 0 width height old-bb 0 0)
    (xlib:free-pixmap temp-pixmap)))

(defmethod network-update ((win xlib:window))
  (let ((width (xlib:drawable-width win))
	(height (xlib:drawable-height win))
	(backbuffer (tag:get win :backbuffer))
	(gc (tag:get win :gcontext)))
    (xlib:clear-area win)
    (xlib:copy-area backbuffer gc 0 0 width height win 0 0 )
    (xlib:map-window win)    
    (xlib:display-force-output xwindows:*display*)))

(defmethod local-update ((win xlib:window))
  (let ((width (xlib:drawable-width win))
	(height (xlib:drawable-height win))
	(backbuffer (tag:get win :backbuffer))
	(win-pic (tag:get win :picture) (xlib:render-create-picture win))
	(bb-pic (tag:get win :bb-picture) (xlib:render-create-picture pixmap)))
    (tag:add win (cons :picture bb-pic))
    (xlib:clear-area win)
    (xlib:render-composite :src bb-pic nil win-pic 0 0 0 0 0 0 width height)
    (xlib:display-force-output xwindows:*display*)))

(defmethod mapable-p ((win xlib:window))t)
(defmethod mapable-p ((win xlib:pixmap))nil)
(defmethod visible?((win xlib:window)) (if (eql (xlib:window-map-state win) :viewable) t nil))
(defmethod map-surface ((win xlib:window)) (xlib:map-window win) (xlib:display-force-output (xlib:drawable-display win)))
(defmethod unmap-surface ((win xlib:window)) (xlib:unmap-window win))
(defmethod resize-surface ((win xlib:window) size)
  (let ((old-height (xlib:drawable-height win))
	(old-width (xlib:drawable-width win)))
    (destructuring-bind (width . height) size
      (unless (= height old-height)
	(setf (xlib:drawable-height win) height))
      (unless (= width old-width)
	(setf (xlib:drawable-width win) width)))
    (when (tag:get win :backbuffer)(update-backbuffer win))
    (xlib:display-force-output xwindows:*display*)))

(defmethod properties ((win xlib:window))
  (values (cons (xlib:drawable-width win)(xlib:drawable-height win))
	  (cons (xlib:drawable-x win) (xlib:drawable-y win))
	  (xlib:drawable-depth win)))

(defmethod location ((win xlib:window))
  (cons (xlib:drawable-x win) (xlib:drawable-y win)))
(defmethod size ((drawable xlib:drawable))
  (cons (xlib:drawable-width drawable)(xlib:drawable-height drawable)))

(defmethod move-surface ((win xlib:window) location)
  (destructuring-bind (x . y) location  
    (setf (xlib:drawable-x win) x
	  (xlib:drawable-y win) y)
    (xlib:display-force-output xwindows::*display*)))

(defmethod destroy-surface ((win xlib:window))
  (ignore-errors
   (let ((display (xlib:window-display win)))
     (unless *network*
       (xlib:render-free-picture (tag:get win :bb-picture))
       (xlib:render-free-picture (tag:get win :picture)))
     (xlib:free-pixmap (tag:get win :backbuffer))
     (xlib:destroy-window win)
     (xlib:display-force-output display))))

(defmethod destroy-surface ((pixmap xlib:pixmap))
  (xlib:free-pixmap pixmap))

(defmethod get-data ((drawable xlib:drawable))
  (xlib::image-x-data (drawable->image drawable)))

(defun update-image (window)
  (declare (type xlib:window window))
  (let* ((image (drawable->image window)))
    (setf (getf (xlib:window-plist window) :image) image)))

;;;; Display methods
(defmethod display ((array array) (win xlib:window)(pov (eql :image)) &key)
  (let* ((bb (tag:get win :backbuffer))
	 (gc (tag:get win :gcontext))
	 (size (destructuring-bind (a b &rest c)(array-dimensions array)(declare (ignore c))(cons b a))) 
	 (width (car size))
	 (height (cdr size))
	 (image	(xlib:create-image :width width :height height :data array)))
    (resize-surface win size)
    (xlib:put-image bb gc image :x 0 :y 0))
  (map-surface win))

(defmethod display ((array array) (pix xlib:pixmap)(pov (eql :image)) &key)
  (let* ((size (destructuring-bind (a b &rest c)(array-dimensions array)(declare (ignore c))(cons b a))) 
	 (width (car size))
	 (height (cdr size))
	 (gc (if (= 32 (xlib:drawable-depth pix))xwindows:*gcontext32* xwindows:*gcontext*))
	 (image	(xlib:create-image :width width :height height :data array)))
    (xlib:put-image pix gc image :x 0 :y 0)))

(defmethod display ((image xlib:image-z) (win xlib:window) (pov (eql :image)) &key)
  (let* ((bb (tag:get win :backbuffer))
	 (gc (tag:get win :gcontext))	 
	 (width (xlib:image-width image))
	 (height (xlib:image-height image))
	 (size (cons width height)) )
    (resize-surface win size )
    (xlib:put-image bb gc image :x 0 :y 0))
  (map-surface win))

(defmethod display ((pixmap xlib:pixmap) (win xlib:window)(pov (eql :image)) &key)
  (with-drawable pixmap
    (xlib:copy-area pixmap (tag:get win :gcontext) 0 0 (min width (xlib:drawable-width win))
		    (min height (xlib:drawable-height win)) (or (tag:get win :backbuffer) win) 0 0)))
