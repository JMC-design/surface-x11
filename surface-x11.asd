(in-package :asdf-user)
(defsystem "surface-x11"
  :description "X11 implementation of surface functions."
  :version "0.0.1"
  :author "Johannes Martinez Calzada"
  :licence "llgpl"
  :depends-on ("surface" "xutils" "tag" )
  :components ((:file "surface-x11")))
