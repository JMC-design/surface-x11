# surface-x11
X11 implementation of surface

(get-surface :depth 32) will get you an 32bit RGBA x11 window for transparency
(display array surface) will display (array '(unsigned-byte 32) (* *)) on X11 surface, 
premultiply your alpha if you want proper transparency. 
It is the callers responsibility to ensure that the surface is proper depth for the array.
