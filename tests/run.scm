
(import chicken scheme)

(use color
     extras
     test)

(test '(0.0 1.0 1.0)
      (color-values (colorspace-convert
                     (make-rgb-color 1 0 0)
                     colorspace-hsv)))

(test '(0.5 1.0 1.0)
      (color-values (colorspace-convert
                     (make-rgb-color 0 1 1)
                     colorspace-hsv)))

(test '(0.0 1.0 0.5)
      (color-values (colorspace-convert
                     (make-rgb-color 1 0 0)
                     colorspace-hsl)))

(test '(0.5 1.0 0.5)
      (color-values (colorspace-convert
                     (make-rgb-color 0 1 1)
                     colorspace-hsl)))

(test-exit)
