
(import chicken scheme)

(use color
     extras
     test)

(test '(0.0 1.0 1.0)
      (map exact->inexact
           (color-values (colorspace-convert
                          (make-rgb-color 1 0 0)
                          colorspace-hsv))))

(test '(255 0 0)
      (color-values
       (colorspace-convert
        (make-rgb-color 1 0 0)
        (make-colorspace
         'rgb256 '(r g b)
         (make-encoding make-vector vector-ref vector-set! 255)))))

(test '(255 0 0)
      (color-values
       (colorspace-convert
        (make-rgb-color 1.0 0.0 0.0)
        (make-colorspace
         'rgb256 '(r g b)
         (make-encoding make-vector vector-ref vector-set! 255)))))

(test '(128 0 0)
      (color-values
       (colorspace-convert
        (make-rgb-color 0.5 0.0 0.0)
        (make-colorspace
         'rgb256 '(r g b)
         (make-encoding make-vector vector-ref vector-set! 255)))))

(test-exit)
