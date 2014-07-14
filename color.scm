;; Copyright 2014 John J Foerch. All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;    1. Redistributions of source code must retain the above copyright
;;       notice, this list of conditions and the following disclaimer.
;;
;;    2. Redistributions in binary form must reproduce the above copyright
;;       notice, this list of conditions and the following disclaimer in
;;       the documentation and/or other materials provided with the
;;       distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY JOHN J FOERCH ''AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL JOHN J FOERCH OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
;; OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
;; ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module color
    (<colorspace> <color>
     color-values color-value
     colorspace-convert
     colorspace-rgb make-rgb-color
     colorspace-hsv make-hsv-color)

(import chicken scheme)

(use (srfi 1)
     coops
     extras
     list-utils)

;; colorspace
;;

(define-class <colorspace> ()
  ((name)
   (channels initform: (list))))


;; color
;;

(define-class <color> ()
  ((colorspace)
   (values initform: (make-vector 0))))

(define (make-color colorspace . values)
  (make <color>
    'colorspace colorspace
    'values (list->vector values)))

(define (color-values c)
  (vector->list (slot-value c 'values)))

(define (color-value c channel)
  (let* ((cs (slot-value c 'colorspace))
         (channels (slot-value cs 'channels)))
    (vector-ref (slot-value c 'values)
                (list-index (lambda (x) (eq? x channel))
                            channels))))

(define colorspace-conversion-functions (list))

(define (colorspace-convert color dst-colorspace)
  (let ((src-colorspace (slot-value color 'colorspace)))
    (cond
     ((eq? dst-colorspace (slot-value color 'colorspace))
      color)
     #;((slot-value dst-colorspace 'profile)
      =>
      (lambda (profile)
        ))
     ((assoc-def (list src-colorspace dst-colorspace) colorspace-conversion-functions)
      =>
      (lambda (conversion)
        (let ((fn (cdr conversion)))
          (fn color))))
     (else
      (error "Don't know how to convert colorspace X to colorspace Y")))))


;; colorspace-rgb
;;

(define colorspace-rgb
  (make <colorspace>
    'name 'rgb
    'channels '(r g b)))


;; rgb-color
;;

(define (make-rgb-color r g b)
  (make-color colorspace-rgb r g b))


;; colorspace-hsv
;;

(define colorspace-hsv
  (make <colorspace>
    'name 'hsv
    'channels '(h s v)))


;; hsv-color
;;

(define (make-hsv-color h s v)
  (make-color colorspace-hsv h s v))


;; conversions
;;

(define (rgb->hsv c)
  (let* ((r (color-value c 'r))
         (g (color-value c 'g))
         (b (color-value c 'b))
         (v (max r g b))
         (mn (min r g b)))
    (cond
     ((= v mn)
      (make-hsv-color 0.0 0.0 v))
     (else
      (let ((s (/ (- v mn) v))
            (h (cond
                ((= r v)
                 (- (/ (- v b) (- v mn))
                    (/ (- v g) (- v mn))))
                ((= g v)
                 (+ 2.0 (- (/ (- v r) (- v mn))
                           (/ (- v b) (- v mn)))))
                (else
                 (+ 4.0 (- (/ (- v g) (- v mn))
                           (/ (- v r) (- v mn))))))))
        (make-hsv-color (modulo (/ h 6.0) 1) s v))))))

(set! colorspace-conversion-functions
      (cons `(,(list colorspace-rgb colorspace-hsv) . ,rgb->hsv)
            colorspace-conversion-functions))

(define (hsv->rgb c)
  (let ((h (exact->inexact (color-value c 'h)))
        (s (exact->inexact (color-value c 's)))
        (v (exact->inexact (color-value c 'v))))
    (cond
     ((<= s 0.0) (make-rgb-color v v v))
     (else
      (let* ((hh (* 6.0 (- h (truncate h))))
             (i (truncate hh))
             (f (- hh i))
             (p (* v (- 1.0 s)))
             (q (* v (- 1.0 (* s f))))
             (t (* v (- 1.0 (* s (- 1.0 f))))))
        (cond
         ((= i 0) (make-rgb-color v t p))
         ((= i 1) (make-rgb-color q v p))
         ((= i 2) (make-rgb-color p v t))
         ((= i 3) (make-rgb-color p q v))
         ((= i 4) (make-rgb-color t p v))
         ((= i 5) (make-rgb-color v p q))))))))

(set! colorspace-conversion-functions
      (cons `(,(list colorspace-hsv colorspace-rgb) . ,hsv->rgb)
            colorspace-conversion-functions))

)
