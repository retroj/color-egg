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
    (color-values
     <colorspace> <color>
     colorspace-convert
     <colorspace-rgb> colorspace-rgb <rgb-color> make-rgb-color
     <colorspace-hsv> colorspace-hsv <hsv-color> make-hsv-color)

(import chicken scheme)

(use (srfi 1)
     coops
     extras)

;; colorspace
;;

(define-generic (colorspace-convert color dest-colorspace))

(define-class <colorspace> ()
  ((name)
   (channels initform: (list))))


;; color
;;

(define-generic (color-values c))

(define-class <color> ()
  ((colorspace)))

(define-method (color-values (c <color>))
  (cond
   ((slot-initialized? c 'colorspace)
    (let ((cs (slot-value c 'colorspace)))
      (map (lambda (x) (slot-value c x)) (slot-value cs 'channels))))
   (else (list))))

(define-method (colorspace-convert (c <color>) (cs <colorspace>))
  (if (eq? cs (slot-value c 'colorspace))
      c
      (error "Don't know how to convert colorspace")))


;; colorspace-rgb
;;

(define-class <colorspace-rgb> (<colorspace>)
  ((name initform: 'rgb)
   (channels initform: '(r g b))))

(define colorspace-rgb (make <colorspace-rgb>))


;; rgb-color
;;

(define-class <rgb-color> (<color>)
  ((colorspace initform: colorspace-rgb)
   (r initform: 0)
   (g initform: 0)
   (b initform: 0)))

(define (make-rgb-color r g b)
  (make <rgb-color> 'r r 'g g 'b b))


;; colorspace-hsv
;;

(define-class <colorspace-hsv> (<colorspace>)
  ((name initform: 'hsv)
   (channels initform: '(h s v))))

(define colorspace-hsv (make <colorspace-hsv>))


;; hsv-color
;;

(define-class <hsv-color> (<color>)
  ((colorspace initform: colorspace-hsv)
   (h initform: 0)
   (s initform: 0)
   (v initform: 0)))

(define (make-hsv-color h s v)
  (make <hsv-color> 'h h 's s 'v v))


;; conversions
;;

;; rgb -> hsv
(define-method (colorspace-convert (c <rgb-color>) (cs <colorspace-hsv>))
  (let* ((r (slot-value c 'r))
         (g (slot-value c 'g))
         (b (slot-value c 'b))
         (v (max r g b))
         (mn (min r g b)))
    (cond
     ((= v mn)
      (make <hsv-color> 'h 0.0 's 0.0 'v v))
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
        (make <hsv-color> 'h (modulo (/ h 6.0) 1) 's s 'v v))))))

;; hsv -> rgb
(define-method (colorspace-convert (c <hsv-color>) (cs <colorspace-rgb>))
  (let ((h (exact->inexact (slot-value c 'h)))
        (s (exact->inexact (slot-value c 's)))
        (v (exact->inexact (slot-value c 'v))))
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

)
