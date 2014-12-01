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

;; encoding
;;

(define-record-type encoding
  (make-encoding constructor getter setter)
  encoding?
  (constructor encoding-constructor)
  (getter encoding-getter)
  (setter encoding-setter))

(define vector-encoding
  (make-encoding make-vector vector-ref vector-set!))


;; colorspace
;;

(define-record-type colorspace
  (%make-colorspace name channels encoding)
  colorspace?
  (name colorspace-name)
  (channels colorspace-channels)
  (nchannels colorspace-nchannels %colorspace-nchannels-set!)
  (encoding colorspace-encoding))

(define make-colorspace
  (case-lambda
   ((name channels encoding)
    (let ((cs (%make-colorspace name channels encoding)))
      (%colorspace-nchannels-set! cs (length (colorspace-channels cs)))
      cs))
   ((name channels)
    (make-colorspace name channels vector-encoding))))


;; color
;;

(define-record-type color
  (%make-color colorspace values values-offset)
  color?
  (colorspace color-colorspace)
  (values %color-values)
  (values-offset color-values-offset color-values-offset-set!))

(define (make-color colorspace . values)
  (let* ((nchannels (colorspace-nchannels colorspace))
         (encoding (colorspace-encoding colorspace))
         (constructor (encoding-constructor encoding))
         (setter (encoding-setter encoding))
         (c (%make-color colorspace (constructor nchannels) 0))
         (%values (%color-values c)))
    (fold (lambda (val i) (setter %values i val) (+ 1 i)) 0 values)
    c))

(define (color-values c)
  (let* ((cs (color-colorspace c))
         (encoding (colorspace-encoding cs))
         (getter (encoding-getter encoding))
         (values (%color-values c)))
    (list-tabulate
     (length (colorspace-channels cs))
     (lambda (i) (getter values i)))))

(define (color-value c channel)
  (let* ((cs (color-colorspace c))
         (channels (colorspace-channels cs))
         (encoding (colorspace-encoding cs))
         (getter (encoding-getter encoding)))
    (getter (%color-values c)
            (+ (color-values-offset c)
               (list-index (lambda (x) (eq? x channel))
                           channels)))))

;;XXX: need a procedure to increment values-offset by the number of
;;     channels in the colorspace

(define colorspace-conversion-functions (list))

(define (colorspace-convert color dst-colorspace)
  (let ((src-colorspace (color-colorspace color)))
    (cond
     ((eq? dst-colorspace (color-colorspace color))
      color)
     #;((colorspace-profile dst-colorspace)
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
  (make-colorspace 'rgb '(r g b)))

(define (make-rgb-color r g b)
  (make-color colorspace-rgb r g b))


;; colorspace-hsv
;;

(define colorspace-hsv
  (make-colorspace 'hsv '(h s v)))

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
  (let ((h (inexact (color-value c 'h)))
        (s (inexact (color-value c 's)))
        (v (inexact (color-value c 'v))))
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
