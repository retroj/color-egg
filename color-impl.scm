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
  (make-encoding constructor getter setter scale)
  encoding?
  (constructor encoding-constructor)
  (getter encoding-getter)
  (setter encoding-setter)
  (scale encoding-scale))

(define vector-encoding
  (make-encoding make-vector vector-ref vector-set! #f))


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
         (scale (encoding-scale encoding))
         (c (%make-color colorspace (constructor nchannels) 0))
         (%values (%color-values c)))
    (fold (lambda (val i)
            (if scale
                (setter %values i (exact (round (* scale val))))
                (setter %values i val))
            (+ 1 i))
          0 values)
    c))

(define color-values
  (case-lambda
   ((c normalized?)
    (let* ((cs (color-colorspace c))
           (encoding (colorspace-encoding cs))
           (getter (encoding-getter encoding))
           (scale (encoding-scale encoding))
           (values (%color-values c)))
      (list-tabulate
       (colorspace-nchannels cs)
       (lambda (i)
         (let ((v (getter values i)))
           (if (and normalized? scale)
               (/ (inexact v) scale)
               v))))))
   ((c)
    (color-values c #f))))

(define color-value
  (case-lambda
   ((c channel normalized?)
    (let* ((cs (color-colorspace c))
           (channels (colorspace-channels cs))
           (encoding (colorspace-encoding cs))
           (getter (encoding-getter encoding))
           (scale (encoding-scale encoding))
           (v (getter (%color-values c)
                      (+ (color-values-offset c)
                         (list-index (lambda (x) (eq? x channel))
                                     channels)))))
      (if (and normalized? scale)
          (/ (inexact v) scale)
          v)))
   ((c channel)
    (color-value c channel #f))))

;;XXX: need a procedure to increment values-offset by the number of
;;     channels in the colorspace

(define colorspace-conversion-functions (list))

(define (colorspace-conversion-add! fromchannels tochannels proc)
  (set! colorspace-conversion-functions
        (cons (cons (list fromchannels tochannels) proc)
              colorspace-conversion-functions)))

(define (colorspace-convert color dst-colorspace)
  (let* ((src-colorspace (color-colorspace color))
         (src-channels (colorspace-channels src-colorspace))
         (dst-channels (colorspace-channels dst-colorspace))
         (src-encoding (colorspace-encoding src-colorspace))
         (dst-encoding (colorspace-encoding dst-colorspace))
         (knowns src-channels)
         (knowns+values (zip knowns (color-values color #t))) ;; normalized
         (unknowns (lset-difference eq? dst-channels knowns)))
    (let ((rule (and (not (null? unknowns))
                     (find (match-lambda
                            (((takes gives) . proc)
                             (and (lset<= eq? takes knowns)
                                  (equal? unknowns (lset-intersection eq? unknowns gives)))))
                           colorspace-conversion-functions))))
      (cond
       ((null? unknowns)
        (apply make-color dst-colorspace
               (map (lambda (ch) (cadr (assoc ch knowns+values eq?)))
                    dst-channels)))
       (rule
        (match-let ((((takes gives) . conversion-proc) rule))
          (let ((result-values
                 (zip gives
                      (apply conversion-proc
                             (map (lambda (ch) (inexact (cadr (assoc ch knowns+values eq?))))
                                  takes)))))
            (apply make-color dst-colorspace
                   (map (lambda (ch) (cadr (assoc ch result-values eq?)))
                        dst-channels)))))
       (else
        (error (sprintf "colorspace-convert could not solve ~S -> ~S" src-channels dst-channels)))))))


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

(define (rgb->hsv r g b)
  (let* ((v (max r g b))
         (mn (min r g b)))
    (cond
     ((= v mn)
      (list 0.0 0.0 v))
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
        (list (modulo (/ h 6.0) 1) s v))))))

(colorspace-conversion-add! '(r g b) '(h s v) rgb->hsv)

(define (hsv->rgb h s v)
  (cond
   ((<= s 0.0) (list v v v))
   (else
    (let* ((hh (* 6.0 (- h (truncate h))))
           (i (truncate hh))
           (f (- hh i))
           (p (* v (- 1.0 s)))
           (q (* v (- 1.0 (* s f))))
           (t (* v (- 1.0 (* s (- 1.0 f))))))
      (cond
       ((= i 0) (list v t p))
       ((= i 1) (list q v p))
       ((= i 2) (list p v t))
       ((= i 3) (list p q v))
       ((= i 4) (list t p v))
       ((= i 5) (list v p q)))))))

(colorspace-conversion-add! '(h s v) '(r g b) hsv->rgb)
