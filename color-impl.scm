;; Copyright 2014--2015 John J Foerch. All rights reserved.
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

;; colorspace
;;

(define-record-type :colorspace
  (%make-colorspace name channels)
  colorspace?
  (name colorspace-name)
  (channels colorspace-channels)
  (nchannels colorspace-nchannels %colorspace-nchannels-set!))

(define (make-colorspace name channels)
  (let ((cs (%make-colorspace name channels)))
    (%colorspace-nchannels-set! cs (length (colorspace-channels cs)))
    cs))


;; color
;;

(define-record-type :color
  (%make-color colorspace values)
  color?
  (colorspace color-colorspace)
  (values %color-values %color-values-set!))

(define (make-color colorspace . values)
  (let* ((nchannels (colorspace-nchannels colorspace))
         (c (%make-color colorspace (make-f64vector nchannels 0)))
         (%values (%color-values c)))
    (unless (null? values)
      (fold (lambda (val i)
              (f64vector-set! %values i (inexact val))
              (+ 1 i))
            0 values))
    c))

(define (color-values c)
  (let ((cs (color-colorspace c))
        (values (%color-values c)))
    (list-tabulate
     (colorspace-nchannels cs)
     (lambda (i) (f64vector-ref values i)))))

(define (color-value c channel)
  (let ((cs (color-colorspace c)))
    (f64vector-ref
     (%color-values c)
     (list-index (lambda (x) (eq? x channel))
                 (colorspace-channels cs)))))

(define (color-values-set! c other)
  (let* ((cs (color-colorspace c))
         (other (if (eq? (color-colorspace other) cs)
                    other
                    (colorspace-convert other cs))))
    (let ((cvals (%color-values c))
          (ovals (%color-values other)))
      (do ((i 0 (+ 1 i))
           (j 0 (+ 1 j))
           (n (colorspace-nchannels cs)))
          ((>= i n))
        (f64vector-set! cvals j (f64vector-ref ovals i))))))


;; color-array
;;

(define-record-type (:color-array :color)
  (%make-color-array colorspace)
  color-array?
  (values-offset color-array-values-offset color-array-values-offset-set!))

(define (color-array-initialize-instance c nelements)
  (let* ((cs (color-colorspace c))
         (nchannels (colorspace-nchannels cs))
         (values (make-f64vector (* nchannels nelements) 0)))
    (%color-values-set! c values)
    (color-array-values-offset-set! c 0)
    c))

(define (make-color-array colorspace nelements)
  (color-array-initialize-instance
   (%make-color-array colorspace)
   nelements))

(define color-array-data %color-values)

(define color-array-data-set! %color-values-set!)

(define (color-array-length c)
  (let* ((cs (color-colorspace c))
         (nchannels (colorspace-nchannels cs)))
    (/ (f64vector-length (color-array-data c)) nchannels)))

(define color-array-set!
  (case-lambda
   ((c other)
    (let* ((cs (color-colorspace c))
           (other (if (eq? (color-colorspace other) cs)
                      other
                      (colorspace-convert other cs)))
           (cvals (%color-values c))
           (offset (color-array-values-offset c))
           (ovals (%color-values other)))
      (do ((i 0 (+ 1 i))
           (j offset (+ 1 j))
           (n (colorspace-nchannels cs)))
          ((>= i n))
        (f64vector-set! cvals j (f64vector-ref ovals i)))))
   ((c i other)
    (let* ((cs (color-colorspace c))
           (nchannels (colorspace-nchannels cs))
           (j (color-array-values-offset c)))
      (color-array-values-offset-set! c (* nchannels i))
      (color-array-set! c other)
      (color-array-values-offset-set! c j)))))

(define (color-array-ref c i)
  (let* ((cs (color-colorspace c))
         (nchannels (colorspace-nchannels cs))
         (c2 (make-color cs))
         (cvals (%color-values c))
         (c2vals (%color-values c2))
         (j (color-array-values-offset c)))
    (color-array-values-offset-set! c (* nchannels i))
    (do ((q (color-array-values-offset c) (+ 1 q))
         (r 0 (+ 1 r))
         (n nchannels))
        ((>= r n))
      (f64vector-set! c2vals r (f64vector-ref cvals q)))
    (color-array-values-offset-set! c j)
    c2))

(define (color-array-for-each c proc)
  (let* ((cs (color-colorspace c))
         (length (f64vector-length (%color-values c)))
         (nchannels (colorspace-nchannels cs)))
    (do ((i 0 (+ nchannels i))
         (j 0 (+ 1 j)))
        ((>= i length))
      (color-array-values-offset-set! c i)
      (proc c j))))

;;XXX: need a procedure to increment values-offset by the number of
;;     channels in the colorspace


;; colorspace-convert
;;

(define colorspace-conversion-functions (list))

(define (colorspace-conversion-add! fromchannels tochannels proc)
  (set! colorspace-conversion-functions
        (cons (cons (list fromchannels tochannels) proc)
              colorspace-conversion-functions)))

(define (colorspace-convert color dst-colorspace)
  (let* ((src-colorspace (color-colorspace color))
         (src-channels (colorspace-channels src-colorspace))
         (dst-channels (colorspace-channels dst-colorspace))
         (knowns src-channels)
         (knowns+values (zip knowns (color-values color)))
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


;; colorspace-hsl
;;

(define colorspace-hsl
  (make-colorspace 'hsl '(h s-hsl l)))

(define (make-hsl-color h s l)
  (make-color colorspace-hsl h s l))


;; colorspace-hsv
;;

(define colorspace-hsv
  (make-colorspace 'hsv '(h s-hsv v)))

(define (make-hsv-color h s v)
  (make-color colorspace-hsv h s v))


;; conversions
;;

(define (rgb->hsl r g b)
  (let* ((M (max r g b))
         (m (min r g b))
         (l (* 0.5 (+ M m))))
    (cond
     ((= M m)
      (list 0.0 0.0 l))
     (else
      (let* ((c (- M m))
             (s (/ c (- 1.0 (abs (- (* 2.0 l) 1.0)))))
             (h (cond
                 ((= r M)
                  (- (/ (- M b) c)
                     (/ (- M g) c)))
                 ((= g M)
                  (+ 2.0 (- (/ (- M r) c)
                            (/ (- M b) c))))
                 (else
                  (+ 4.0 (- (/ (- M g) c)
                            (/ (- M r) c)))))))
        (list (/ h 6.0) s l))))))

(colorspace-conversion-add! '(r g b) '(h s-hsl l) rgb->hsl)

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
        (list (/ h 6.0) s v))))))

(colorspace-conversion-add! '(r g b) '(h s-hsv v) rgb->hsv)

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

(colorspace-conversion-add! '(h s-hsv v) '(r g b) hsv->rgb)
