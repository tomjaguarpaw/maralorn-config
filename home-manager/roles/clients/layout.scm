;; (define (install-handler x y) x)

(define (user-command cmd tags output) 0)

(define (mono view-count width height)
                                  (letrec ((iter (lambda (n)
                                                   (if (eq? 0 n)
                                                       '()
                                                       (append (list (list 0 0 width height))
                                                               (iter (1- n)))))))
                                    (iter view-count)))

(define threesplit 0.59)

(define (three view-count width height)
  (letrec ((lrel (lambda (a b c d) (rel width height a b c d)))
          (lrelo (lambda (a b c d) (relo width height a b c d)))
          (nthsplit (lambda (n N) (lrel 0.8 (/ n N) 0.2 (/ (+ n 1) N))))
          (vsplit (lambda (n N) (if (eq? n -1) '() (cons (nthsplit n N) (vsplit (- n 1) N)))))
          )
    (cond
      ((eq? view-count 0)         '())
      ((eq? view-count 1)         (list (lrel 0 0 threesplit 1)))
      ((eq? view-count 2)         (list (lrelo threesplit 0 (- 1 threesplit) 1) (lrel 0 0 threesplit 1)))
      ((eq? view-count 3)         (list (lrelo threesplit 0 (- 1 threesplit) 0.5) (lrelo threesplit 0.5 (- 1 threesplit) 0.5) (lrel 0 0 threesplit 1)))
      ((eq? view-count 4)         (list (lrelo threesplit 0 (- 1 threesplit) 0.5) (lrelo threesplit 0.5 (- 1 threesplit) 0.5) (lrel 0 0.6 threesplit 0.4) (lrel 0 0 threesplit 0.6)))
      (else               (append (list (lrelo threesplit 0 (- 1 threesplit) 0.5) (lrelo threesplit 0.5 (- 1 threesplit) 0.5) (lrel 0 0.6 threesplit 0.4) (lrel 0 0 threesplit 0.6)) (repeat (- view-count 4) (lrelo threesplit 0 (- 1 threesplit) 1)))
      )
    )
  )
)

(define (repeat n x)
  (if (eq? n 0) '() (cons x (repeat (- n 1) x)))
)

(define bar-offset 200)

(define (rel width height startx starty w h)
  (list (* width startx) (* height starty) (* width w) (* height h)))

(define (relo width height startx starty w h)
  (list (* width startx) (+ bar-offset (* (- height bar-offset) starty)) (* width w) (* (- height bar-offset) h)))

(define (five view-count width height)
  (letrec ((lrel (lambda (a b c d) (rel width height a b c d)))
          (lrelo (lambda (a b c d) (relo width height a b c d)))
          (nthsplit (lambda (n N) (lrel 0.8 (/ n N) 0.2 (/ 1 N))))
          (vsplit (lambda (n N) (if (eq? n -1) '() (cons (nthsplit n N) (vsplit (- n 1) N)))))
          )
    (cond
      ((eq? view-count 0)         '())
      ((eq? view-count 1)         (list (lrelo 0.2 0 0.6 1)))
      ((eq? view-count 2)         (list (lrelo 0 0 0.5 1)     (lrelo 0.5 0 0.5 1)))
      ((eq? view-count 3)         (list (lrelo 0.4 0 0.2 1)   (lrel 0.6 0 0.4 1)      (lrel 0 0 0.4 1)))
      ((eq? view-count 4)         (list (lrelo 0.4 0 0.2 1)   (lrel 0.6 0 0.4 1)      (lrel 0 0.5 0.4 0.5)   (lrel 0 0 0.4 0.5)))
      ((eq? view-count 5)         (list (lrelo 0.4 0 0.2 1)   (lrel 0.6 0 0.2 1)      (lrel 0.8 0 0.2 1)     (lrel 0 0.5 0.4 0.5)   (lrel 0 0 0.4 0.5)))
      ((eq? view-count 6)         (list (lrelo 0.4 0 0.2 1)   (lrel 0.6 0 0.2 1)      (lrel 0.8 0 0.2 0.5)   (lrel 0.8 0.5 0.2 0.5) (lrel 0 0.5 0.4 0.5)   (lrel 0 0 0.4 0.5)))
      ((eq? view-count 7)         (list (lrelo 0.4 0 0.2 1)   (lrel 0.6 0 0.2 0.5)    (lrel 0.6 0.5 0.2 0.5) (lrel 0.8 0 0.2 0.5)   (lrel 0.8 0.5 0.2 0.5) (lrel 0 0.5 0.4 0.5)    (lrel 0 0 0.4 0.5)))
      ((eq? view-count 8)         (list (lrelo 0.4 0 0.2 0.5) (lrelo 0.4 0.5 0.2 0.5) (lrel 0.6 0 0.2 0.5)   (lrel 0.6 0.5 0.2 0.5) (lrel 0.8 0 0.2 0.5)   (lrel 0.8 0.5 0.2 0.5)  (lrel 0 0.5 0.4 0.5) (lrel 0 0 0.4 0.5)))
      (else               (append (list (lrelo 0.4 0 0.2 0.5) (lrelo 0.4 0.5 0.2 0.5) (lrel 0.6 0 0.2 0.5)   (lrel 0.6 0.5 0.2 0.5))
                                  (vsplit (- view-count 7) (- view-count 6))
                                  (list (lrel 0 0.5 0.4 0.5) (lrel 0 0 0.4 0.5))
                          )
      )
    )
  )
)


(define (layout view-count width height tags output)
  (cond
      ((> width 3000) (five  view-count width height))
      (else           (three view-count width height))
  )
)

(install-handler 'user-command user-command)
(install-handler 'layout-demand layout)

;; (display (layout 1 3840 2160 3 0))
;; (newline)
;; (display (layout 2 3840 2160 3 0))
;; (newline)
;; (display (layout 3 3840 2160 3 0))
;; (newline)
;; (display (layout 4 3840 2160 3 0))
;; (newline)
;; (display (layout 5 3840 2160 3 0))
;; (newline)
;; (display (layout 6 3840 2160 3 0))
;; (newline)
;; (display (layout 7 3840 2160 3 0))
;; (newline)
;; (display (layout 8 3840 2160 3 0))
;; (newline)
;; (display (layout 9 3840 2160 3 0))
;; (newline)
;; (display (layout 10 3840 2160 3 0))
;; (newline)
;; (display (layout 11 3840 2160 3 0))
;; (newline)


