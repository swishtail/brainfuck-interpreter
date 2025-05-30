#! /usr/local/bin/racket
#lang racket/base

(define (file->ins port)
  (let next-char ((current-char (read-char port)))
    (cond ((eof-object? current-char) '())
          ((char->ins current-char)
           => (lambda (ins)
                (cons ins (next-char (read-char port)))))
          (else (next-char (read-char port))))))

(define (char->ins char)
  (cond ((char=? char #\>) tape-right)
        ((char=? char #\<) tape-left)
        ((char=? char #\+) inc-cell)
        ((char=? char #\-) dec-cell)
        ((char=? char #\.) tape-print)
        ((char=? char #\,) tape-accept)
        ((char=? char #\[) 'jmp-fwd)
        ((char=? char #\]) 'jmp-bwd)
        (else #f)))

(define (list->zipper x)
  (list '() (car x) (cdr x)))

(define (zeros n)
  (if (zero? n)
      '()
      (cons 0 (zeros (- n 1)))))

(define (tape-read t) (cadr t))
(define (tape-write t s) (list (car t) s (caddr t)))

(define (tape-left t) (list (cdar t) (caar t) (cons (cadr t) (caddr t))))
(define (tape-right t) (list (cons (cadr t) (car t)) (caaddr t) (cdaddr t)))

(define (inc-cell t)
  (list (car t) (modulo (+ (cadr t) 1) 256) (caddr t)))

(define (dec-cell t)
  (list (car t) (modulo (- (cadr t) 1) 256) (caddr t)))

(define (tape-print t)
  (begin (display (integer->char (tape-read t))) t))

(define (tape-accept t)
  (let ((ip-char (read-char)))
    (if (eof-object? ip-char)
        t
        (tape-write t (char->integer ip-char)))))

(define (make-jmp-alist ins)
  (let jmp-alist-iter ((alist '())
                       (stack '())
                       (ic 0)
                       (rem-ins ins))
    (cond ((null? rem-ins) alist)
          ((eq? (car rem-ins) 'jmp-fwd)
           (jmp-alist-iter alist
                           (cons ic stack)
                           (+ ic 1)
                           (cdr rem-ins)))
          ((eq? (car rem-ins) 'jmp-bwd)
           (jmp-alist-iter (cons (list (car stack) ic) alist)
                           (cdr stack)
                           (+ ic 1)
                           (cdr rem-ins)))
          (else
           (jmp-alist-iter alist
                           stack
                           (+ ic 1)
                           (cdr rem-ins))))))

(define (fwd-jmp-dest ic alist)
  (+ 1 (cadr (assoc ic alist))))

(define (bwd-jmp-dest ic alist)
  (+ 1 (cadr (assoc ic
                    (map (lambda (x)
                           (list (cadr x) (car x)))
                         alist)))))

(define (run-machine data ins)
  (let ((jmp-alist (make-jmp-alist ins)))
    (let machine-iter ((current-data data)
                       (ic 0)
                       (rem-ins ins))
      (if (null? rem-ins)
          (begin (newline)
                 (display "halt")
                 (newline))
          (let ((current-ins (car rem-ins)))
            (cond ((eq? current-ins 'jmp-fwd)
                   (if (zero? (tape-read current-data))
                       (let ((jmp-dest (fwd-jmp-dest ic jmp-alist)))
                         (machine-iter current-data
                                       jmp-dest
                                       (list-tail ins jmp-dest)))
                       (machine-iter current-data
                                     (+ ic 1)
                                     (cdr rem-ins))))
                  ((eq? current-ins 'jmp-bwd)
                   (if (not (zero? (tape-read current-data)))
                       (let ((jmp-dest (bwd-jmp-dest ic jmp-alist)))
                         (machine-iter current-data
                                       jmp-dest
                                       (list-tail ins jmp-dest)))
                       (machine-iter current-data
                                     (+ ic 1)
                                     (cdr rem-ins))))
                  (else
                   (machine-iter (current-ins current-data)
                                 (+ ic 1)
                                 (cdr rem-ins)))))))))

(run-machine (list->zipper (zeros 30000))
             (call-with-input-file
               (vector-ref (current-command-line-arguments) 0) file->ins))
