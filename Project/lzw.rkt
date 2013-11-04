
(require racket/mpair)
(require racket/list)
(require racket/file)
(require racket/runtime-path)

(define (compress input-path output-path method)
  (display-to-file (list->bytes (string-to-byte (method (string-append (file->string input-path) "##")))) output-path #:exists 'replace))

(define (decompress output-path input-path)
  (define fl (bytes->list (file->bytes input-path)))
  (display-to-file (decompress-lzw (string-append (foldr (λ (x y) (string-append (extend-bn (number->binary x) 8) y)) 
                                                         "" 
                                                         (drop (take fl (- (length fl) 1)) 1))
                                                  (extend-bn (number->binary 
                                                              (car (drop fl (- (length fl) 1)))) 
                                                             (car fl))))
                   output-path #:exists 'replace))

(define (extend-bn bn bits)
  (if (> bits (string-length bn)) (string-append (make-string (- bits (string-length bn)) #\0) bn) bn))

(define (string-to-byte st)
  (define (helper str l)
    (if (< (string-length str) 9) (append (list (string-length str)) l (list (binary->number (string->number str))))
      (helper (substring str 8) (append l (list (binary->number (string->number (substring str 0 8))))))))
  (helper st `()))

(define (binary->number bnr)
  (define (helper bn n)
    (if (= (quotient bn 10) 0) (* (expt 2 n) (remainder bn 10))
        (+ (* (expt 2 n) (remainder bn 10)) (helper (quotient bn 10) (+ n 1)))))
  (helper bnr 0))
    
(define (number->binary no)
  (cond 
    ((= no 0) "0")
    ((= no 1) "1")
    (else (string-append (number->binary (quotient no 2)) 
                         (number->string (remainder no 2))))))
(define (extendextra bn bits)
    (if (> bits (string-length bn)) (string-append bn (make-string (- bits (string-length bn)) #\0)) bn))
(define (dropextra bn)
  (define (helper l)
    (if (equal? (last l) #\0) (helper (take l (- (length l) 1)))
        (list->string l)))
    (let
      ([lst (string->list bn)])
      (helper lst)))
     

(define (table-producer l n)
  (if (= n 256) (append l (list (list (substring 
                                       (with-input-from-file "list_ascii.txt"
                                         (lambda () (read-string n))) 
                                       (- n 1)) 
                                      (extend-bn (number->binary (- n 1)) 9))))
      (table-producer (append l (list (list (substring 
                                       (with-input-from-file "list_ascii.txt" 
                                         (lambda () (read-string n))) 
                                       (- n 1))
                                            (extend-bn (number->binary (- n 1)) 9)))) (+ n 1))))

(define basic-table (table-producer `() 1))
  
(define (compress-lzw input)
  
  (define curr (current-time))
  (define bit-size 9)
  (define char *)
  (define str *)
  (define table `(("##" 000000000)))
  (define tablepos 256)
  (define percent 0)                                                  
  (define output "")
  (define total (string-length input))
  
  (define (helper in str char)
    
    (cond ((> (- (current-time) curr) dl) (begin 
                                              (set! percent (floor(* (- 1 (/ (string-length in) total)) 100.0)))
                                              (progress-bar percent)
                                              (set! curr (current-time) ))))
    
    (define (from-basic-table l dtable)
      (if (null? dtable) (from-basic-table l table)
          (if (string=? l (caar dtable)) (cadar dtable)
              (from-basic-table l (cdr dtable)))))
    
    (define (table-search l ctable)
      (if (null? ctable) 
          (begin
            (set! output (string-append output (extend-bn 
                                                (from-basic-table str basic-table)
                                                bit-size)))
            (set! table (append table (list (list (string-append str char)
                                                  (number->binary tablepos)))))
            (set! tablepos (+ tablepos 1))
            (if (= tablepos (expt 2 bit-size)) (set! bit-size (+ bit-size 1)) "")
            (if (= (string-length in) 1) output
                (helper (substring in 1) 
                        char 
                        (string (string-ref in 0)))))
          (if (string=? l (caar ctable))
              (begin
                (if (= (string-length in) 1) output
                    (helper (substring in 1) 
                            (string-append str char) 
                            (string (string-ref in 0))))) 
              (table-search l (cdr ctable)))))
    
    (table-search (string-append str char) table))
  
  (helper (substring input 2) (string (string-ref input 0)) (string (string-ref input 1))))

(define (decompress-lzw input)
  
  (define curr (current-time))
  (define bit-size 9)
  (define tablepos 256)
  (define table basic-table)
  (define output "")
  (define percent 0)
  (define total (string-length input))
  
  (define (helper in a b)
    
    (cond ((> (- (current-time) curr) dl) (begin 
                                              (set! percent (floor(* (- 1 (/ (string-length in) total)) 100.0)))
                                              (progress-bar percent)
                                              (set! curr (current-time) ))))
    
    (define (b-search j tbl)
      (if (null? tbl) (begin (display j) error)
          (if (string=? j (cadar tbl)) (caar tbl)
              (b-search j (cdr tbl)))))

    (define str-a (b-search a table))
    
    (define (t-search j tbl)
      (if (null? tbl) (string-append str-a (substring str-a 0 1))
          (if (string=? j (cadar tbl)) (caar tbl)
              (t-search j (cdr tbl)))))
    
    (define (add-to-table x)
      (set! table 
            (append table 
                    (list (list x (extend-bn (number->binary tablepos) bit-size)))))
      (set! tablepos (+ tablepos 1))
      (if (= (+ tablepos 1) (expt 2 bit-size)) (begin 
                                          (set! bit-size (+ bit-size 1))
                                          (set! table (map (λ (x) (list (car x) (extend-bn (cadr x) bit-size))) table)))
          ""))
    
    (begin
      (set! output (string-append output str-a))
      (add-to-table (string-append 
                     (t-search a table) 
                     (substring (t-search b table) 0 1)))
      (if (= (string-length in) 0)
          (string-append output (t-search b table))
          (helper (substring in bit-size) (extend-bn b bit-size) (extend-bn (substring in 0 bit-size) bit-size)))))
  
  (helper (substring input (* 2 9)) 
          (substring input 0 9) 
          (substring input 9 (* 9 2))))

(define (t-search j tbl)
  (if (null? tbl) ""
      (if (string=? j (cadar tbl)) (caar tbl)
          (t-search j (cdr tbl)))))

(define (compression-percentage input)
  (* (/ (string-length (compress-lzw input)) (* (- (string-length input) 1) 8)) 100.0))

(define (helper-lst n l)
  (if (= n 255) (append l (list n))
      (helper-lst (+ n 1) (append l (list n)))))

(define list-0-to-255 (helper-lst 0 `()))

  

