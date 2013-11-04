
(define shano-tree (leaf #\#))

(define (shano path)
  (define cnt 0)
  (define crr (current-time))
  (set! str (file->string path))
  (define len (file-size path))
  (define (init count)
    (cond ((< count 256) (begin (vector-set! vec count (cons (integer->char count) 0))
                                (init (+ count 1))))))
  (define (build-freq s)
    (cond ((not (string=? s ""))
           (begin (define n (char->integer (string-ref s 0)))
                  (set! cnt (+ cnt 1))
                  (cond ((> (- (current-time) crr) dl1)
                         (begin (set! crr (current-time))
                                (progress-bar (floor (* 20.0 (/ cnt len)))))))
                  (vector-set! vec n (cons (string-ref s 0) (+ 1 (cdr (vector-ref vec n)))))
                  (build-freq (substring s 1))))))
  
  (define (my-sort l1 l2)
    (> (cdr l1) (cdr l2)))
  
  (define (clear-lst l)
    (foldr (lambda (x y) (if (= 0 (cdr x)) y (append (list x) y))) `() l))
  
  (define (convert-initial-list lst)
    (if (null? lst) `() (append (list (cons (leaf (caar lst)) (cdar lst))) (convert-initial-list (cdr lst)))))
  
  (define (singleton? l)
  (if (null? l) #f (if (null? (cdr l)) #t #f)))
  
  (define (until f p l)
    (if (p l) l (until f p (f l))))
  
  (define (tree-shano lst)
   (define (sum l) (foldr (lambda (x y) (+ (cdr x) y)) 0 l ))
   (define (sum-till i l) (foldr (lambda (x y) (+ (cdr x) y)) 0 (take l i))) 
   (define (break-point l)
     (define total (sum l))
     (define difference total)
     (define (helper i)
       (define currentsum (sum-till i l))
       (define currentdiff (abs (- (* 2 currentsum) total)))
       (if (< currentdiff difference) (begin (set! difference currentdiff) (helper (+ i 1))) (- i 1) ))
     (helper 1))
  
  (bnode (if (singleton? (take lst (break-point lst))) (caar(take lst (break-point lst))) (tree-shano (take lst (break-point lst))))   
         (if (singleton? (drop lst (break-point lst))) (caar(drop lst (break-point lst))) (tree-shano (drop lst (break-point lst))))))
  
 
  (define (encode tr strng)
    (define count 0)
    (define curr (current-time))
    (define (transform tree)
      (define (helper t str)
        (cond ((leaf? t) (list (cons (leaf-val t) str)))
              (else (append (helper (bnode-ltree t) (string-append str "0")) (helper (bnode-rtree t) (string-append str "1"))))))
      (helper tree ""))
    (define (new-transform lst)
      (cond ((not (null? lst)) (begin 
                                 (vector-set! arr (char->integer (caar lst)) (cdar lst))
                                 (new-transform (cdr lst))))))
    
    (define (encode-h str result)
      (cond ((not (string=? str "")) (begin (set! count (+ count 1))
                                            (cond ((> (- (current-time) curr) dl1)
                                                   (begin (set! curr (current-time))
                                                          (progress-bar (floor (+ 20.0 (* 70.0 (/ count len))))))))
                                            (encode-h (substring str 1) (string-append result (vector-ref arr (char->integer (string-ref str 0)))))))
            (else result))) 
    (begin (new-transform (transform tr))
           (encode-h strng "")))
  
  (begin 
    (init 0)
    (build-freq str)
    (vector-sort! my-sort vec)
    (set! shano-tree (tree-shano (convert-initial-list (clear-lst (vector->list vec)))))
    (set! bit-str (encode shano-tree str))
    (set! tree-str (encode-tree shano-tree))))

(define (compress-shano input-path output-path)
  (begin
    (init-huff)
    (shano input-path)
    (set! bstr-tree (foldr (lambda (x y) (string-append x y)) "" (map (lambda (x) (extend-bn (number->binary (char->integer x)) 8)) (string->list (string-append (number->string (string-length tree-str)) tree-str)))))
    (display-to-file (list->bytes (string-to-byte (string-append bstr-tree bit-str))) output-path #:exists 'append)))