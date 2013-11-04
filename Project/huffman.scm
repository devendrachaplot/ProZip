
(require rnrs/sorting-6)
(require mzlib/cml)

(struct bnode (ltree rtree) #:transparent)
(struct leaf (val) #:transparent)

(define ex 16)
(define dl1 500)

(define vec (make-vector (expt 2 ex) (cons 0 0)))
(define arr (make-vector (expt 2 ex) ""))
(define huff-tree (leaf #\#))
(define bit-str "")
(define tree-str "")
(define str "")
(define bstr-tree "")

(define (init-huff)
  (begin (set! ex 16)
         (set! dl1 500)
         (set! vec (make-vector (expt 2 ex) (cons 0 0)))
         (set! arr (make-vector (expt 2 ex) ""))
         (set! huff-tree (leaf #\#))
         (define shano-tree (leaf #\#))
         (set! bit-str "")
         (set! tree-str "")
         (set! str "")
         (set! bstr-tree "")))


(define (encode-tree tr)
  (cond ((leaf? tr) (string-append "(L" (string (leaf-val tr)) ")"))
        (else (string-append "(B" (encode-tree (bnode-ltree tr)) (encode-tree (bnode-rtree tr)) ")"))))


(define (huffman path)
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
    (< (cdr l1) (cdr l2)))
  
  (define (clear-lst l)
    (foldr (lambda (x y) (if (= 0 (cdr x)) y (append (list x) y))) `() l))
  
  (define (convert-initial-list lst)
    (if (null? lst) `() (append (list (cons (leaf (caar lst)) (cdar lst))) (convert-initial-list (cdr lst)))))
  
  (define (combine pair1 pair2)
    (cons (bnode (car pair1) (car pair2)) (+ (cdr pair1) (cdr pair2))))
  
  (define (insert pair lst)
    (cond ((null? lst) (list pair))
          ((< (cdar lst) (cdr pair)) (append (list (car lst)) (insert pair (cdr lst))))
          (else (append (list pair) lst))))
  
  (define (combine-and-insert lst)
    (insert (combine (cons (caar lst) (cdar lst)) (cons (caadr lst) (cdadr lst))) (cddr lst)))
  
  (define (singleton lst)
    (= (length lst) 1))
  
  (define (until f p l)
    (if (p l) l (until f p (f l))))
  
  (define (tree-huffman l)
    (caar (until (lambda (x) (combine-and-insert x)) singleton (convert-initial-list l))))
  
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
    (set! huff-tree (tree-huffman (clear-lst (vector->list vec))))
    (set! bit-str (encode huff-tree str))
    (set! tree-str (encode-tree huff-tree))
    ))

(define (compress-huffman input-path output-path)
  (begin
    (init-huff)
    (huffman input-path)
    (set! bstr-tree (foldr (lambda (x y) (string-append x y)) "" (map (lambda (x) (extend-bn (number->binary (char->integer x)) 8)) (string->list (string-append (number->string (string-length tree-str)) tree-str)))))
    (display-to-file (list->bytes (string-to-byte (string-append bstr-tree bit-str))) output-path #:exists 'replace)
    )
  )