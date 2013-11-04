
(define start-width 265) 
(define start-height 150)
(define start-x 500)
(define start-y 300)
(define screen-x 10)
(define screen-y 10)
(define d #f)

(define opening-frame (new frame% [label "ProZip"] [style 'no-caption] [width start-width] [height start-height] [x start-x] [y start-y]))
(define opening-canvas 
  (new canvas% [parent opening-frame]
       [paint-callback (lambda (opening-canvas opening-dc)
                         (send opening-dc draw-bitmap screen-image screen-x screen-y)
                         )
       ]
  )
)

(define opening-dc (send opening-canvas get-dc))
(define screen-image (make-object bitmap% 204 204))
(set! d (send screen-image load-file "Splash.jpg" `jpeg))

