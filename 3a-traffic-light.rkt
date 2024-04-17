(require 2htdp/universe)
(require 2htdp/image)
(require racket/gui/base)
(require rebellion/type/enum)

(define-enum-type traffic-light-color (red yellow green))
(define (get-draw-color-from-traffic-color color)
    (match color
        [(== red) "red"]
        [(== yellow) "Medium Yellow"]
        [(== green) "green"]))

(define LIGHT_RADIUS 40)

(define (draw-light color is_active)
    (circle LIGHT_RADIUS (if is_active "solid" "outline") (get-draw-color-from-traffic-color color)))

(define (draw-traffic-light color-current)
    (above 
        (draw-light red (eq? color-current red))
        (draw-light yellow (eq? color-current yellow))
        (draw-light green (eq? color-current green))))

(define (get-next-tariff-color color-current)
    (match color-current
        [(== red) yellow]
        [(== yellow) green]
        [(== green) red]))

(big-bang red 
          (on-tick get-next-tariff-color 1)
          (to-draw draw-traffic-light 500 500))
