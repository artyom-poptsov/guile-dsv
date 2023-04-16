;; Example:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "graphic-with-shadow"
;; ┌─────────┬─────────┬─────────┐  
;; │ a       │ b       │ c       │  
;; ├─────────┼─────────┼─────────┤░░
;; │ a1      │ b1      │ c1      │░░
;; ├─────────┼─────────┼─────────┤░░
;; │ a2      │ b2      │ c2      │░░
;; └─────────┴─────────┴─────────┘░░
;;   ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░
;;
;; With a header:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "graphic-with-shadow" -H
;; ┏━━━━━━━━━┳━━━━━━━━━┳━━━━━━━━━┓  
;; ┃ a       ┃ b       ┃ c       ┃░░
;; ┡━━━━━━━━━╇━━━━━━━━━╇━━━━━━━━━┩░░
;; │ a1      │ b1      │ c1      │░░
;; ├─────────┼─────────┼─────────┤░░
;; │ a2      │ b2      │ c2      │░░
;; └─────────┴─────────┴─────────┘░░
;;   ░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░░

((name                    . "graphic-with-shadow")
 (description             . "Pseudo-graphic table with a fancy shadow.")
 ;; Table border style.
 (border-top              . "─")
 (border-top-left         . "┌")
 (border-top-right        . "┐")
 (border-top-joint        . "┬")
 (border-left             . "│")
 (border-left-joint       . "├")
 (border-right            . "│")
 (border-right-joint      . "┤")
 ;; Shadow.
 (shadow                     . "░")
 (shadow-offset              . "2,1")   ; x,y
 ;; Inner table lines style.
 (row-separator           . "─")
 (row-joint               . "┼")
 (column-separator        . "│")
 ;; Header style.
 (border-bottom           . "─")
 (border-bottom-left      . "└")
 (border-bottom-right     . "┘")
 (border-bottom-joint     . "┴")
 (header-top              . "━")
 (header-top-left         . "┏")
 (header-top-right        . "┓")
 (header-top-joint        . "┳")
 (header-left             . "┃")
 (header-right            . "┃")
 (header-bottom           . "━")
 (header-bottom-left      . "┡")
 (header-bottom-right     . "┩")
 (header-bottom-joint     . "╇")
 (header-column-separator . "┃"))
