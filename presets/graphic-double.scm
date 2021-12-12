;; Example:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "graphic-double"
;; ╔═════════╦═════════╦═════════╗
;; ║ a       ║ b       ║ c       ║
;; ╠═════════╬═════════╬═════════╣
;; ║ a1      ║ b1      ║ c1      ║
;; ╠═════════╬═════════╬═════════╣
;; ║ a2      ║ b2      ║ c2      ║
;; ╚═════════╩═════════╩═════════╝
;;
;; With a header:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "graphic-double" -H
;;
;;   a         b         c
;; ╔═════════╦═════════╦═════════╗
;; ║ a1      ║ b1      ║ c1      ║
;; ╠═════════╬═════════╬═════════╣
;; ║ a2      ║ b2      ║ c2      ║
;; ╚═════════╩═════════╩═════════╝

((name                    . "graphic-double")
 (description             . "Pseudo-graphic table with double lines.")
 (border-top              . "═")
 (border-top-left         . "╔")
 (border-top-right        . "╗")
 (border-top-joint        . "╦")
 (border-left             . "║")
 (border-left-joint       . "╠")
 (border-right            . "║")
 (border-right-joint      . "╣")
 (border-bottom           . "═")
 (border-bottom-left      . "╚")
 (border-bottom-right     . "╝")
 (border-bottom-joint     . "╩")
 (row-separator           . "═")
 (row-joint               . "╬")
 (column-separator        . "║")
 ;; Header style.
 (header-top              . " ")
 (header-top-left         . " ")
 (header-top-right        . " ")
 (header-top-joint        . " ")
 (header-left             . " ")
 (header-right            . " ")
 (header-bottom           . "═")
 (header-bottom-left      . "╔")
 (header-bottom-right     . "╗")
 (header-bottom-joint     . "╦")
 (header-column-separator . " "))
