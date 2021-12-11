;; Example:
;; .---------.
;; |    |    |
;; |----+----|
;; |    |    |
;; |----+----|
;; |    |    |
;; '---------'
;;
;; With a header:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv -b "ascii" -H
;; .-----------------------------.
;; | a       | b       | c       |
;; |=========+=========+=========|
;; | a1      | b1      | c1      |
;; |---------+---------+---------|
;; | a2      | b2      | c2      |
;; '-----------------------------'

((name                . "ascii")
 (description         . "Plain ASCII table.")
 (border-top          . "-")
 (border-top-left     . ".")
 (border-top-right    . ".")
 (border-top-joint    . "-")
 (border-left         . "|")
 (border-left-joint   . "|")
 (border-right        . "|")
 (border-right-joint  . "|")
 (row-separator       . "-")
 (row-joint           . "+")
 (column-separator    . "|")
 (border-bottom       . "-")
 (border-bottom-left  . "'")
 (border-bottom-right . "'")
 (border-bottom-joint . "-")
 ;; Header style.
 (header-top              . "-")
 (header-top-left         . ".")
 (header-top-right        . ".")
 (header-top-joint        . "-")
 (header-left             . "|")
 (header-right            . "|")
 (header-bottom           . "=")
 (header-bottom-left      . "|")
 (header-bottom-right     . "|")
 (header-bottom-joint     . "+")
 (header-column-separator . "|"))
