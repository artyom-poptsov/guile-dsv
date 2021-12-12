;; Example:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "org"
;; | a       | b       | c       |
;; | a1      | b1      | c1      |
;; | a2      | b2      | c2      |
;;
;; With a header:
;; $ echo -e "a,b,c\na1,b1,c1\na2,b2,c2\n" | dsv  -b "org" -H
;; | a       | b       | c       |
;; |---------+---------+---------|
;; | a1      | b1      | c1      |
;; | a2      | b2      | c2      |

((name                    . "org")
 (description             . "Org-mode tables.")
 ;; Table border style.
 (border-left             . "|")
 (border-right            . "|")
 ;; Table header style.
 (header-left             . "|")
 (header-right            . "|")
 (header-bottom           . "-")
 (header-bottom-left      . "|")
 (header-bottom-right     . "|")
 (header-bottom-joint     . "+")
 (header-column-separator . "|")
 ;; Inner table lines style.
 (column-separator        . "|"))
