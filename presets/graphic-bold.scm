;; Example:
;; ┏━━━━━━━━━┰━━━━━━━━━┰━━━━━━━━━┓
;; ┃ a1      ┃ b1      ┃ c1      ┃
;; ┣━━━━━━━━━╋━━━━━━━━━╋━━━━━━━━━┫
;; ┃ a2      ┃ b2      ┃ c2      ┃
;; ┗━━━━━━━━━┻━━━━━━━━━┻━━━━━━━━━┛

;; ╔═════════╦═════════╦═════════╗
;; ║ a1      ║ b1      ║ c1      ║
;; ╠═════════╬═════════╬═════════╣
;; ┃ a2      ┃ b2      ┃ c2      ┃
;; ┗━━━━━━━━━┻━━━━━━━━━┻━━━━━━━━━┛

((name             . "graphic-bold")
 (description      . "Pseudo-graphic table with bold lines.")
 ;; Table border style.
 (border-top              . "━")
 (border-top-left         . "┏")
 (border-top-right        . "┓")
 (border-top-joint        . "┳")
 (border-left             . "┃")
 (border-left-joint       . "┣")
 (border-right            . "┃")
 (border-right-joint      . "┫")
 (border-bottom           . "━")
 (border-bottom-left      . "┗")
 (border-bottom-right     . "┛")
 (border-bottom-joint     . "┻")
 ;; Inner table lines style.
 (row-separator           . "━")
 (row-joint               . "╋")
 (column-separator        . "┃")
 ;; Header style.
 (header-top              . " ")
 (header-top-left         . " ")
 (header-top-right        . " ")
 (header-top-joint        . " ")
 (header-left             . " ")
 (header-right            . " ")
 (header-bottom           . "━")
 (header-bottom-left      . "┏")
 (header-bottom-right     . "┓")
 (header-bottom-joint     . "┳")
 (header-column-separator . " "))
