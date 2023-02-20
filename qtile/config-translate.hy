;; Python libs
(import libqtile [bar layout widget extension])
(import libqtile.config [Click Drag Group Key Match Screen])
(import libqtile.lazy [lazy])
(import libqtile.utils [guess_terminal])

;; Hy libs
(import hy.pyops *)

;; Variables
(setv mod "mod1")
(setv terminal (guess_terminal))
(setv
  keys
  ())

;; Example
(defn add-key [modifier key function]
  (Key [modifier] key (function)))
