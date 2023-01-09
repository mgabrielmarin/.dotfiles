(deftheme default-black)

(custom-theme-set-faces
 'default-black
 '(default ((t (:inherit nil :stipple nil :background "Black" :foreground "White" :inverse-video nil :box nil :strike-t*hrough nil :overline nil :underline nil :slant normal :weight normal :width normal :height 105))))
 '(highlight ((((class color) (min-colors 88) (background dark)) (:background "#111111"))))
 '(region ((nil (:background "#464740"))))
 '(hl-line ((nil (:background "#222222"))))
 '(yas-field-highlight-face ((nil (:background "#333399"))))
  '(js2-function-param-face ((t (:foreground "LightGoldernrod"f))))
 '(font-lock-warning-face ((nil (:foreground "#ff6666"))))
  '(show-paren-match ((nil (:background "#333399"))))
 '(show-paren-mismatch ((((class color)) (:background "red"))))
 '(font-lock-comment-face ((t (:foreground "DarkGreen"f))))
 '(font-lock-string-face ((t (:foreground "DarkGreen"f)))))


(provide-theme 'default-black)
