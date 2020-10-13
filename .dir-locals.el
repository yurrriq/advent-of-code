;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (mode . direnv))
 (gap-mode
  (gap-indent-step . 2)
  (gap-start-options "-E" "-f" "-b" "-m" "2m")
  (gap-executable . "gap"))
 (haskell-mode
  (mode . interactive-haskell))
 ("_src"
  (nil
   (eval progn
         (global-emojify-mode 0)
         (global-emojify-mode-line-mode 0)))))
