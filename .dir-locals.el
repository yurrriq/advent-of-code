;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil
  (mode . direnv)
  (indent-tabs-mode . nil)
  (fill-column . 80)
  (eval . (turn-on-fci-mode)))
 (gap-mode
  (gap-indent-step . 2)
  (gap-start-options "-E" "-f" "-b" "-m" "2m")
  (gap-executable . "gap"))
 (haskell-mode
  (mode . interactive-haskell))
 (idris-mode
  (idris-interpreter-flags . ("-X" "ElabReflection"
                              "-p" "contrib"
                              "-p" "effects"
                              "-p" "lightyear")))
 (makefile-mode
  . ((indent-tabs-mode . t)))
 ("_src"
  (nil
   (eval progn
         (global-emojify-mode 0)
         (global-emojify-mode-line-mode 0)))))
