;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((gap-mode
   (gap-indent-step . 2)
   (gap-start-options "-E" "-f" "-b" "-m" "2m")
   (gap-executable . "gap"))
  (haskell-mode
    . ((eval
         . (progn
             ;; (call-interactively 'lsp-workspace-folders-add)
             (interactive-haskell-mode))))))
