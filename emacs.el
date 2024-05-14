(column-number-mode 1)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(tool-bar-mode 0)

(require 'package)
(setq-default
  frames-only-mode t
  indent-tabs-mode nil
  inhibit-splash-screen t
  package-archives nil
  package-enable-at-startup nil)
(package-initialize)

(load-theme 'wombat)

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "s-u") 'revert-buffer)

(set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 110)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package company
  :custom
  (company-idle-begin 0.5)
  :bind
  (:map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-<" . company-select-first)
    ("M->" . company-select-last)))

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))

(use-package direnv
  :config
  (direnv-mode))

(use-package dune)

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(use-package flycheck)

;; FIXME
;; (use-package gap-mode)

(use-package haskell-mode
  :hook (haskell-mode . interactive-haskell-mode))

(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))

(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->" "->-" ">-" ">>-"
     "=<<" "=<" "=<=" "<==" "<<=" "<=" "=>" "=>>" "==>" "===>" "=>=" ">=" ">>="
     "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::" ":::" "__"
     "<~~" "</" "</>" "/>" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!==" "!===" "=/=" "=!="
     "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>" "+*" "=*" "=:" ":>"
     "(*" "*)" "/*" "*/" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-" "-|" "<!--"
     "<!---"))
  (global-ligature-mode t))

(use-package lsp-mode
  :hook ((haskell-mode . lsp-deferred)
         (tuareg-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (advice-add 'lsp :before #'direnv-update-environment)
  (setq lsp-modeline-code-actions-enable nil))

(use-package lsp-ui
  :hook ((haskell-mode . lsp-ui-mode)
         (tuareg-mode . lsp-ui-mode))
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-haskell)

(use-package magit
  :demand
  :bind
  (("C-c g" . magit-file-dispatch)
   ("C-x g" . magit-status)
   ("C-x C-g" . magit-status)))

(use-package multiple-cursors
  :demand
  :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nix-mode)

(eval-and-compile
  ;;; https://www.emacswiki.org/emacs/ThreadMacroFromClojure

  ;; (defmacro ->> (&rest body)
  ;;   (let ((result (pop body)))
  ;;     (dolist (form body result)
  ;;       (setq result (append form (list result))))))

  (defmacro -> (&rest body)
    (let ((result (pop body)))
      (dolist (form body result)
        (setq result (append (list (car form) result) (cdr form))))))

  (defun yurrriq/noweb-load-path ()
    (-> (executable-find "noweb")
      (file-name-directory)
      (directory-file-name)
      (file-name-directory)
      (file-name-concat "share" "emacs" "site-lisp")
      (file-name-as-directory))))

(use-package noweb-mode
  :load-path (lambda () (list (yurrriq/noweb-load-path)))
  :mode ("\\.nw\\'")
  :demand)

(use-package nyan-mode
  :demand
  :config (nyan-mode 1))

(use-package ocamlformat
  :init
  (add-hook 'before-save-hook 'ocamlformat-before-save)
  :config
  (setq ocamlformat-enable 'enable-outside-detected-project))

(use-package ormolu
  :hook (haskell-mode . ormolu-format-on-save-mode)
  :bind
  (:map haskell-mode-map
        ("C-c r" . ormolu-format-buffer))
  :config
  (setq ormolu-extra-args '("--ghc-opt" "-XTemplateHaskell")))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smex
  :demand
  :bind
  (("M-x" . smex)
   ("M-X" . smex-major-mode-commands)
   ("C-c C-c M-x" . execute-extended-command)))

(use-package tuareg
  :mode (("\\.ml[ily]?$" . tuareg-mode)
         ("\\.topml$" . tuareg-mode)))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package yaml-mode)
