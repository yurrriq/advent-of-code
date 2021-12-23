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

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-ensure t)

(use-package company)

(use-package crux
  :config
  (global-set-key (kbd "C-a") 'crux-move-beginning-of-line))

(use-package direnv
  :config
  (direnv-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package fill-column-indicator
  :config
  (setq-default fill-column 80)
  (global-display-fill-column-indicator-mode))

(use-package gap-mode)

(use-package haskell-mode)

(use-package hl-todo
  :demand
  :config (global-hl-todo-mode t))

(use-package lsp-mode
  :hook ((haskell-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config
  (advice-add 'lsp :before #'direnv-update-environment)
  (setq lsp-modeline-code-actions-enable nil))

(use-package lsp-ui
  :hook (haskell-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-haskell)

(use-package multiple-cursors
  :demand
  :config (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines))

(use-package nix-mode)

(let ((noweb-load-path
        (file-name-as-directory
          (expand-file-name "site-lisp"
            (expand-file-name "emacs"
              (expand-file-name "share"
                (file-name-directory
                  (directory-file-name
                    (file-name-directory
                      (executable-find "noweb"))))))))))
  (use-package noweb-mode
    :load-path load-path
    :mode ("\\.nw\\'")
    :demand))

(use-package nyan-mode
  :demand
  :config (nyan-mode 1))

(use-package paredit)

(use-package smex
  :demand
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode t))

(use-package yaml-mode)
