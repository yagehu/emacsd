(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-packages)

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

(global-auto-revert-mode t)

;; Show all whitespace.
(global-whitespace-mode)

(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 105)
