(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-packages)
(require 'ghcid)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(tooltip-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

(global-auto-revert-mode t)

(setq whitespace-style '(face
                         trailing
                         tabs
                         spaces
                         empty
                         indentation
                         space-after-tab
                         space-before-tab
                         space-mark
                         tab-mark))
(global-whitespace-mode)

(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 105)

(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
