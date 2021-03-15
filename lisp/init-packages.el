(provide 'init-packages)


(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))


;; Standard ML
(straight-use-package 'sml-mode)
(add-to-list 'auto-mode-alist '("\\.\\(sml\\|sig\\)\\'" . sml-mode))


(straight-use-package 'general)
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-create-definer huyage/leader
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "SPC")


;; Counsel includes ivy and swiper as dependencies.
(straight-use-package 'counsel)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(general-define-key "C-s" 'swiper)
(general-define-key "C-x C-f" 'counsel-find-file)


(straight-use-package 'evil)
(setq evil-want-integration t)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(evil-mode 1)


(straight-use-package 'evil-collection)
(evil-collection-init)


(straight-use-package 'treemacs)
(add-hook 'treemacs-mode-hook (lambda () (display-line-numbers-mode 0)))


(straight-use-package 'treemacs-evil)
(require 'treemacs-evil)


(straight-use-package 'projectile)
(projectile-mode +1)
(huyage/leader
  "p"  '(projectile-command-map :which-key "projectile"))


(straight-use-package 'which-key)
(setq which-key-idle-delay 1)
(which-key-mode)


(straight-use-package 'vterm)
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))


(straight-use-package 'doom-themes)
(load-theme 'doom-gruvbox t)


(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)


(straight-use-package 'hydra)
(defhydra hydra-evil-window-resize (:timeout 4)
  "Resize window"
  ("h" evil-window-decrease-width  "Decrease width")
  ("j" evil-window-decrease-height "Decrease height")
  ("k" evil-window-increase-height "Increase height")
  ("l" evil-window-increase-width  "Increase width"))


(huyage/leader
  "t"  '(:ignore t          :which-key "Toggle")
  "tt" '(treemacs           :which-key "treemacs")
  "tv" '(vterm              :which-key "vterm")
  "w"  '(:ignore t          :which-key "Window")
  "wr" '(hydra-evil-window-resize/body :which-key "Resize"))


(straight-use-package 'lsp-mode)
(add-hook 'javascript-mode-hook #'lsp)


(straight-use-package 'lsp-ui)


(straight-use-package 'lsp-ivy)


(straight-use-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)


(straight-use-package 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)
