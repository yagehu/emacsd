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
(general-create-definer huyage/leader
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "SPC")


(straight-use-package 'ivy)
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)


(straight-use-package 'evil)
(setq evil-want-keybinding nil)
(setq evil-want-C-u-scroll t)
(evil-mode 1)


(straight-use-package 'evil-collection)
(evil-collection-init)


(straight-use-package 'which-key)
(setq which-key-idle-delay 1)
(which-key-mode)


(straight-use-package 'vterm)
(add-hook vterm-mode-hook (lambda () (display-line-numbers-mode 0)))


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
  "t"  '(:ignore t :which-key "Toggle")
  "tt" '(vterm     :which-key "vterm")
  "w"  '(:ignore t :which-key "Window")
  "wr" '(hydra-evil-window-resize/body :which-key "Resize"))
