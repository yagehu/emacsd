(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)


(straight-use-package 'smooth-scrolling)
(smooth-scrolling-mode 1)


(straight-use-package 'proof-general)


(straight-use-package 'general)
(general-define-key "<escape>" 'keyboard-escape-quit)
(general-create-definer custom/leader
  :keymaps '(normal visual emacs)
  :prefix "SPC"
  :global-prefix "SPC")


(straight-use-package 'counsel)
(counsel-mode)
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(general-define-key "C-s" 'swiper)
(general-define-key "C-x C-f" 'counsel-find-file)


(straight-use-package 'company)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") 'company-complete-selection))
(add-hook 'after-init-hook 'global-company-mode)


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


(straight-use-package 'magit)


(straight-use-package 'projectile)
(projectile-mode +1)
(custom/leader
  "p" '(projectile-command-map :which-key "projectile"))


(straight-use-package 'which-key)
(setq which-key-idle-delay 1)
(which-key-mode)


(straight-use-package 'vterm)
(setq-default vterm-shell "/usr/bin/zsh")
(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))


(straight-use-package 'doom-themes)
(load-theme 'doom-gruvbox t)


(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)


(straight-use-package 'hydra)
(defhydra hydra-text-scale (:timeout 4)
  "Text Scale"
  ("k" text-scale-increase "Up")
  ("j" text-scale-decrease "Down")
  ("r" (text-scale-set 0) "Reset")
  ("0" (text-scale-set 0) :bind nil :exit t))
(defhydra hydra-evil-window-resize (:timeout 4)
  "Resize window"
  ("h" evil-window-decrease-width  "Decrease width")
  ("j" evil-window-decrease-height "Decrease height")
  ("k" evil-window-increase-height "Increase height")
  ("l" evil-window-increase-width  "Increase width"))


(straight-use-package 'fzf)
(custom/leader
  "z"  '(:ignore t      :which-key "Fzf")
  "zp" '(fzf-projectile :which-key "Projectile"))


(straight-use-package 'lsp-mode)
(add-hook 'javascript-mode-hook #'lsp)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(setq lsp-eldoc-render-all t)
(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :workspace))


;; JavaScript/TypeScript
(straight-use-package 'typescript-mode)
(add-hook 'typescript-mode-hook #'lsp)

;; YAML
(straight-use-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(setq lsp-yaml-schema-store-uri "https://www.schemastore.org/api/json/catalog.json")
(setq lsp-yaml-schemas
      '(:github-workflow ".github/workflows/**.yaml"))
(add-hook 'yaml-mode-hook #'lsp)


;; Rust
(straight-use-package 'rustic)
(setq rustic-format-display-method 'ignore)
(setq rustic-format-on-save t)
(setq rustic-lsp-server 'rust-analyzer)
(setq rustic-rustfmt-args "+nightly")
(setq lsp-rust-analyzer-cargo-watch-command "clippy")
(setq lsp-rust-analyzer-display-chaining-hints nil)
(setq lsp-rust-analyzer-server-display-inlay-hints t)
(setq lsp-rust-analyzer-proc-macro-enable t)


(straight-use-package 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)


(straight-use-package 'lsp-ivy)


(straight-use-package 'lsp-treemacs)
(lsp-treemacs-sync-mode 1)


(straight-use-package 'lsp-haskell)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)


(straight-use-package 'flycheck)
(global-flycheck-mode)


(straight-use-package 'exec-path-from-shell)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(setq org-ellipsis "⤵")
(setq org-roam-v2-ack t)
(straight-use-package 'org-roam)
(setq org-roam-directory (file-truename "~/sync/org"))
(org-roam-db-autosync-mode)
(add-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
(setq
  org-edit-src-content-indentation 0
  org-src-tab-acts-natively t
  org-src-preserve-indentation t)


(straight-use-package 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


(custom/leader
  "o"  '(:ignore t                     :which-key "org-roam")
  "of" '(org-roam-node-find            :which-key "Find node")
  "t"  '(:ignore t                     :which-key "Toggle")
  "ts" '(hydra-text-scale/body         :which-key "Text scale")
  "tt" '(treemacs                      :which-key "treemacs")
  "tv" '(vterm                         :which-key "vterm")
  "w"  '(:ignore t                     :which-key "Window")
  "wr" '(hydra-evil-window-resize/body :which-key "Resize"))


(provide 'init-packages)
