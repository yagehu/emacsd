(add-to-list 'load-path "~/.emacs.d/lisp")

(require 'init-packages)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(initial-frame-alist '((fullscreen . maximized)))
 '(safe-local-variable-values
   '((eval with-eval-after-load 'lsp-mode
           (add-to-list 'lsp-file-watch-ignored-directories "bazel-.+\\'")
           (add-to-list 'lsp-file-watch-ignored-directories "\\.git\\'")))))

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

(setq-default indent-tabs-mode nil)

(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs-saves")))

(set-face-attribute 'default nil :font "JetBrains Mono" :height 95)

(setq-default display-fill-column-indicator-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; Restart Emacs.
(defun launch-separate-emacs-in-terminal ()
  (suspend-emacs "fg ; emacs -nw"))

(defun launch-separate-emacs-under-x ()
  (call-process "sh" nil nil nil "-c" "emacs &"))

(defun restart-emacs ()
  (interactive)
  ;; We need the new emacs to be spawned after all kill-emacs-hooks
  ;; have been processed and there is nothing interesting left
  (let ((kill-emacs-hook (append kill-emacs-hook (list (if (display-graphic-p)
                                                           #'launch-separate-emacs-under-x
                                                         #'launch-separate-emacs-in-terminal)))))
    (save-buffers-kill-emacs)))
