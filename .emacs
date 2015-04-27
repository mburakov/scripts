(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(global-linum-mode 1)
(setq visible-bell 1)
(setq-default indent-tabs-mode nil)

(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable "~/.emacs.d/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))

(require 'google-c-style)
(defun google-c-style-setup ()
  (google-set-c-style)
  (google-make-newline-indent))

(defun my-c-init ()
  (ac-cc-mode-setup)
  (google-c-style-setup))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hool 'my-c-init)
(global-auto-complete-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(frame-background-mode (quote dark))
 '(initial-frame-alist (quote ((fullscreen . maximized)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Liberation Mono for Powerline" :foundry "unknown" :slant normal :weight normal :height 180 :width normal)))))

(load-theme 'solarized t)
