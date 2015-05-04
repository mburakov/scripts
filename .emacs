(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(tool-bar-mode 0)
(global-linum-mode 1)
(setq visible-bell 1)
(setq-default indent-tabs-mode nil)
(setq-default truncate-lines 1)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

(defun google-c-style-setup ()
  (google-set-c-style)
  (google-make-newline-indent))

(defun my-c-init ()
  (google-c-style-setup))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hook 'my-c-init)

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
 '(default ((t (:family "Liberation Mono for Powerline" :foundry "unknown" :slant normal :weight normal :height 120 :width normal)))))

(load-theme 'solarized t)
