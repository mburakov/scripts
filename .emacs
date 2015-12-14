(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(evil-mode)
(global-linum-mode 1)
(setq visible-bell 1)
(setq-default indent-tabs-mode nil)
(setq enable-local-variables :all)

(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-clang))

;;(defun my-irony-init ()
;;  (company-irony-setup-begin-commands)
;;  (irony-cdb-autosetup-compile-options)
;;  (setq-local irony--working-directory default-directory))

(defun google-c-style-setup ()
  (google-set-c-style)
  (google-make-newline-indent))

(defun my-c-init ()
  (google-c-style-setup)
;;(irony-mode)
  (setq-local truncate-lines 1)
  (setq compilation-read-command nil)
  (setq compile-command "make -j8")
  (local-set-key (kbd "<f2>") 'save)
  (local-set-key (kbd "<f7>") 'compile)
  (local-set-key (kbd "<f12>") 'vc-diff)
  (local-set-key (kbd "<tab>") 'company-complete))

(defun my-gdb-init ()
  (global-set-key (kbd "<f9>") 'gud-break)
  (global-set-key (kbd "<f10>") 'gud-next)
  (global-set-key (kbd "<f11>") 'gud-step))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hook 'my-c-init)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;;(add-hook 'irony-mode-hook 'my-irony-init)
(add-hook 'gud-mode-hook 'my-gdb-init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-trailing-lines t)
 '(frame-background-mode (quote dark))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(tool-bar-mode nil))

(load-theme 'solarized t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Ttyp0" :foundry "UW" :slant normal :weight normal :height 165 :width normal)))))
