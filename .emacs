(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'rtags)
(require 'company)
(require 'evil)
(require 'google-c-style)

(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'solarized t)

(evil-mode)
(setq rtags-autostart-diagnostics t)
(rtags-diagnostics)
(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(global-company-mode)
(global-linum-mode 1)
(setq visible-bell 1)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist `(("." . "~/.saves")))
(setq backup-by-copying t)
(setq ediff-split-window-function 'split-window-horizontally)
(set-default-font "Monospace-11:weight=bold")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setenv "CINEMO_BUILDMACHINE_VALID" "yes")
(setenv "MAKE_OPTS" "-j16")
(setenv "APP_ALLOW_MISSING_DEPS" "true")

(defun my-c-init ()
  (define-key evil-normal-state-map (kbd "C-]") 'rtags-find-symbol-at-point)
  (define-key evil-normal-state-map (kbd "<f12>") 'vc-ediff)
  (google-set-c-style)
  (google-make-newline-indent)
  (electric-pair-local-mode))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hook 'my-c-init)

(defun make-header ()
  (interactive)
  (search-forward "(")
  (backward-char)
  (push-mark)
  (search-backward " ")
  (forward-char)
  (copy-region-as-kill t t t)
  (beginning-of-line)
  (insert ?/ (make-string 79 ?*))
  (newline)
  (insert (make-string 4 ?\s))
  (yank)
  (newline)
  (insert ?\s (make-string 78 ?*) ?/)
  (newline 2))

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun plantuml ()
  (interactive)
  (shell-command-on-region
   (point-min)
   (point-max)
   "plantuml -p | feh -")
  (deactivate-mark))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-trailing-lines t)
 '(evil-shift-width 2)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (google-c-style evil company)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
