(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'rtags)
(require 'company)
(require 'evil)
(require 'google-c-style)
(require 'color-theme)

(evil-mode)
(color-theme-solarized-dark)
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
(set-default-font "DejaVu Sans Mono-11:weight=bold")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-c-init ()
  (define-key evil-normal-state-map (kbd "C-]") 'rtags-find-symbol-at-point)
  (define-key evil-normal-state-map (kbd "<f12>") 'vc-ediff)
  (google-set-c-style)
  (google-make-newline-indent)
  (electric-pair-local-mode))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hook 'my-c-init)

(defun gnome-on-wayland ()
  "Works around https://bugzilla.gnome.org/show_bug.cgi?id=736660"
  (interactive)
  (setenv "NDK_TOOLCHAIN_VERSION" "4.9")
  (setenv "ANDROID_HOME" "/opt/android-sdk")
  (setenv "ANDROID_NDK" "/opt/android-ndk")
  (setenv "PATH" (concat (getenv "PATH") ":"
                         "/opt/android-ndk" ":"
                         "/opt/android-sdk/platform-tools")))

(defun sudo-save ()
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-trailing-lines t)
 '(evil-shift-width 2)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
