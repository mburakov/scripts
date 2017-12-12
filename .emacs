(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(require 'rtags)
(require 'company)
(require 'evil)

(add-to-list 'load-path "~/projects/scripts")
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized")
(customize-set-variable 'frame-background-mode 'dark)
(load-theme 'solarized t)

(require 'ebear)

(evil-mode)
(push 'company-rtags company-backends)
(global-company-mode)
(global-linum-mode 1)
(setq ediff-split-window-function 'split-window-horizontally)
(set-default-font "Monospace-11:weight=bold")
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-c-init ()
  (define-key evil-normal-state-map (kbd "C-]") 'rtags-find-symbol-at-point)
  (define-key evil-normal-state-map (kbd "<f12>") 'vc-ediff)
  (define-key evil-insert-state-map (kbd "<backtab>")
    '(lambda () (interactive) (insert-char ?\t)))
  (setq-local c-basic-offset 2)
  (electric-pair-local-mode))

(defun clang-format ()
  (interactive)
  (let ((prev-point (point)))
    (shell-command-on-region
     (point-min) (point-max) "clang-format" t)
    (delete-region (point) (point-max))
    (goto-char prev-point)))

(defun hexify ()
  (interactive)
  (shell-command-on-region
   (point) (mark) "od -An -tx1 -" t))

(add-hook 'c-mode-hook 'my-c-init)
(add-hook 'c++-mode-hook 'my-c-init)

(defun make-header ()
  (interactive)
  (search-forward "(")
  (push-mark (1- (point)))
  (let ((sol (line-beginning-position)))
    (if (search-backward " " sol t)
        (forward-char)
      (goto-char sol)))
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
   (if mark-active (mark) (point-min))
   (if mark-active (point) (point-max))
   "plantuml -p | feh -")
  (deactivate-mark))

(defun pdflatex ()
  (interactive)
  (shell-command
   (concat "pdflatex " (buffer-file-name))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (quote (("." . "~/.saves"))))
 '(compilation-scroll-output t)
 '(delete-trailing-lines t)
 '(evil-shift-width 2)
 '(frame-background-mode (quote dark))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(menu-bar-mode nil)
 '(package-selected-packages (quote (evil company)))
 '(rtags-autostart-diagnostics t)
 '(rtags-completions-enabled t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'erase-buffer 'disabled nil)
