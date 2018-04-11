(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

(setenv "EDITOR" "emacsclient")
(setenv "PAGER" "cat")
(setenv "PATH" (concat (getenv "PATH") ":"
                       (getenv "HOME") "/projects/scripts"))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'default-frame-alist '(font . "Iosevka medium 12"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c++-mode-hook 'my-c-init)
(add-hook 'c-mode-hook 'my-c-init)

(setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)
(setq cquery-cache-dir "/tmp/cquery")
(setq cquery-executable "/usr/bin/cquery")
(setq cquery-extra-args '("--log-file=/tmp/cq.log"))
(setq cquery-extra-init-params '(:cacheFormat "msgpack"))
(setq ediff-split-window-function 'split-window-horizontally)

(require 'company)
(require 'cquery)
(require 'lsp-ui)

(customize-set-variable 'frame-background-mode 'dark)
(evil-mode)
(global-company-mode)
(global-linum-mode 1)
(load-theme 'solarized t)
(push 'company-lsp company-backends)

(define-key evil-insert-state-map (kbd "<backtab>")
  '(lambda () (interactive) (insert-char ?\t)))
(define-key evil-insert-state-map (kbd "<tab>") 'company-complete)
(define-key evil-normal-state-map (kbd "<backtab>") 'previous-buffer)
(define-key evil-normal-state-map (kbd "<tab>") 'next-buffer)
(define-key evil-normal-state-map (kbd "SPC .") 'xref-find-definitions)
(define-key evil-normal-state-map (kbd "SPC <backspace>") 'kill-any-buffer)
(define-key evil-normal-state-map (kbd "SPC <down>") 'windmove-down)
(define-key evil-normal-state-map (kbd "SPC <left>") 'windmove-left)
(define-key evil-normal-state-map (kbd "SPC <right>") 'windmove-right)
(define-key evil-normal-state-map (kbd "SPC <up>") 'windmove-up)

(defun kill-any-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-this-buffer)))

(defun my-c-init ()
  (local-set-key (kbd "<backtab>")
                 '(lambda () (interactive) (insert-char ?\t)))
  (setq-local c-basic-offset 2)
  (electric-pair-local-mode)
  (lsp-cquery-enable)
  (lsp-ui-mode)
  (flycheck-mode))

(defun clang-format ()
  "Format current buffer with clang-format."
  (interactive)
  (let ((prev-point (point)))
    (shell-command-on-region
     (point-min) (point-max) "clang-format" t)
    (delete-region (point) (point-max))
    (goto-char prev-point)))

(defun hexify ()
  "Transform region to hex character codes."
  (interactive)
  (shell-command-on-region
   (point) (mark) "od -An -tx1 -" t))

(defun sudo-save ()
  "Overwrite current file as root using tramp."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun plantuml ()
  "Interpret the whole buffer or active region with PlantUML."
  (interactive)
  (shell-command-on-region
   (if mark-active (mark) (point-min))
   (if mark-active (point) (point-max))
   "plantuml -p | feh -")
  (deactivate-mark))

(defun pdflatex ()
  "Interpret current buffer as a latex markup."
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
 '(edit-server-new-frame nil)
 '(enable-local-eval t)
 '(fci-dash-pattern 0.1)
 '(fci-rule-color "#586e75")
 '(fci-rule-column 80)
 '(fci-rule-use-dashes t)
 '(frame-background-mode (quote dark))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(lsp-ui-doc-enable nil)
 '(lsp-ui-imenu-enable nil)
 '(lsp-ui-peek-enable nil)
 '(lsp-ui-sideline-enable nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (magit lsp-ui cquery company-lsp fill-column-indicator color-theme-solarized evil)))
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
