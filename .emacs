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
(add-to-list 'default-frame-alist '(font . "Hack 11"))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'c++-mode-hook 'my-c-init)
(add-hook 'c-mode-hook 'my-c-init)

(setq cquery-executable "/usr/bin/cquery")
(setq ediff-split-window-function 'split-window-horizontally)

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck))
(require 'cquery)
(require 'company-lsp)
(require 'edit-server)

(customize-set-variable 'frame-background-mode 'dark)
(global-company-mode)
(global-linum-mode 1)
(load-theme 'solarized t)
(push 'company-lsp company-backends)
(server-start)
(edit-server-start)

(defun my-c-init ()
  (local-set-key (kbd "<backtab>")
                 '(lambda () (interactive) (insert-char ?\t)))
  (setq-local c-basic-offset 4)
  (electric-pair-local-mode)
  (lsp-cquery-enable)
  (flycheck-mode))

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
 '(edit-server-new-frame nil)
 '(fci-dash-pattern 0.25)
 '(fci-rule-column 80)
 '(fci-rule-use-dashes t)
 '(frame-background-mode (quote dark))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(menu-bar-mode nil)
 '(package-selected-packages
   (quote
    (fill-column-indicator edit-server color-theme-solarized company-lsp cquery evil)))
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
