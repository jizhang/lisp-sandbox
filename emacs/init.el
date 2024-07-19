(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package markdown-mode
  :init (setq markdown-command '("pandoc" "--embed-resources" "--standalone" "--shift-heading-level-by=-1")))

(use-package editorconfig
  :config (editorconfig-mode 1))

(use-package helm
  :config (helm-mode 1))

(use-package helm-gtags
  :init
  (setq
    helm-gtags-ignore-case t
    helm-gtags-auto-update t)
  :config
  (progn
    (add-hook 'c-mode-hook 'helm-gtags-mode)
    (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
    (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
    (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
    (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
    (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)))

(push '(fullscreen . maximized) default-frame-alist)
(push '(background-color . "#FFFFDF") default-frame-alist)
(tool-bar-mode -1)
(setq-default word-wrap t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(when (fboundp 'markdown-mode)
  (defun bubble ()
    (interactive)
    (kill-region (region-beginning) (region-end))
    (move-beginning-of-line 1)
    (yank))
  (defun hook ()
    (local-set-key (kbd "C-c a") 'bubble))
  (add-hook 'markdown-mode-hook 'hook))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(helm-gtags editorconfig markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
