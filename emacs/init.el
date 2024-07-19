(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(use-package markdown-mode
  :ensure t
  :init (setq markdown-command '("pandoc" "--embed-resources" "--standalone" "--shift-heading-level-by=-1")))

(use-package editorconfig
  :ensure t
  :config (editorconfig-mode 1))

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
 '(package-selected-packages '(editorconfig markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
