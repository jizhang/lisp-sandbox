(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package markdown-mode
  :init
  (setq markdown-command '("pandoc" "--embed-resources" "--standalone"
                           "--shift-heading-level-by=-1"))
  (when (eq system-type 'windows-nt)
    (setq markdown-command-needs-filename t))
  :bind (:map markdown-mode-map
              ("C-c a" . bubble-region)))

(use-package editorconfig
  :init (editorconfig-mode 1))

(use-package helm
  :init (helm-mode 1))

(use-package helm-gtags
  :if (executable-find "gtags")
  :init
  (setq
    helm-gtags-ignore-case t
    helm-gtags-auto-update t
    helm-gtags-prefix-key "\C-cg"
    helm-gtags-suggested-key-mapping t)
  :hook (c-mode . helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
              ("C-j" . helm-gtags-select)
              ("M-." . helm-gtags-dwim)
              ("M-," . helm-gtags-pop-stack)
              ("C-c <" . helm-gtags-previous-history)
              ("C-c >" . helm-gtags-next-history)))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (unless (executable-find "clang")
    (delete 'company-clang company-backends)))

(use-package semantic
  :hook (prog-mode . semantic-mode)
  :bind (:map semantic-mode-map
              ("C-c C-j" . semantic-ia-fast-jump)
              ("C-c C-s" . semantic-ia-show-summary)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(background-color . "#FFFFDF"))
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(setq-default word-wrap t)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(defun bubble-region ()
  (interactive)
  (kill-region (region-beginning) (region-end))
  (move-beginning-of-line 1)
  (yank))

(defun compile-now ()
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(global-set-key (kbd "<f5>") 'compile-now)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company helm-gtags editorconfig markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
