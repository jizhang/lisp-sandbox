(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)

(use-package delight
  :config
  (delight '((eldoc-mode nil "eldoc")
             (abbrev-mode " Abv" abbrev))))

(use-package markdown-mode
  :custom
  (markdown-command '("pandoc" "--embed-resources" "--standalone" "--shift-heading-level-by=-1"))
  (markdown-command-needs-filename (eq system-type 'windows-nt))
  :bind (:map markdown-mode-map
              ("C-c a" . bubble-phrase)
              ("C-c d" . insert-date)
              ("C-c c" . calculate-calories)))

(use-package editorconfig
  :init (editorconfig-mode 1)
  :delight)

(use-package vertico
  :init (vertico-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package savehist
  :init (savehist-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (unless (executable-find "clang")
    (delete 'company-clang company-backends))
  :delight)

(use-package flymake
  :hook ((c-mode . flymake-mode)
         (emacs-lisp-mode . enable-flymake-el))
  :config
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake))

(use-package rust-mode
  :if (executable-find "rustc"))

(use-package eglot
  :init
  (when (executable-find "rust-analyzer")
    (add-hook 'rust-mode-hook 'eglot-ensure))
  :custom
  (eglot-autoshutdown t))

(use-package compile
  :bind (("<f5>" . compile-now))
  :custom
  (compile-command "make ")
  (compilation-read-command nil)
  (compilation-window-height 15))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(background-color . "#FFFFDF"))
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(electric-pair-mode 1)
(setq-default word-wrap t)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq eval-expression-print-length 1000)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

(when (eq system-type 'windows-nt)
  (set-language-environment "UTF-8")
  (dolist (face '(default fixed-pitch))
    (set-face-attribute face nil :family "Consolas" :height 105))
  (dolist (script '(han cjk-misc kana))
    (set-fontset-font t script "微软雅黑"))
  (dolist (script '(symbol emoji))
    (set-fontset-font t script "Segoe UI Emoji")))

(defun ensure-string (s)
  (cl-assert (stringp s))
  (dolist (c (append s nil))
    (if (char-equal (char-after) c)
        (forward-char)
      (insert-char c))))

(defun bubble-phrase ()
  (interactive "*")
  (if (use-region-p)
      (bubble-region (region-beginning) (region-end))
    (let ((basename (file-name-nondirectory (buffer-file-name))))
      (cond ((string-prefix-p "en-" basename) (mark-phrase-en))
            ((string-prefix-p "jp-" basename) (mark-phrase-jp))
            (t (error "Unknown file type"))))
    (bubble-region (region-beginning) (region-end))))

(defun mark-phrase (delimiter)
  (if (re-search-backward delimiter (pos-bol) t)
      (forward-char)
    (beginning-of-line))
  (push-mark)
  (if (re-search-forward delimiter (pos-eol) t)
      (backward-char)
    (end-of-line))
  (when (string-blank-p (buffer-substring (region-beginning) (region-end)))
    (error "Nothing to select")))

(defun mark-phrase-en ()
  (interactive "*")
  (mark-phrase "[,/]")
  (exchange-point-and-mark)
  (skip-chars-forward "[:blank:]")
  (exchange-point-and-mark)
  (ensure-string ", ")
  (activate-mark))

(defun mark-phrase-jp ()
  (interactive "*")
  (mark-phrase "[[:blank:]/]")
  (ensure-string "　")
  (activate-mark))

(defun bubble-region (beg end)
  (interactive "*r")
  (kill-region beg end)
  (beginning-of-line)
  (yank))

(defun replace-space ()
  (interactive "*")
  (beginning-of-line)
  (while (search-forward " " (pos-eol) t)
    (replace-match "　")))

(defun insert-date (arg)
  (interactive "*P")
  (let* ((days (if (integerp arg) arg 0))
         (time (time-add (current-time) (* days 86400)))
         (date (format-time-string "%Y-%m-%d" time)))
    (if (consp arg)
        (insert date)
      (insert "## " date "\n"))
    date))

(defun calculate-calories (message-only)
  (interactive "P")
  (unless message-only (barf-if-buffer-read-only))
  (let ((total 0))
    (save-excursion
      (while (and (zerop (forward-line -1))
                  (looking-at "^\\w+\\s-+\\([0-9]+\\)"))
        (setq total (+ total (string-to-number (match-string 1))))))
    (if message-only
        (message "%d" total)
      (insert (number-to-string total)))
    total))

(defun compile-now ()
  (interactive)
  (save-buffer)
  (cond ((derived-mode-p 'rust-mode) (rust-run))
        (t (call-interactively 'compile))))

(defun enable-flymake-el ()
  (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
  (flymake-mode 1))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(rust-mode delight company editorconfig markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
