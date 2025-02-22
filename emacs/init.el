(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
(setq custom-file (make-temp-file "emacs-custom"))

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/720
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init (exec-path-from-shell-initialize))

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
  (remove-hook 'flymake-diagnostic-functions #'flymake-proc-legacy-flymake)
  :bind (:map flymake-mode-map
              ("C-c f l" . flymake-show-buffer-diagnostics)
              ("C-c f a" . flymake-show-project-diagnostics)
              ("C-c f n" . flymake-goto-next-error)
              ("C-c f p" . flymake-goto-prev-error)))

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

(use-package ert
  :bind (("<f6>" . ert-silently)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(background-color . "#FFFFDF"))
(tool-bar-mode -1)
(column-number-mode 1)
(electric-pair-mode 1)
(setq-default word-wrap t
              word-wrap-by-category t)
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
    (if (and (char-after) (char-equal (char-after) c))
        (forward-char)
      (insert-char c))))

(defun buffer-prefix ()
  (let ((basename (file-name-nondirectory (buffer-file-name))))
    (cond
     ((string-prefix-p "en-" basename) 'en)
     ((string-prefix-p "jp-" basename) 'jp)
     (t (error "Unknown file type")))))

(defun bubble-phrase ()
  (interactive "*")
  (if (use-region-p)
      (bubble-region (region-beginning) (region-end))
    (pcase (buffer-prefix)
      ('en (mark-phrase-en))
      ('jp (mark-phrase-jp)))
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

(defun count-phrases ()
  (interactive)
  (let* ((line (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (save-excursion
                   (beginning-of-line)
                   (thing-at-point 'line))))
         (exps (pcase (buffer-prefix)
                 ('en (cons "[[:space:],]+" ","))
                 ('jp (cons "[[:space:]]+" "[[:blank:]]"))))
         (line (string-trim line (car exps) (car exps)))
         (count (if (string-empty-p line)
                    0
                  (length (string-split line (cdr exps))))))
    (message "%d" count)
    count))

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
        ((derived-mode-p 'emacs-lisp-mode) (eval-buffer))
        (t (call-interactively 'compile))))

(defun enable-flymake-el ()
  (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
  (flymake-mode 1))

(defun ert-silently (arg)
  (interactive "P")
  (when (derived-mode-p 'emacs-lisp-mode)
    (save-buffer)
    (eval-buffer))
  (if (consp arg)
      (call-interactively 'ert)
    (ert t)))
