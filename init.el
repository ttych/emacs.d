;;; init.el --- Thomas Emacs configuration

(require 'package)
  ; [Enter ↵] (package-menu-describe-package) → Describe the package under cursor.
  ; [i] (package-menu-mark-install) → mark for installation.
  ; [u] (package-menu-mark-unmark) → unmark.
  ; [d] (package-menu-mark-delete) → mark for deletion (removal of a installed package).
  ; [x] (package-menu-execute) → for “execute” (start install/uninstall of marked items).
  ; [r] (package-menu-refresh) → refresh the list from server.
  ; (For complete list of keys, call describe-mode [Ctrl+h m])
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  ; (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(unless package-archive-contents
  (package-refresh-contents))
(global-set-key (kbd "C-x P") 'list-packages)

;; (when (>= emacs-major-version 24)
;;   (require 'package)
;;   (add-to-list 'package-archives
;; 	       '("melpa" . "https://melpa.org/packages/") t)
;;   (package-initialize)
;;   ;; keep the installed packages in .emacs.d
;;   (setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
;;   ;; update the package metadata is the local cache is missing
;;   (unless package-archive-contents
;;     (package-refresh-contents))
;;   (global-set-key (kbd "C-x P") 'list-packages)
;;   )

(defconst emacs-config-directory (expand-file-name "config" user-emacs-directory))
(unless (file-exists-p emacs-config-directory)
  (make-directory emacs-config-directory))

(defconst emacs-third-party-directory (expand-file-name "third-party" user-emacs-directory))
(unless (file-exists-p emacs-third-party-directory)
  (make-directory emacs-third-party-directory))

(defconst user-savefile-directory (expand-file-name "savefile" user-emacs-directory))
(unless (file-exists-p user-savefile-directory)
  (make-directory user-savefile-directory))

(add-to-list 'load-path emacs-config-directory)
(add-to-list 'load-path emacs-third-party-directory)
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))


(setq user-login (getenv "USER")
      user-full-name "Thomas Tych"
      user-mail-address (getenv "thomas.tych@gmail.com"))

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst *is-unix* (member system-type '(freebsd)))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (member system-type '(gnu gnu/linux gnu/kfreebsd)))


;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; menu bar
(menu-bar-mode -1)

;; the blinking cursor is nothing, but an annoyance
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode -1))

;; disable startup screen
(setq inhibit-startup-screen t)

;; scrach message
(setq initial-scratch-message nil)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position t)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)
(global-linum-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)
;(defalias 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
;(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
;(setq-default tab-width 8)            ;; but maintain correct appearance
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; highlight current line


;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;(prefer-coding-system 'utf-8)
;(set-default-coding-systems 'utf-8)
;(set-terminal-coding-system 'utf-8)
;(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; use hippie-expand instead of dabbrev
;(global-set-key (kbd "M-/") #'hippie-expand)
;(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
;(global-set-key (kbd "C-x \\") #'align-regexp)

;(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
;(global-set-key (kbd "s-<") #'beginning-of-buffer)
;(global-set-key (kbd "s->") #'end-of-buffer)
;(global-set-key (kbd "s-q") #'fill-paragraph)
;(global-set-key (kbd "s-x") #'execute-extended-command)

;; smart tab behavior - indent or complete
;(setq tab-always-indent 'complete)

;;; edit

;;; mini buffer
(icomplete-mode 1)


(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

;;; built-in packages
(use-package paren
  :config
  (show-paren-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silent)
  (setq abbrev-file-name "~/.emacs.d/abbrev_defs")
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-savefile-directory))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-savefile-directory))
  (savehist-mode +1))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-savefile-directory)
        recentf-max-saved-items 500
        recentf-max-menu-items 25
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (global-set-key (kbd "C-c C-r") 'recentf-open-files)
  (recentf-mode +1))

;; (use-package windmove
;;   :config
;;   ;; use shift + arrow keys to switch between visible buffers
;; (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x))

(use-package lisp-mode
  :config
  (defun user-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
	(crux-start-or-switch-to 'ielm "*ielm*"))

    (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
	(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
	(define-key emacs-lisp-mode-map (kbd "C-c C-z") #'user-visit-ielm)
	(define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
	(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
	(add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
	(add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))


;; third-party packages

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :config
;;   (when (memq window-system '(mac ns))
;;     (exec-path-from-shell-initialize)))

(use-package which-key
  :ensure t
  :config
  (which-key-mode +1))

(use-package rainbow-delimiters
  :ensure t)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;; (load-theme 'solarized t))

;; (use-package mandm-theme
;;   :ensure t
;;   :config
;;   (load-theme 'mandm t))

;; (use-package avy
;;   :ensure t
;;   :bind (("s-." . avy-goto-word-or-subword-1)
;;          ("s-," . avy-goto-char))
;;   :config
;;   (setq avy-background t))

(use-package magit
  :ensure t
  :bind (("C-c C-g m" . magit-status)
		 ("C-c C-g g" . magit-status)))

(use-package git-timemachine
  :ensure t
  :bind (("C-c C-g t" . git-timemachine)))

(use-package ag
  :ensure t)

;; (use-package projectile
;;   :ensure t
;;   :init
;;   (setq projectile-completion-system 'ivy)
;;   :config
;;   (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;;   (projectile-mode +1))

;; (use-package pt
;;   :ensure t)

(use-package expand-region
  :ensure t
  :bind (("M-+" . er/expand-region)
		 ("C-c +" . er/expand-region)))

(use-package elisp-slime-nav
  :ensure t
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hogok #'paredit-mode)
  ;; enable in the *scratch* buffer
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode))

(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

;; (use-package easy-kill
;;   :ensure t
;;   :config
;;   (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package move-text
  :ensure t
  :bind  (([(shift up)] . move-text-up)
		  ([(shift down)] . move-text-down)))


(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . gfm-mode))
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc\\'")

(use-package yaml-mode
  :ensure t)

(use-package cask-mode
  :ensure t)


(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode))

(use-package ruby-mode
  :config
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'ruby-mode-hook #'subword-mode))

;; (use-package clojure-mode
;;   :ensure t
;;   :config
;;   (add-hook 'clojure-mode-hook #'paredit-mode)
;;   (add-hook 'clojure-mode-hook #'subword-mode)
;;   (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))

;; (use-package cider
;;   :ensure t
;;   :config
;;   (setq nrepl-log-messages t)
;;   (add-hook 'cider-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook #'eldoc-mode)
;;   (add-hook 'cider-repl-mode-hook #'paredit-mode)
;;   (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode))

;; (use-package flycheck-joker
;;   :ensure t)

;; (use-package elixir-mode
;;   :ensure t
;;   :config
;;   (add-hook 'elixir-mode #'subword-mode))

;; (use-package erlang
;;   :ensure t
;;   :config
;;   (when (eq system-type 'windows-nt)
;;     (setq erlang-root-dir "C:/Program Files/erl7.2")
;;     (add-to-list 'exec-path "C:/Program Files/erl7.2/bin")))

;; (use-package haskell-mode
;;   :ensure t
;;   :config
;;   (add-hook 'haskell-mode #'subword-mode))


(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.5)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)
  ;; invert the navigation direction if the the completion popup-isearch-match
  ;; is displayed on top (happens near the bottom of windows)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode)
  :bind (("C-c T p" . hl-todo-previous)
		 ("C-c T n" . hl-todo-next)
		 ("C-c T o" . hl-todo-occur)))

;; (use-package zop-to-char
;;   :ensure t
;;   :bind (("M-z" . zop-up-to-char)
;;          ("M-Z" . zop-to-char)))

;; (use-package imenu-anywhere
;;   :ensure t
;;   :bind (("C-c i" . imenu-anywhere)
;;          ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
        ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; (use-package super-save
;;   :ensure t
;;   :config
;;   ;; add integration with ace-window
;;   (add-to-list 'super-save-triggers 'ace-window)
;;   (super-save-mode +1))

;; (use-package crux
;;   :ensure t
;;   :bind (("C-c o" . crux-open-with)
;;          ("M-o" . crux-smart-open-line)
;;          ("C-c n" . crux-cleanup-buffer-or-region)
;;          ("C-c f" . crux-recentf-find-file)
;;          ("C-M-z" . crux-indent-defun)
;;          ("C-c u" . crux-view-url)
;;          ("C-c e" . crux-eval-and-replace)
;;          ("C-c w" . crux-swap-windows)
;;          ("C-c D" . crux-delete-file-and-buffer)
;;          ("C-c r" . crux-rename-buffer-and-file)
;;          ("C-c t" . crux-visit-term-buffer)
;;          ("C-c k" . crux-kill-other-buffers)
;;          ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
;;          ("C-c I" . crux-find-user-init-file)
;;          ("C-c S" . crux-find-shell-init-file)
;;          ("s-r" . crux-recentf-find-file)
;;          ("s-j" . crux-top-join-line)
;;          ("C-^" . crux-top-join-line)
;;          ("s-k" . crux-kill-whole-line)
;;          ("C-<backspace>" . crux-kill-line-backwards)
;;          ("s-o" . crux-smart-open-line-above)
;;          ([remap move-beginning-of-line] . crux-move-beginning-of-line)
;;          ([(shift return)] . crux-smart-open-line)
;;          ([(control shift return)] . crux-smart-open-line-above)
;;          ([remap kill-whole-line] . crux-kill-whole-line)
;;          ("C-c s" . crux-ispell-word-then-abbrev)))

;; (use-package diff-hl
;;   :ensure t
;;   :config
;;   (global-diff-hl-mode +1)
;;   (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; (use-package undo-tree
;;   :ensure t
;;   :config
;;   ;; autosave the undo-tree history
;;   (setq undo-tree-history-directory-alist
;;         `((".*" . ,temporary-file-directory)))
;;   (setq undo-tree-auto-save-history t))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "C-c o") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

;; ;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :config
  (volatile-highlights-mode +1))

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t)
;;   (setq enable-recursive-minibuffers t)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume)
;;   (global-set-key (kbd "<f6>") 'ivy-resume))

;; (use-package swiper
;;   :ensure t
;;   :config
;;   (global-set-key "\C-s" 'swiper))

;; (use-package counsel
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "M-x") 'counsel-M-x)
;;   (global-set-key (kbd "C-x C-f") 'counsel-find-file)
;;   (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;;   (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;;   (global-set-key (kbd "<f1> l") 'counsel-find-library)
;;   (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   (global-set-key (kbd "C-c a") 'counsel-ag)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)
;;   (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(expand-region ag zenburn-theme use-package solarized-theme rainbow-mode rainbow-delimiters mandm-theme magit git-timemachine))))

(setq key-bindings-file (expand-file-name "key-bindings.el" emacs-config-directory))
(when (file-exists-p key-bindings-file)
  (load key-bindings-file))

(setq mode-config-file (expand-file-name "mode.el" emacs-config-directory))
(when (file-exists-p mode-config-file)
  (load mode-config-file))

(setq local-file (expand-file-name "local.el" emacs-config-directory))
(when (file-exists-p local-file)
  (load local-file))

;;; init.el ends here
