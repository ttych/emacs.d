;;; init.el --- base emacs configuration

;;; Commentary:

;;; Code:

;; debug
; (setq debug-on-error t)

;; Define vars here
(defvar my-init-file (expand-file-name "my-init.el" user-emacs-directory)
  "All configurations stored in this file.")

(defvar my-vendor-dir (expand-file-name "packages/" user-emacs-directory)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(unless (file-exists-p my-vendor-dir)
  (make-directory my-vendor-dir))

(defvar savefile-dir (expand-file-name "savefile/" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

(add-to-list 'load-path my-vendor-dir)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backward-delete-char-untabify-method nil)
 '(column-number-mode t)
 '(delete-selection-mode t)
 '(delete-trailing-lines t)
 '(delete-trailing-whitespace t)
 '(gc-cons-threshold 50000000)
 '(global-auto-revert-mode t)
 '(global-display-line-numbers-mode t)
 '(history-length 1000)
 '(indent-tabs-mode t)
 '(indicate-empty-lines t)
 '(large-file-warning-threshold 100000000)
 '(line-number-mode t)
 '(load-prefer-newer t)
 '(max-lisp-eval-depth 2000)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position t)
 '(show-trailing-whitespace t)
 '(size-indication-mode t)
 '(tab-width 4)
 '(transient-mark-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (prefer-coding-system 'utf-8)
;; (set-default-coding-systems 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-language-environment 'utf-8)


(if (file-exists-p my-init-file)
    (load-file my-init-file))

;;; init.el ends here
