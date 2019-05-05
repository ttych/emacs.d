;; Comment region
(global-set-key (kbd "C-c #") 'comment-region)
(global-set-key (kbd "C-c @") 'uncomment-region)

;; Auto-Fill
(global-set-key (kbd "C-c C-m f") 'auto-fill-mode)
;; Whitespace
(global-set-key (kbd "C-c C-m w") 'whitespace-mode)
;; Autosave
(global-set-key (kbd "C-c C-m s") 'auto-save-mode)

; C-c C-r    =>  recentf
; C-c C-g g  => magit-status
; C-c C-g t  => git-timemachine



;; Indent region
;(global-set-key (kbd "C-c TAB") 'indent-region)

;; Time
;(global-set-key (kbd "C-c t") 'do_insert_time)

;; Speedbar
;(global-set-key (kbd "C-c s") 'speedbar)

;; goto
;(global-set-key (kbd "C-l") 'goto-line)

;;; Compare windows
;(global-set-key "\C-cw" ’compare-windows)
;(global-set-key (kbd "C-c w") 'compare-windows)

;;; Keybinding for ‘occur’
;(global-set-key (kbd "C-c o") 'occur)

;;; unbind ‘C-x f’ set=fill-column
;(global-unset-key "\C-xf")

;;; Rebind ‘C-x C-b’ for ‘buffer-menu’
;(global-set-key "\C-x\C-b" 'buffer-menu)


(global-set-key (kbd "M-j")
                (lambda ()
                  (interactive)
                  (join-line -1)))
