;; Comment region
(global-set-key (kbd "C-c #") 'comment-region)
(global-set-key (kbd "C-c @") 'uncomment-region)

;; Auto-Fill
(global-set-key (kbd "C-c C-t f") 'auto-fill-mode)

;; Whitespace
(global-set-key (kbd "C-c C-t w") 'whitespace-mode)

;; Autosave
(global-set-key (kbd "C-c C-t s") 'auto-save-mode)



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
