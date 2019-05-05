
(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  )

(use-package multiple-cursors
  :ensure t
  )

(use-package iy-go-to-char
  :ensure t
  :bind (
         ("C-c f" . iy-go-to-char)
         ("C-c F" . iy-go-to-char-backward)
         ("C-c ." . iy-go-to-or-up-to-continue)
         ("C-c ," . iy-go-to-or-up-to-continue-backward)
         ("C-c v f" . iy-go-to-char)
         ("C-c v F" . iy-go-to-char-backward)
         ("C-c v ." . iy-go-to-or-up-to-continue)
         ("C-c v ," . iy-go-to-or-up-to-continue-backward)
         ("C-c v t" . iy-go-up-to-char)
         ("C-c v T" . iy-go-up-to-char-backward)
         )
  :config
  ;; multiple-cursors
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  )

(use-package yasnippet
  :ensure t
  :bind (
         ("C-c y n" . yas-new-snippet)
         ("C-c y i" . yas-insert-snippet)
         ("C-c y v" . yas-visit-snippet-file)
         )
  :config
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t
  )

;; (use-package key-chord
;;   :ensure t
;;   :config
;;   (key-chord-mode 1)
;;   (key-chord-define-global "ss" 'isearch-forward)
;;   )

;; (use-package key-seq
;;   :ensure t
;;   )
