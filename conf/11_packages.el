
(use-package diminish
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode 1)
  :diminish
  )

(use-package multiple-cursors
  :ensure t
  :bind (
         ("M-# e" . mc/edit-lines)
         ("M-# n" . mc/mark-next-like-this)
         ("M-# p" . mc/mark-previous-like-this)
         ("M-# w" . mc/mark-next-like-this-word)
         ("M-# W" . mc/mark-previous-like-this-word)
         ("M-# s" . mc/mark-next-like-this-word)
         ("M-# S" . mc/mark-previous-like-this-word)
         ("M-# a" . mc/mark-all-like-this)
         ("C-c c e" . mc/edit-lines)
         ("C-c c n" . mc/mark-next-like-this)
         ("C-c c p" . mc/mark-previous-like-this)
         ("C-c c w" . mc/mark-next-like-this-word)
         ("C-c c W" . mc/mark-previous-like-this-word)
         ("C-c c s" . mc/mark-next-like-this-word)
         ("C-c c S" . mc/mark-previous-like-this-word)
         ("C-c c a" . mc/mark-all-like-this)
         )
  )

(use-package expand-region
  :ensure t
  :bind (("M-+" . er/expand-region)
		 ("C-c +" . er/expand-region))
  )

(use-package ace-jump-mode
  :ensure t
  :bind (
         ("C-c j" . ace-jump-mode)
         ("C-c J" . ace-jump-mode-pop-mark)
         ("M-g j" . ace-jump-mode)
         ("M-g J" . ace-jump-mode-pop-mark)
         )
  )

(use-package iy-go-to-char
  :ensure t
  :bind (
         ("M-g f" . iy-go-to-char)
         ("M-g F" . iy-go-to-or-up-to-continue)
         ("M-g b" . iy-go-to-char-backward)
         ("M-g B" . iy-go-to-or-up-to-continue-backward)
         ("C-c f" . iy-go-to-char)
         ("C-c F" . iy-go-to-or-up-to-continue)
         ("C-c b" . iy-go-to-char-backward)
         ("C-c B" . iy-go-to-or-up-to-continue-backward)
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
  :diminish (yas-minor-mode . "")
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



;;; language

(use-package groovy-mode
  :ensure t)
