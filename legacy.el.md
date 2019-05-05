# server

since 26.0 replaced by **daemon**.

```
;;; daemon
(require 'server)
(unless (server-running-p)
   (server-start))
```

# global-linum-mode

since 26.0 replaced by **line-number-mode**.

```
;; mode line settings
(global-linum-mode 1)
(defadvice linum-update-window (around linum-dynamic activate)
  (let* ((w (length (number-to-string
                    (count-lines (point-min) (point-max)))))
         (linum-format (concat "%" (number-to-string w) "d|")))
    ad-do-it))
;; (setq linum-format "%d ")
;; (setq linum-format "%4d \u2502 ")
```

# saveplace

since 25.1 replaced by **save-place-mode**.

```
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))
```

# local

load os specific environment :
```
(load (car (reverse (split-string (symbol-name system-type) "\\/"))))
```

load hostname specific environment :
```
(load (car (split-string (system-name) "\\.")) t)
```

# goto-line

Activate linum only when using goto.

```
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
```

# move

```
(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (move-to-column col)))

(global-set-key (kbd "<C-S-down>") 'move-line-down)
(global-set-key (kbd "<C-S-up>") 'move-line-up)
```
