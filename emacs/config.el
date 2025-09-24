;; make the esc key as leader key
(global-set-key (kbd "<escape>") 'leader)
(define-prefix-command 'leader)

(global-set-key (kbd "C-c C-<return>") 'go-to-config)

;; Move focus
(global-set-key (kbd "C-M-<up>") 'windmove-up)
(global-set-key (kbd "C-M-<down>") 'windmove-down)
(global-set-key (kbd "C-M-<left>") 'windmove-left)
(global-set-key (kbd "C-M-<right>") 'windmove-right)

(define-key org-mode-map (kbd "M-<up>") 'windmove-up)
(define-key org-mode-map (kbd "M-<down>") 'windmove-down)
(define-key org-mode-map (kbd "M-<left>") 'windmove-left)
(define-key org-mode-map (kbd "M-<right>") 'windmove-right)


;; Make a new line below the current line
(define-key leader (kbd "RET") 'insert-new-line-below)

;; Jump to the start or end of the buffer
(define-key leader (kbd "<up>") 'beginning-of-buffer)
(define-key leader (kbd "<down>") 'end-of-buffer)

;; Jump to a specific line
(define-key leader (kbd "g l") 'goto-line)

;; Vim-like navigation motions
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-j") 'next-line)
(define-key org-mode-map (kbd "C-j") 'next-line) ; org-mode can be a pain
(define-key global-map (kbd "C-k") 'previous-line)
(define-key global-map (kbd "C-l") 'forward-char)

;; go-to style commands
(define-key leader (kbd "g c") (lambda () (interactive) (find-file "~/.config/emacs/config.org")))
(define-key leader (kbd "g t") (lambda () (interactive) (find-file "~/orgfiles/Tasks.org")))
(define-key leader (kbd "g s") (lambda () (interactive) (dired "~/.config/emacs/snippets/org-mode")))
(define-key leader (kbd "g d") 'dashboard-open)

(define-key leader (kbd "b") 'projectile-switch-to-buffer)

(define-key leader (kbd "x f") 'find-file)
(define-key leader (kbd "p f") 'projectile-find-file)

(define-key leader (kbd "0") 'delete-window)
(define-key leader (kbd "1") 'delete-other-windows)
(define-key leader (kbd "2") 'split-window-below)
(define-key leader (kbd "3") 'split-window-right)
(define-key leader (kbd "DEL") 'kill-current-buffer-and-window)

(define-key leader (kbd "o t") 'org-toggle-item)
(define-key leader (kbd "o l") 'org-open-at-point)

;;org roam keys
(define-key leader (kbd "n i") 'org-roam-node-insert)
(define-key leader (kbd "n f") 'org-roam-node-find)
(define-key leader (kbd "n u") 'org-roam-ui-mode)
(define-key leader (kbd "n #") 'org-roam-tag-add)
(define-key leader (kbd "n c") 'org-roam-add-citation)
(define-key leader (kbd "n h") 'org-is-get-create)

(define-key leader (kbd "h") 'help)

(defun python-shell-send-line ()
  "Select the current line and send it to the Python shell."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((start (point)))
      (end-of-line)
      (python-shell-send-region start (point)))))

(define-key leader (kbd "r l") 'python-shell-send-line)
(define-key leader (kbd "r r") 'python-shell-send-region)
(define-key leader (kbd "r b") 'python-shell-send-buffer)

;;selections ('m' is for mark, 's' is taken by 'save')
(define-key leader (kbd "m l") 'select-current-line)
(define-key leader (kbd "m a") 'select-buffer)
(define-key leader (kbd "m p") 'select-paragraph)
(define-key leader (kbd "m w") 'select-word)
(define-key leader (kbd "m m w") 'mc/mark-all-words-like-this)
(define-key leader (kbd "m m a") 'mc/mark-all-like-this)
(define-key leader (kbd "m f") 'select-line-forward)
(define-key leader (kbd "m b") 'select-line-backward)

(define-key leader (kbd "k l") 'kill-whole-line)
(define-key leader (kbd "k f") 'kill-line)
(define-key leader (kbd "k b") 'kill-to-start-of-line)
(define-key leader (kbd "k r") 'kill-region)
(define-key leader (kbd "k p") 'kill-whole-paragraph)

(define-key leader (kbd "k RET") 'save-buffers-kill-terminal)

(define-key leader (kbd "d r") 'delete-region)
(define-key leader (kbd "d l") 'delete-line)
(define-key leader (kbd "d f") 'delete-line-forward)
(define-key leader (kbd "d b") 'delete-line-backward)
(define-key leader (kbd "d p") 'delete-paragraph)
(define-key leader (kbd "d w") 'delete-word)

(define-key leader (kbd "c r") 'kill-ring-save)
(define-key leader (kbd "c l") 'copy-line)
(define-key leader (kbd "c p") 'copy-paragraph)
(define-key leader (kbd "c f") 'copy-line-forward)
(define-key leader (kbd "c b") 'copy-line-backward)
(define-key leader (kbd "c w") 'copy-word)

(define-key leader (kbd "y") 'yank)

(define-key leader (kbd "s a") 'org-save-all-org-buffers)
(define-key leader (kbd "s s") 'save-buffer)

(define-key leader (kbd "t t") 'counsel-load-theme)
(define-key leader (kbd "t r") 'rand-theme)

(defun select-current-line ()
  "Select the current line."
  (interactive)
  (beginning-of-line) ; move to the beginning of the line
  (set-mark-command nil) ; set the mark here
  (end-of-line)) ; move to the end of the line

(defun select-buffer ()
  "Select the whole buffer."
  (interactive)
  (beginning-of-buffer) ; move to the beginning of the buffer
  (set-mark-command nil) ; set the mark here
  (end-of-buffer)) ; move to the end of the buffer

(defun select-paragraph ()
  "Select the whole paragraph."
  (interactive)
  (backward-paragraph) ; move to the beginning of the paragraph
  (set-mark-command nil) ; set the mark here
  (forward-paragraph)) ; move to the end of the paragraph
(defun select-word ()
  "Select the whole word under the point."
  (interactive)
  (backward-word) ; move to the beginning of the word
  (set-mark-command nil) ; set the mark here
  (forward-word)) ; move to the end of the word

(defun select-line-backward ()
  "Select everything on the line before the point"
  (interactive)
  (set-mark-command nil) ; set the mark here
  (move-beginning-of-line nil)) ; move to the end of the line

(defun select-line-forward ()
  "Select everything on the line after the point"
  (interactive)
  (set-mark-command nil) ; set the mark here
  (end-of-line)) ; move to the end of the line

(defun kill-to-start-of-line ()
  "Kill from the current position to the start of the line."
  (interactive)
  (kill-line 0)) ; 0 as argument to kill-line kills text before the cursor

(defun copy-line ()
  "Copy the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (kill-ring-save begin end)))

(defun copy-paragraph ()
  "Copy the current paragraph."
  (interactive)
  (let ((start (progn (backward-paragraph) (point)))
        (end (progn (forward-paragraph) (point))))
    (kill-ring-save start end)))

(defun copy-line-forward ()
  "Copy the line from the point backward."
  (interactive)
  (let ((begin (point))
        (end (line-end-position)))
    (kill-ring-save begin end)))

(defun copy-line-backward ()
  "Copy the line from the point onward"
  (interactive)
  (let ((begin (point))
        (end (line-beginning-position)))
    (kill-ring-save begin end)))

(defun copy-word ()
  "Copies the word under the point."
  (interactive)
  (let ((start (progn (backward-word) (point)))
        (end (progn (forward-word) (point))))
    (kill-ring-save start end)))

(defun kill-whole-paragraph ()
  "Kill the current paragraph."
  (interactive)
  (let ((start (progn (backward-paragraph) (point)))
        (end (progn (forward-paragraph) (point))))
    (kill-region start end)))

(defun delete-paragraph ()
  "Delete the current paragraph."
  (interactive)
  (let ((start (progn (backward-paragraph) (point)))
        (end (progn (forward-paragraph) (point))))
    (delete-region start end)))

(defun delete-line ()
  "Delete the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (delete-region begin end)))

(defun delete-line-forward ()
  "Delete the current line."
  (interactive)
  (let ((begin (point))
        (end (line-end-position)))
    (delete-region begin end)))

(defun delete-line-backward ()
  "Delete the current line."
  (interactive)
  (let ((begin (point))
        (end (line-beginning-position)))
    (delete-region begin end)))

(defun delete-word ()
  "Delete the whole word under the point."
  (interactive)
  (let ((start (progn (backward-word) (point)))
        (end (progn (forward-word) (point))))
    (delete-region start end)))

(defun insert-new-line-below ()
  "Insert a new line below the current line and move the cursor to that line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun kill-current-buffer-and-window ()
  "Kill the current buffer and close the window it is displayed in."
  (interactive)
  (let ((current-buffer (current-buffer))
        (current-window (selected-window)))
    (kill-buffer current-buffer)
    ;; If there's more than one window, delete the current window.
    (when (> (length (window-list)) 1)
      (delete-window current-window))))

;; Make sure everything is utf-8
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Actually set the fonts
(set-face-attribute 'default nil
		    :font "Victor Mono Nerd Font"
		    :height 100
		    :weight 'medium)

(set-face-attribute 'variable-pitch nil
		    :font "Victor Mono Nerd Font"
		    :height 120
		    :weight 'medium)

(set-face-attribute 'fixed-pitch nil
		    :font "Victor Mono Nerd Font"
		    :height 100
		    :weight 'medium)

(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'italic)

;; and to make sure client windows open with these fonts
(add-to-list 'default-frame-alist '(font . "Victor Mono Nerd Font"))

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;;(doom-themes-neotree-config)
  ;; or for treemacs| users
  (setq doom-themes-treemacs-theme "doom-colors") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ef-themes
  :straight t)

(use-package catppuccin-theme
  :straight t
  :config
  )

;;(load-theme 'modus-operandi t)
(load-theme 'catppuccin t)

(use-package rand-theme
  :straight t)
(setq rand-theme-unwanted '(tango light-blue))

;; In this house, we use shortcuts damnit!!!

;; Get rid of pesky GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(defun my/disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))
(add-hook 'after-make-frame-functions 'my/disable-scroll-bars)
(setq default-frame-alist '((undecorated . t)))

;; Some nice transparency
(add-to-list 'default-frame-alist '(alpha-background . 95))

;; Margin Adjust
(setq left-margin-width 3)
(setq right-margin-width 3)

(setq-default cursor-type 'bar)

(use-package doom-modeline
  :straight t
  :config
  (doom-modeline-mode))

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; clean up the mode-line
(use-package diminish
  :straight t)

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(setq fill-column 79)
(setq-default display-fill-column-indicator-column 79)
;;(global-whitespace-mode)
(setq whitespace-line-column 100)
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) ; normal space, ·
        (newline-mark 10 [10]) ; newline
        (tab-mark 9 [9655 9] [92 9]) ; tab, ▷
        ))

(setq whitespace-space-regexp "\\(\t+\\| +\\)[^#]")

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
;;  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guide-method 'character)
  (setq highlight-indent-guides-auto-odd-face-perc 50)
  (setq highlight-indent-guides-auto-even-face-perc 75)
  (setq highlight-indent-guides-auto-character-face-perc 50))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(delete-selection-mode 1)

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
   "Create parent directory if not exists while visiting file."
   (unless (file-exists-p filename)
     (let ((dir (file-name-directory filename)))
       (unless (file-exists-p dir)
         (make-directory dir t)))))

(setq confirm-kill-processes nil)

(setq org-confirm-babel-evaluate nil)

(setq window-divider-default-right-width 3)   ;; vertical divider width
(setq window-divider-default-bottom-width 3)  ;; horizontal divider width
;;(setq window-divider-default-places 'right-only) ;; or 'bottom-only or 'right-and-bottom
(window-divider-mode 1)

(use-package centered-cursor-mode
  :straight t
  :hook
  (prog-mode . centered-cursor-mode)
;;  (python-ts-mode . centered-cursor-mode)
  :config
  (setq ccm-recenter-at-end-of-file t)
  (global-centered-cursor-mode)
  )

;; Highlight the line on the point to make it clearer where t he point is.
(global-hl-line-mode)

(use-package nerd-icons
  :straight t)

(use-package dashboard
  :straight t
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "woah what how did he get here")
  ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-startup-banner "~/.config/emacs/wohhowdidhegethere/toby.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((projects . 5)
			  (bookmarks . 10)
			  (recents . 10)))
  :custom
  (dashboard-modify-heading-icons '((recents . "file-text")
				    ))
  :config
  (dashboard-setup-startup-hook)
  )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
(setq dashboard-center-content t)
(setq dashboard-projects-backend 'projectile)

(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

(use-package no-littering)

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
(setq org-return-follows-link t)

(use-package counsel
  :straight t
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :straight t
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :diminish
  :config
  (ivy-mode)) ; ivy-mode can be a pain)

(use-package all-the-icons-ivy-rich
  :straight t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :straight t
  :after ivy
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
  			       'ivy-rich-switch-buffer-transformer))


(setq ivy-initial-inputs-alist
      '((counsel-M-x . "")
  	;; other commands can be added here
  	))

;;(use-package flyspell-correct-ivy
;;  :straight t
;;  :after flyspell
;;  :bind (:map flyspell-mode-map
;;              ("ESC a a" . flyspell-correct-wrapper)))

(use-package multiple-cursors
  :straight t)
(global-set-key (kbd "<escape> <escape>") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "M-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-<prior>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<next>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<prior>") 'mc/cycle-backward)
(global-set-key (kbd "C-<next>") 'mc/cycle-forward)

(use-package move-text
  :straight t
  :config
  (global-set-key (kbd "M-<up>") 'move-text-up)
  (global-set-key (kbd "M-<down>") 'move-text-down)
  )

(use-package perfect-margin
  :straight t
  :hook
  (poly-markdown-mode . perfect-margin-mode)
  )

(use-package pythonic
  :straight t
  :config
  )

(setq python-shell-completion-native-enable nil)

(use-package elpy
  :straight t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (pyvenv-activate "~/Projects/JAX-IDEM/.venv")))
  (add-to-list 'elpy-modules 'elpy-module-folding)
  (define-key leader (kbd "t f") 'elpy-folding-toggle-at-point)
  :init
  (elpy-enable)
;;  (setq elpy-rpc-virtualenv-path "~/Projects/JAX-IDEM/.venv")
)

(with-eval-after-load 'elpy
  (add-to-list 'elpy-modules 'elpy-module-folding)
  ;; Remove Elpy's overrides
  (define-key elpy-mode-map (kbd "C-<up>")   nil)
  (define-key elpy-mode-map (kbd "C-<down>") nil)
)

(defun my/python-start-folded ()
  "Enable hideshow and fold all blocks when entering python-mode."
  (hs-minor-mode 1)
  (hs-hide-all))

(add-hook 'python-mode-hook #'my/python-start-folded)

;;(setq python-shell-interpreter "ipython"
;;      python-shell-interpreter-args "-i --simple-prompt")

(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

(use-package ruff-format
  :straight t
  :config
  (add-hook 'python-mode-hook 'ruff-format-on-save-mode)
  )

(use-package quarto-mode
  :straight t
  :mode (("\\.Rmd" . poly-quarto-mode))
  :config
  )

(setq markdown-enable-math t)

(use-package ess
  :straight t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

(setq org-babel-R-command "./docket-r.sh")

;; (define-key leader (kbd "r l") 'ess-eval-line)
;; (define-key leader (kbd "r r") 'ess-eval-region)

(setq ess-ask-for-ess-directory nil)
(setq ess-startup-directory nil)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(electric-pair-mode 1)

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (elisp-mode . rainbow-delimiters-mode)
  (latex-mode . rainbow-delimiters-mode)
  )

(use-package sudo-edit)

(use-package swiper
  :straight t
  :config
  (define-key leader (kbd "f") 'swiper)
  )

(tab-bar-mode)

(global-set-key (kbd "C-<tab>") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "C-S-<iso-lefttab>") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "C-t") 'tab-bar-new-tab)
(global-set-key (kbd "C-q") 'tab-bar-close-tab)

(use-package vimish-fold
  :straight t
  :config
  (vimish-fold-global-mode 1)
  (define-key leader (kbd "v f") #'vimish-fold)
  (define-key leader (kbd "v v") #'vimish-fold-delete)
  )

(use-package windmove
  :straight t
  :config
  )

(use-package yaml-mode
:straight t
:config
)

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode 1)
;; :hook
;;  (org-mode . yas-minor-mode)
  )

(use-package yascroll
  :straight t
  :config
  (global-yascroll-bar-mode)
  )
