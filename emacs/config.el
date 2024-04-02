  ;; Quickly reload this file after making edits. Refers to a function defined under the tab 'neat-tricks'
  (global-set-key (kbd "C-c r") 'reload-init-file)

  ;; Quickly get to this file
  (global-set-key (kbd "C-c C-<return>") 'go-to-config)

  ;; Rearrange the buffers
  (global-set-key (kbd "C-M-<up>") 'buf-move-up)
  (global-set-key (kbd "C-M-<down>") 'buf-move-down)
  (global-set-key (kbd "C-M-<left>") 'buf-move-left)
  (global-set-key (kbd "C-M-<right>") 'buf-move-right)

  ;; Move focus
  (global-set-key (kbd "C-M-<up>") 'windmove-up)
  (global-set-key (kbd "C-M-<down>") 'windmove-down)
  (global-set-key (kbd "C-M-<left>") 'windmove-left)
  (global-set-key (kbd "C-M-<right>") 'windmove-right)

  ;; Todo
  (global-set-key (kbd "C-c t") 'org-toggle-item)
  (global-set-key (kbd "C-c d") 'org-todo)

  ;; Return to dashboard
  (global-set-key (kbd "C-c <return>") 'dashboard-open)

  ;; Org-Agenda Shortcut
  (global-set-key (kbd "C-c o") 'org-agenda)

(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))

;; make the menu key as leader key
(global-set-key (kbd "<menu>") 'my-leader-key-map)(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))

(define-prefix-command 'my-leader-key-map)

  (define-key my-leader-key-map (kbd "RET") 'insert-new-line-below)
  (define-key my-leader-key-map (kbd "<left>") 'org-beginning-of-line)
  (define-key my-leader-key-map (kbd "<right>") 'org-end-of-line)
  (global-set-key (kbd "<next>") 'next-buffer)
  (global-set-key (kbd "<prior>") 'previous-buffer)
  (define-key my-leader-key-map (kbd "<up>") 'beginning-of-buffer)
  (define-key my-leader-key-map (kbd "<down>") 'end-of-buffer)

  (define-key my-leader-key-map (kbd "=") 'jump-lines)
  (define-key my-leader-key-map (kbd "-") 'jump-lines-back)

  (define-key my-leader-key-map (kbd "b") 'ibuffer-list-buffers)

(define-key my-leader-key-map (kbd "0") 'delete-window)
(define-key my-leader-key-map (kbd "1") 'delete-other-windows)
(define-key my-leader-key-map (kbd "2") 'split-window-below)
(define-key my-leader-key-map (kbd "3") 'split-window-right)
(define-key my-leader-key-map (kbd "DEL") 'kill-current-buffer-and-window)

(define-key my-leader-key-map (kbd "i l") 'org-insert-link)
(define-key my-leader-key-map (kbd "i c") 'org-cite-insert)
(define-key my-leader-key-map (kbd "i i") 'org-insert-image)
(define-key my-leader-key-map (kbd "i a") 'insert-char)

;;org keys
(define-key my-leader-key-map (kbd "o t") 'org-toggle-item)
(define-key my-leader-key-map (kbd "o l") 'org-open-at-point)
(define-key my-leader-key-map (kbd "o s") 'set-org-latex-scale)
(define-key my-leader-key-map (kbd "c '") 'org-edit-special)
(define-key org-src-mode-map (kbd "C-c '") nil) ; unbind the original key
(define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit) ; bind to your key

;;org roam keys
(define-key my-leader-key-map (kbd "n i") 'org-roam-node-insert)
(define-key my-leader-key-map (kbd "n f") 'org-roam-node-find)
(define-key my-leader-key-map (kbd "n u") 'org-roam-ui-mode)
(define-key my-leader-key-map (kbd "n #") 'org-roam-tag-add)
(define-key my-leader-key-map (kbd "n c") 'org-roam-add-citation)
(define-key my-leader-key-map (kbd "n h") 'org-is-get-create)

  (define-key my-leader-key-map (kbd "g c") 'go-to-config)
  (define-key my-leader-key-map (kbd "g d") 'dashboard-open)
  (define-key my-leader-key-map (kbd "g a") 'org-agenda)
  (define-key my-leader-key-map (kbd "x f") 'find-file)

  (define-key my-leader-key-map (kbd "r c") 'reload-init-file)
  (define-key my-leader-key-map (kbd "r b") 'org-babel-execute-buffer)
  (define-key my-leader-key-map (kbd "r l") 'org-latex-refresh)

;;selections ('m' is for mark, 's' is taken by 'save')
(define-key my-leader-key-map (kbd "m l") 'select-current-line)
(define-key my-leader-key-map (kbd "m a") 'select-buffer)
(define-key my-leader-key-map (kbd "m p") 'select-paragraph)
(define-key my-leader-key-map (kbd "m w") 'select-word)
(define-key my-leader-key-map (kbd "m m w") 'mc/mark-all-words-like-this)
(define-key my-leader-key-map (kbd "m m a") 'mc/mark-all-like-this)

;;murder
(define-key my-leader-key-map (kbd "k l") 'kill-whole-line)
(define-key my-leader-key-map (kbd "k f") 'kill-line)
(define-key my-leader-key-map (kbd "k b") 'kill-to-start-of-line)
(define-key my-leader-key-map (kbd "k r") 'kill-region)
(define-key my-leader-key-map (kbd "k p") 'kill-whole-paragraph)
(define-key my-leader-key-map (kbd "k RET") 'save-buffers-kill-terminal)

;;irrevocably murder
(define-key my-leader-key-map (kbd "d r") 'delete-region)
(define-key my-leader-key-map (kbd "d l") 'delete-line)
(define-key my-leader-key-map (kbd "d f") 'delete-line-forward)
(define-key my-leader-key-map (kbd "d b") 'delete-line-backward)
(define-key my-leader-key-map (kbd "d p") 'delete-paragraph)

;;copy
(define-key my-leader-key-map (kbd "c r") 'kill-ring-save)
(define-key my-leader-key-map (kbd "c l") 'copy-line)
(define-key my-leader-key-map (kbd "c p") 'copy-paragraph)

;;yank
(define-key my-leader-key-map (kbd "y") 'yank)

;;save
(define-key my-leader-key-map (kbd "s a") 'org-save-all-org-buffers)
(define-key my-leader-key-map (kbd "s s") 'save-buffer)

(define-key my-leader-key-map (kbd "t l") 'global-display-line-numbers-mode)

  (define-key my-leader-key-map (kbd "f") 'swiper)
  (define-key my-leader-key-map (kbd "C-f") 'swiper-backward)

(define-key my-leader-key-map (kbd "#") 'flyspell-correct-word-before-point)

(define-key my-leader-key-map (kbd "/") 'vterm)

(define-key my-leader-key-map (kbd "?") 'chatgpt-shell)

(define-key my-leader-key-map (kbd "t t") 'counsel-load-theme)
(define-key my-leader-key-map (kbd "t r") 'rand-theme)

;; make the menu key as leader key
(global-set-key (kbd "<menu>") 'my-leader-key-map)

  (defun reload-init-file ()
    (interactive) ;; (interactive allows you to call the function with M-x
    (load-file user-init-file)
    (load-file user-init-file)
    (previous-buffer))

  (defun go-to-config ()
    (interactive)
    (find-file "~/.config/emacs/config.org"))

  (require 'windmove)

  ;;;###autoload
  (defun buf-move-up ()
    "Swap the current buffer and the buffer above the split.
  If there is no split, ie now window above the current one, an
  error is signaled."
  ;;  "Switches between the current buffer, and the buffer above the
  ;;  split, if possible."
    (interactive)
    (let* ((other-win (windmove-find-other-window 'up))
           (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No window above this one")
        ;; swap top with this one
        (set-window-buffer (selected-window) (window-buffer other-win))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  ;;;###autoload
  (defun buf-move-down ()
  "Swap the current buffer and the buffer under the split.
  If there is no split, ie now window under the current one, an
  error is signaled."
    (interactive)
    (let* ((other-win (windmove-find-other-window 'down))
           (buf-this-buf (window-buffer (selected-window))))
      (if (or (null other-win) 
              (string-match "^ \\*Minibuf" (buffer-name (window-buffer other-win))))
          (error "No window under this one")
        ;; swap top with this one
        (set-window-buffer (selected-window) (window-buffer other-win))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  ;;;###autoload
  (defun buf-move-left ()
  "Swap the current buffer and the buffer on the left of the split.
  If there is no split, ie now window on the left of the current
  one, an error is signaled."
    (interactive)
    (let* ((other-win (windmove-find-other-window 'left))
           (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No left split")
        ;; swap top with this one
        (set-window-buffer (selected-window) (window-buffer other-win))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

  ;;;###autoload
  (defun buf-move-right ()
  "Swap the current buffer and the buffer on the right of the split.
  If there is no split, ie now window on the right of the current
  one, an error is signaled."
    (interactive)
    (let* ((other-win (windmove-find-other-window 'right))
           (buf-this-buf (window-buffer (selected-window))))
      (if (null other-win)
          (error "No right split")
        ;; swap top with this one
        (set-window-buffer (selected-window) (window-buffer other-win))
        ;; move this one to top
        (set-window-buffer other-win buf-this-buf)
        (select-window other-win))))

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
  (backward-paragraph) ; move to the beginning of the buffer
  (set-mark-command nil) ; set the mark here
  (forward-paragraph)) ; move to the end of the buffer

(defun select-word ()
  "Select the whole word under the point."
  (interactive)
  (backward-word) ; move to the beginning of the buffer
  (set-mark-command nil) ; set the mark here
  (forward-word)) ; move to the end of the buffer

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

  (defun insert-new-line-below ()
    "Insert a new line below the current line and move the cursor to that line."
    (interactive)
    (end-of-line)
    (newline-and-indent))

  (defun enclose-in-yas-snippet (start end)
    "Enclose the selected region within a YASnippet."
    (interactive "r")
    (let ((region (buffer-substring start end)))
      (delete-region start end)
      (insert (concat "${1:" region "}$0"))))

  (defun org-roam-add-citation ()
    (interactive)
    (let ((filename "~/RoamNotes/Bibliography.bib")
          (text (read-string "Citation to append:")))
      (with-temp-buffer
        (insert "\n")
        (insert text)
        (insert "\n")
        (append-to-file (point-min) (point-max) filename))))

  (defun org-insert-image ()
    (interactive)
      (let* ((path (read-file-name "Enter image path: "))
             (caption (read-string "Enter caption: "))
             (name (read-string "Enter name: ")))
        (insert (format "#+CAPTION: %s\n#+NAME: fig:%s\n[[file:%s]]" caption name path))))

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
		      :font "VictorMonoNerdFont"
		      :height 165
		      :weight 'medium)

  (set-face-attribute 'variable-pitch nil
		      :font "Ubuntu"
		      :height 180
		      :weight 'medium)
  
  (set-face-attribute 'fixed-pitch nil
		       :font "JetBrains Mono"
		       :height 165
		       :weight 'medium)

  (set-face-attribute 'font-lock-comment-face nil
		      :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
			:slant 'italic)

  ;; and to make sure client windows open with these fonts
  (add-to-list 'default-frame-alist '(font . "VictorMonoNerdFont"))

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
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ef-themes
  :straight t)

(load-theme 'modus-operandi t)

(use-package rand-theme
  :straight t)
(setq rand-theme-unwanted '(tango light-blue))

  ;; In this house, we use shortcuts damnit!!!'

  ;; Get rid of pesky GUI elements
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;;(setq default-frame-alist '((undecorated . t)))

  ;; Some nice transparency
  (add-to-list 'default-frame-alist '(alpha-background . 100))

  ;; Make the modeline pretty
  ;;(use-package solaire-mode
  ;;  :config (solaire-global-mode))

  ;; or use doom-modeline
  (use-package doom-modeline
    :straight t
    :config
    (doom-modeline-mode))

  ;; not sure where to put this lol
  (delete-selection-mode 1)

  ;; Margin Adjust
  (setq left-margin-width 3)
  (setq right-margin-width 3)

(setq org-agenda-files
      '("~/RoamNotes"))

  (setq org-agenda-custom-commands
        '(("v" "PhD Tasks"
           ((tags "general"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "General Statistics Tasks")))
            (tags "org"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Tasks relating to org and the config file")))
            (tags "reading"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Tasks relating to the reading list")))
            ))))

(use-package centered-cursor-mode
  :straight t)

;;(defun previous-line-and-recenter ()
;;  "move to the previous line and recenter"
;;  (interactive)
;;  (previous-line)
;;  (recenter))
;;
;;(defun next-line-and-recenter ()
;;  "move to the next line and recenter"
;;  (interactive)
;;  (next-line)
;;  (recenter))
;;
;;(global-set-key [wheel-right] 'forward-char)
;;(global-set-key [wheel-left] 'backward-char)
;;
;;(setq scroll-preserve-screen-postion 1)
;;
;;(define-minor-mode scroll-remap-mode
;;  "Remap mouse scroll wheel to next-line and previous-line."
;;  :local t
;;  :lighter " Scroll-Remap"
;;  (if scroll-remap-mode
;;      (progn
;;        (global-set-key (kbd "<mouse-4>") 'next-line-and-recenter)
;;        (global-set-key (kbd "<mouse-5>") 'previous-line-and-recenter)
;;        (global-set-key (kbd "<triple-wheel-down>") 'next-line-and-recenter)
;;        (global-set-key (kbd "<triple-wheel-up>") 'previous-line-and-recenter)
;;        (global-set-key (kbd "C-n") 'next-line-and-recenter)
;;        (global-set-key (kbd "C-p") 'previous-line-and-recenter)
;;        (global-set-key (kbd "<down>") 'next-line-and-recenter)
;;        (global-set-key (kbd "<up>") 'previous-line-and-recenter))
;;    ;; Reset to default scrolling behavior
;;    (global-set-key (kbd "<mouse-4>") 'scroll-down-command)
;;    (global-set-key (kbd "<mouse-5>") 'scroll-up-command)
;;    (global-set-key (kbd "<triple-wheel-down>") 'scroll-down-command)
;;    (global-set-key (kbd "<triple-wheel-up>") 'scroll-up-command)
;;    (global-set-key (kbd "C-n") 'next-line)
;;    (global-set-key (kbd "C-p") 'previous-line)
;;    (global-set-key (kbd "<down>") 'next-line)
;;    (global-set-key (kbd "<up>") 'previous-line)))

 (defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
    "Create parent directory if not exists while visiting file."
    (unless (file-exists-p filename)
      (let ((dir (file-name-directory filename)))
        (unless (file-exists-p dir)
          (make-directory dir t)))))

(setq confirm-kill-processes nil)

(defun smart-forward ()
  "Move the cursor forward depending on the context:
   - If there is a bracket, move forward by one character.
   - Otherwise, move forward by one word.
   - I will add more conditions as i come up with them
  "
  (interactive)
  (let ((char (char-after)))
    (cond
     ;; Check if the character is an opening or closing bracket
     ((or (eq char ?\() (eq char ?\)))
      (forward-char))
     (t
      (cond
       ;;check if we are at the end of a line
       ((= (point) (line-end-position))
	;; if we are at the end of a line, go to the start of the next
	(next-line)
	(beginning-of-line))
       ;; if we aren't, try moving forward or moving the the end of the line
       (t
	(let ((current-line (line-number-at-pos)))	  
	  (forward-word)
	  ;; check wether this has moved us onto a new line
	  (while (> (line-number-at-pos) current-line)
	    ;; if it has, keep going back until we are on the old line, then move to the end of that line
	    (previous-line)
	    (end-of-line)
	    (end-of-line)))))))))

(defun my-programming-tab ()
  "Attempts to indent the current line. If the indentation does not change,
   moves the cursor forward by one word."
  (interactive)
  (let ((start-point (point)))
    ;; Attempt to indent the current line
    (org-cycle)
    ;; Check if the cursor position has changed
    (when (= (point) start-point)
      ;; If indentation did not change, move forward by one word
      (smart-forward))))

(defun my/org-tab-behavior ()
  "Custom TAB behavior for Org mode:
- Use `cdlatex` behavior in LaTeX fragments.
- Do not interfere with source block indentation.
- Cycle visibility for headings and drawers outside LaTeX fragments.
- Expand yasnippet at point if possible and not in a LaTeX fragment.
- Otherwise, move forward to the next word but only if not at a heading, and not in a LaTeX fragment."
  (interactive)
  (cond
   ;; If inside a LaTeX fragment, defer to cdlatex
   ((and (derived-mode-p 'org-mode) (org-inside-LaTeX-fragment-p))
    (cdlatex-tab))
   
   ;; If inside a source block, use the major mode's default TAB behavior
   ((org-in-src-block-p)
    (my-programming-tab))
   
   ;; Check if we can expand a yasnippet; if yes, do it and prevent further action
   ((yas-expand)
    nil)
   
   ;; If at a heading or at a drawable structure, cycle visibility and prevent further action
   ((or (org-at-heading-p) (org-at-drawer-p))
    (org-cycle))

   ;; Default action: move forward to the next word
   (t (smart-forward))))

(with-eval-after-load 'org
  ;; Bind the custom function to TAB in Org mode.
  ;; Make sure this doesn't conflict with other keybindings you might have.
  (define-key org-mode-map (kbd "TAB") #'my/org-tab-behavior))

(use-package shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("shell-maker.el")))

(use-package chatgpt-shell
  :requires shell-maker
  :straight (:host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell.el")))

(setq chatgpt-shell-openai-key "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")

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
	(setq dashboard-items '((recents . 20)
				(bookmarks . 10)))
	:custom
	(dashboard-modify-heading-icons '((recents . "file-text")
					  ))
	:config
	(dashboard-setup-startup-hook)
	)

    (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
    (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
    (setq dashboard-center-content t)

    (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

    (use-package beacon
      :straight t
      :config (beacon-mode))

(setq-default cursor-type 'bar)

;;(setq display-line-numbers 'relative)
;;(global-display-line-numbers-mode)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

      ;; clean up the mode-line
      (use-package diminish
	:straight t)

(use-package htmlize
  :straight t)

  (use-package quarto-mode
    :straight t
    :mode (("\\.Rmd" . poly-quarto-mode))
    )
  (setq markdown-enable-math t)

  (use-package haskell-mode
    :straight t)

    (use-package auctex
      :defer t
      :straight t)
    (setq org-highlight-latex-and-related '(native))

    (use-package cdlatex
      :straight t)
    (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
    (add-hook 'latex-mode-hook 'turn-on-cdlatex)
    (add-hook 'org-mode-hook #'turn-on-org-cdlatex)

    ;; Line below currently breaks things
    ;; (add-hook 'after-save-hook #'org-latex-export-to-pdf)

(setq org-latex-pdf-process
  '("lualatex -shell-escape -interaction nonstopmode %f"
    "lualatex -shell-escape -interaction nonstopmode %f")) 

(setq luamagick '(luamagick :programs ("lualatex" "convert")
       :description "pdf > png"
       :message "you need to install lualatex and imagemagick."
       :use-xcolor t
       :image-input-type "pdf"
       :image-output-type "png"
       :image-size-adjust (1.0 . 1.0)
       :latex-compiler ("lualatex -interaction nonstopmode -output-directory %o %f")
       :image-converter ("convert -density %D -trim -antialias %f -quality 100 %O")))

;;(add-to-list 'org-preview-latex-process-alist luamagick)

;;(setq org-preview-latex-default-process 'luamagick)

    (use-package ess
      :straight t)

  ;; Enable scala-mode for highlighting, indentation and motion commands
  (use-package scala-mode
    :straight t
    :interpreter ("scala" . scala-mode))

  ;; Enable sbt mode for executing sbt commands
  (use-package sbt-mode
    :straight t
    :commands sbt-start sbt-command
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
     ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
     (setq sbt:program-options '("-Dsbt.supershell=false")))

  ;; Enable nice rendering of diagnostics like compile errors.
  (use-package flycheck
    :straight t
    :diminish
    :init (global-flycheck-mode))

  (use-package lsp-mode
    :straight t
    :diminish
    ;; Optional - enable lsp-mode automatically in scala files
    ;; You could also swap out lsp for lsp-deffered in order to defer loading
    :hook  (scala-mode . lsp)
	   (lsp-mode . lsp-lens-mode)
    :config
    ;; Uncomment following section if you would like to tune lsp-mode performance according to
    ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
    ;; (setq gc-cons-threshold 100000000) ;; 100mb
    ;; (setq read-process-output-max (* 1024 1024)) ;; 1mb
    ;; (setq lsp-idle-delay 0.500)
    ;; (setq lsp-log-io nil)
    ;; (setq lsp-completion-provider :capf)
    (setq lsp-prefer-flymake nil)
    ;; Makes LSP shutdown the metals server when all buffers in the project are closed.
    ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/#lsp-keep-workspace-alive
    (setq lsp-keep-workspace-alive nil))

  ;; Add metals backend for lsp-mode
  (use-package lsp-metals
    :straight t)

  ;; Enable nice rendering of documentation on hover
  ;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
  ;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
  ;;   In that case you have to not only disable this but also remove from the packages since
  ;;   lsp-mode can activate it automatically.
  (use-package lsp-ui
    :straight t)

  ;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
  ;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
  ;; to avoid odd behavior with snippets and indentation

  ;; Use company-capf as a completion provider.
  ;;
  ;; To Company-lsp users:
  ;;   Company-lsp is no longer maintained and has been removed from MELPA.
  ;;   Please migrate to company-capf.
  (use-package company
    :straight t
    :diminish
    :hook (prog-mode . company-mode)
	  (prog-mode . (lambda () (setq display-line-numbers 'absolute)))
	  (prog-mode . display-line-numbers-mode)
	  (org-mode . company-mode)
    :config
    (setq lsp-completion-provider :capf))

  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  (use-package posframe
    :straight t)

  ;; Use the Debug Adapter Protocol for running tests and debugging
  (use-package dap-mode
    :straight t
    :hook
    (lsp-mode . dap-mode)
    (lsp-mode . dap-ui-mode))

  (org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (R . t)
      (latex . t)
      (haskell . t)
      (python . t)
     )
  )

  ;; disable the confirmation message
  (setq org-confirm-babel-evaluate nil)

(use-package multiple-cursors
  :straight t)
(global-set-key (kbd "<menu> <menu>") 'mc/edit-lines)
(global-unset-key (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "M-SPC") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-<prior>") 'mc/mark-previous-like-this)
(global-set-key (kbd "M-<next>") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<prior>") 'mc/cycle-backward)
(global-set-key (kbd "C-<next>") 'mc/cycle-forward)

(use-package no-littering)

  (add-hook 'org-mode-hook 'org-indent-mode)

;;(setq org-image-actual-width t) ;; Sets the width of image previewq in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'abbrev-mode)

;;(use-package org
;;  :straight `(org
;;              :fork (:host nil
;;                     :repo "https://git.tecosaur.net/tec/org-mode.git"
;;                     :branch "dev"
;;                     :remote "tecosaur")
;;              :files (:defaults "etc")
;;              :build t
;;              :pre-build
;;              (with-temp-file "org-version.el"
;;               (require 'lisp-mnt)
;;               (let ((version
;;                      (with-temp-buffer
;;                        (insert-file-contents "lisp/org.el")
;;                        (lm-header "version")))
;;                     (git-version
;;                      (string-trim
;;                       (with-temp-buffer
;;                         (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
;;                         (buffer-string)))))
;;                (insert
;;                 (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
;;                 (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
;;                 "(provide 'org-version)\n")))
;;              :pin nil))

(use-package org-download
  :straight t
)

  (use-package org-modern
    :straight t
    :hook
    (org-mode . org-modern-mode)
    :config
    (global-org-modern-mode))

(use-package org-trello
  :straight t)

(custom-set-variables '(org-trello-files '("~/orgfiles/phd_tasks.org")))

(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-return-follows-link t)

(use-package org-roam
  :straight t
  :custom
  (org-roam-directory (file-truename "~/RoamNotes"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
         ("C-c n u" . org-roam-ui-open)
         ("C-c n m" . org-roam-ui-mode))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; For completeion everywhere
  (setq org-roam-completion-everywhere t)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))

  (use-package org-roam-ui
    :after org-roam
    :straight t
    :config
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save t
	  org-roam-ui-open-on-start t))

  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

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
    (ivy-mode))

  ;;(use-package all-the-icons-ivy-rich
    ;;:straight t
    ;;:init (all-the-icons-ivy-rich-mode 1))

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

(use-package ox-pandoc
  :straight t)

(use-package perfect-margin
  :straight t
  :hook
  (org-mode . perfect-margin-mode))

(use-package popper
  :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (setq popper-reference-buffers
      (append popper-reference-buffers
              '("^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
                "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
                "^\\*term.*\\*$"   term-mode   ;term as a popup
                "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
                "^\\*chatgpt*\\*$"  chatgpt-shell-mode  ;vterm as a popup
                "example"  ess-r-mode  ;vterm as a popup
                )))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setq prettify-symbols-alist
      '(
        ("lambda" . 955) ; λ
        ("->" . 8594)    ; →
        ("=>" . 8658)    ; ⇒
        ("map" . 8614)    ; ↦
        ))

(global-prettify-symbols-mode 1)

(use-package vterm
  :straight t)

  (use-package smartparens-mode
    :straight smartparens  ;; install the package
    :hook (prog-mode text-mode markdown-mode org-mode inferior-ess-mode) ;; add `smartparens-mode` to these hooks
    :config
    ;; load default config
    (require 'smartparens-config)
    (sp-pair "$" "$")
  )

  (use-package rainbow-delimiters
    :straight t
    :hook
    (prog-mode . rainbow-delimiters-mode)
    (elisp-mode . rainbow-delimiters-mode)
    (latex-mode . rainbow-delimiters-mode)
)

(use-package smooth-scroll
  :straight t
  :config
  (pixel-scroll-precision-mode)
  (smooth-scroll-mode)
)

  (use-package sudo-edit)

  (use-package which-key
    :straight t
    :init
    (which-key-mode 1)
    :diminish
    :config
    (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order-alpha
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 56
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-lenght 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-seperator "➢"))

  (use-package yasnippet
    :straight t
    :config
    (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
    (yas-global-mode 1)
    :hook
    (org-mode . yas-minor-mode)
  )

(defun my-yas-org-fold-drawer-after-insert ()
  "Fold drawer just inserted by a yasnippet in org-mode."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-cycle)))) ;; Fold the drawer.

(add-hook 'yas-after-exit-snippet-hook #'my-yas-org-fold-drawer-after-insert)
(add-hook 'org-mode-hook 'org-fold-hide-drawer-all)
