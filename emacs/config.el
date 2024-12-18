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

;; make the esc key as leader key
(global-set-key (kbd "<escape>") 'my-leader-key-map)

(define-prefix-command 'my-leader-key-map)

(define-key my-leader-key-map (kbd "RET") 'insert-new-line-below)
(define-key my-leader-key-map (kbd "<left>") 'org-beginning-of-line)
(define-key my-leader-key-map (kbd "<right>") 'org-end-of-line)
(global-set-key (kbd "<next>") 'next-buffer)
(global-set-key (kbd "<prior>") 'previous-buffer)
(define-key my-leader-key-map (kbd "<up>") 'beginning-of-buffer)
(define-key my-leader-key-map (kbd "<down>") 'end-of-buffer)

;; currently broken
(define-key my-leader-key-map (kbd "=") 'jump-lines)
(define-key my-leader-key-map (kbd "-") 'jump-lines-back)
(define-key my-leader-key-map (kbd "g l") 'goto-line)

(define-key my-leader-key-map (kbd "b") 'ibuffer-list-buffers)

;; Vim-like motions
(define-key global-map (kbd "C-h") 'backward-char)
(define-key global-map (kbd "C-j") 'next-line)
(define-key org-mode-map (kbd "C-j") 'next-line) ; org-mode can be a pain
(define-key global-map (kbd "C-k") 'previous-line)
(define-key global-map (kbd "C-l") 'forward-char)

;; recenter
(define-key my-leader-key-map (kbd "l") 'recenter)

;; Quicker backspace
(define-key global-map (kbd "C-b") 'delete-backward-char)

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

;; Backup org-cycle
(define-key my-leader-key-map (kbd "TAB") 'org-cycle)

(define-key org-src-mode-map (kbd "C-c '") nil) ; unbind the original key
(define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit) ; bind to your key

;;org roam keys
(define-key my-leader-key-map (kbd "n i") 'org-roam-node-insert)
(define-key my-leader-key-map (kbd "n f") 'org-roam-node-find)
(define-key my-leader-key-map (kbd "n u") 'org-roam-ui-mode)
(define-key my-leader-key-map (kbd "n #") 'org-roam-tag-add)
(define-key my-leader-key-map (kbd "n c") 'org-roam-add-citation)
(define-key my-leader-key-map (kbd "n h") 'org-is-get-create)

(define-key my-leader-key-map (kbd "g c") (lambda () (interactive) (find-file "~/.config/emacs/config.org")))
(define-key my-leader-key-map (kbd "g t") (lambda () (interactive) (find-file "~/orgfiles/Tasks.org")))
(define-key my-leader-key-map (kbd "g s") (lambda () (interactive) (dired "~/.config/emacs/snippets/org-mode")))
(define-key my-leader-key-map (kbd "g d") 'dashboard-open)
(define-key my-leader-key-map (kbd "g a") 'org-agenda-execute)
(define-key my-leader-key-map (kbd "x f") 'find-file)
(define-key my-leader-key-map (kbd "p f") 'projectile-find-file)

(define-key my-leader-key-map (kbd "r c") 'reload-init-file)
(define-key my-leader-key-map (kbd "r b") 'org-babel-execute-buffer)
(define-key my-leader-key-map (kbd "r l") 'org-latex-refresh)

(define-key my-leader-key-map (kbd "h") 'help)

;;selections ('m' is for mark, 's' is taken by 'save')
(define-key my-leader-key-map (kbd "m l") 'select-current-line)
(define-key my-leader-key-map (kbd "m a") 'select-buffer)
(define-key my-leader-key-map (kbd "m p") 'select-paragraph)
(define-key my-leader-key-map (kbd "m w") 'select-word)
(define-key my-leader-key-map (kbd "m m w") 'mc/mark-all-words-like-this)
(define-key my-leader-key-map (kbd "m m a") 'mc/mark-all-like-this)
(define-key my-leader-key-map (kbd "m f") 'select-line-forward)
(define-key my-leader-key-map (kbd "m b") 'select-line-backward)

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
(define-key my-leader-key-map (kbd "d w") 'delete-word)

;;copy
(define-key my-leader-key-map (kbd "c r") 'kill-ring-save)
(define-key my-leader-key-map (kbd "c l") 'copy-line)
(define-key my-leader-key-map (kbd "c p") 'copy-paragraph)
(define-key my-leader-key-map (kbd "c f") 'copy-line-forward)
(define-key my-leader-key-map (kbd "c b") 'copy-line-backward)
(define-key my-leader-key-map (kbd "c w") 'copy-word)

;;yank
(define-key my-leader-key-map (kbd "y") 'yank)

;;save
(define-key my-leader-key-map (kbd "s a") 'org-save-all-org-buffers)
(define-key my-leader-key-map (kbd "s s") 'save-buffer)

(define-key my-leader-key-map (kbd "t l") 'global-display-line-numbers-mode)

(define-key my-leader-key-map (kbd "f") 'swiper)
(define-key my-leader-key-map (kbd "C-f") 'swiper-backward)

(define-key my-leader-key-map (kbd "a w") 'flyspell-correct-word-before-point)
(define-key my-leader-key-map (kbd "a a") 'flyspell-auto-correct-word)

(define-key my-leader-key-map (kbd "/") 'vterm)

(define-key my-leader-key-map (kbd "t t") 'counsel-load-theme)
(define-key my-leader-key-map (kbd "t r") 'rand-theme)

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (let ((region (buffer-substring start end)))
    (delete-region start end)
    (forward-line n)
    (insert region)
    (set-mark (point))
    (forward-char (- (length region)))
    (setq deactivate-mark nil)))

(defun move-region-up (start end)
  "Move the current region up by one line."
  (interactive "r")
  (move-region start end -1))

(defun move-region-down (start end)
  "Move the current region down by one line."
  (interactive "r")
  (move-region start end 1))

(defun move-region-or-line (start end n)
  "Move the current region or line up or down by N lines."
  (if (use-region-p)
      (move-region start end n)
    (let ((line-start (line-beginning-position))
          (line-end (line-end-position)))
      (move-region line-start line-end n)
      (goto-char line-start)
      (set-mark (line-end-position))
      (setq deactivate-mark nil))))

(defun move-region-or-line-up (start end)
  "Move the current region or line up by one line."
  (interactive "r")
  (move-region-or-line start end -1))

(defun move-region-or-line-down (start end)
  "Move the current region or line down by one line."
  (interactive "r")
  (move-region-or-line start end 1))

(global-set-key (kbd "C-S-<up>") 'move-region-or-line-up)
(global-set-key (kbd "C-S-<down>") 'move-region-or-line-down)
(global-set-key (kbd "C-S-<up>") 'move-line-or-region-up)
(global-set-key (kbd "C-S-<down>") 'move-line-or-region-down)
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-S-<up>") 'move-line-or-region-up)
  (define-key org-mode-map (kbd "C-S-<down>") 'move-line-or-region-down))

(define-key my-leader-key-map (kbd "r l") 'elpy-shell-send-statement)
(define-key my-leader-key-map (kbd "r b") 'elpy-shell-send-buffer)
(define-key my-leader-key-map (kbd "r r") 'elpy-shell-send-region-or-buffer)
(define-key my-leader-key-map (kbd "r f") 'elpy-autopep8-fix-code)

(define-key my-leader-key-map (kbd "r C-l") 'elpy-shell-send-statement-and-go)
(define-key my-leader-key-map (kbd "r C-b") 'elpy-shell-send-buffer-and-go)
(define-key my-leader-key-map (kbd "r C-r") 'elpy-shell-send-region-or-buffer-and-go)

(define-key my-leader-key-map (kbd "p t") 'elpy-folding-toggle-docstrings)
(define-key my-leader-key-map (kbd "p g") 'elpy-goto-definition)
(define-key my-leader-key-map (kbd "p h") 'elpy-doc)
(define-key my-leader-key-map (kbd "r t") 'pytest-run)

(use-package python-mode
  :straight t
  :config
  (define-key python-mode-map (kbd "C-c C-c") 'elpy-send-region-or-buffer-and-step)
  (define-key python-mode-map (kbd "M-<left>") 'elpy-nav-indent-shift-left)
  (define-key python-mode-map (kbd "M-<right>") 'elpy-nav-indent-shift-right)
  (define-key python-mode-map (kbd "M-<down>") 'elpy-nav-move-line-or-region-down)
  (define-key python-mode-map (kbd "M-<up>") 'elpy-nav-move-line-or-region-up))

;; make the menu key as leader key
(global-set-key (kbd "<escape>") 'my-leader-key-map)

(defun reload-init-file ()
  (interactive) ;; (interactive allows you to call the function with M-x
  (load-file user-init-file)
  (load-file user-init-file)
  (previous-buffer))

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

(defun git-commit ()
  "Prompt for a commit message, add all then commit"
  (interactive)
  (let ((commit-msg (read-string "Enter commit message: ")))
    (shell-command (format "git add . && git commit -m \"%s\"" commit-msg))))

(defun git-status ()
  "Check Git Status"
  (interactive)
  (shell-command "git status"))

(defun git-push ()
  "Check Git Status"
  (interactive)
  (shell-command "git push -u origin"))

(define-key my-leader-key-map (kbd "g h c") 'git-commit)
(define-key my-leader-key-map (kbd "g h s") 'git-status)
(define-key my-leader-key-map (kbd "g h p") 'git-push)

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
		    :font "JetBrains Mono"
		    :height 165
		    :weight 'medium)

(set-face-attribute 'variable-pitch nil
		    :font "JetBrains Mono"
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
(add-to-list 'default-frame-alist '(font . "JetBrains Mono"))

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

(load-theme 'doom-solarized-dark t)

(use-package rand-theme
  :straight t)
(setq rand-theme-unwanted '(tango light-blue))

;; In this house, we use shortcuts damnit!!!

;; Get rid of pesky GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;;(setq default-frame-alist '((undecorated . t)))

;; Some nice transparency
(add-to-list 'default-frame-alist '(alpha-background . 95))

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

(setq org-agenda-hide-tags-regexp ".*")
(setq org-agenda-prefix-format '(
				 (agenda . "%?i %?-12t %s")
				 (todo . " ")
				 (tags . "%s %?t - ")
				 (search . " ")))
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-skip-timestamp-if-done t)

(setq org-todo-keywords
      '((sequence "TODO" "DEADLINED" "IN PROGRESS" "|" "DONE" "ON HOLD")))

(setq org-agenda-files
      '("~/RoamNotes"
	"~/.config/emacs/config.org"
	"~/orgfiles/Supervisor_meetings"
	"~/orgfiles/Tasks.org"
	"~/MyProjects/Adaptive-MCMC-in-Scala-and-JAX"
	"~/MyProjects/First-Year-Report"))

(setq org-agenda-custom-commands
      '(("v" "PhD Tasks"
	 ((agenda "" ((org-agenda-span 7)))
	  (todo "DEADLINED"
		((org-agenda-overriding-header "Deadlined Assignments")))
	  (todo "IN PROGRESS"
		((org-agenda-overriding-header "Actively being worked on")))
	  ;;(tags "events"
          ;;      ((org-agenda-span 'week)
	  ;;   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
          ;;   (org-agenda-overriding-header "Upcoming Events")))
	  (tags "projects"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Project Tasks")))
          (tags "general"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "General Statistics Tasks")))
          (tags "org"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Organisational Tasks")))
          (tags "reading"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Reading")))
	  (todo "ON HOLD"
		((org-agenda-overriding-header "Put on hold")))
          ))))

(setq org-hierarchical-todo-statistics nil)

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
     ((or (eq char ?\() (eq char ?\)) (eq char ?\]) (eq char ?\[) (eq char ?\}) (eq char ?\{))
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


(defun my/org-in-table-p ()
  "Check if point is inside an Org table."
  (when (eq major-mode 'org-mode)
    (let ((element (org-element-at-point)))
      (eq (org-element-type element) 'table))))

(defun my/org-tab-behaviour ()
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
   ((or (org-at-heading-p) (org-at-drawer-p) (my/org-in-table-p))
    (org-cycle))

   ;; Default action: move forward to the next word
   (t (smart-forward))))

(defun md-tab-behaviour ()
  (interactive)
  (cond

   ;; Check if we can expand a yasnippet; if yes, do it and prevent further action
   ((yas-expand)
    nil)

   ;; Default action: move forward to the next word
   (t (smart-forward))))

 (with-eval-after-load 'org
  ;; Bind the custom function to TAB in Org mode.
  ;; Make sure this doesn't conflict with other keybindings you might have.
  (define-key org-mode-map (kbd "TAB") #'my/org-tab-behaviour)

  (define-key org-mode-map (kbd "C-<tab>") 'backward-word))

;;(define-key markdown-mode-map (kbd "TAB") 'smart-forward)
;;(define-key poly-quarto-mode-map (kbd "TAB") 'smart-forward)
;;(define-key markdown-mode-map (kbd "C-<tab>") 'backward-word)
;;(define-key poly-quarto-mode-map (kbd "C-<tab>") 'backward-word)

(use-package centered-cursor-mode
  :straight t
  :hook
  (prog-mode . centered-cursor-mode)
  (python-ts-mode . centered-cursor-mode)
  :config
  (setq ccm-recenter-at-end-of-file t)
  )

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

(setq-default cursor-type 'bar)

;;(setq display-line-numbers 'relative)
;;(global-display-line-numbers-mode)

(use-package treemacs
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "<f8>") 'treemacs)
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'right
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

(use-package neotree
  :straight t
  :config
  ;;(global-set-key [f8] 'neotree-toggle)
  (setq neo-window-position 'right)
)

;;(setq projectile-switch-project-action 'neotree-projectile-action)

;; clean up the mode-line
(use-package diminish
  :straight t)

(use-package olivetti
  :straight t
  :config
  (setq olivetti-body-width 150)
  :hook
  (org-agenda-mode . olivetti-mode))

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

;;(add-hook 'prog-mode-hook 'whitespace-mode)
;;(add-hook 'python-ts-mode-hook 'whitespace-mode)

(use-package highlight-indent-guides
  :straight t
  :hook (prog-mode . highlight-indent-guides-mode)
  :hook (python-ts-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guide-method 'character)
  (setq highlight-indent-guides-auto-odd-face-perc 50)
  (setq highlight-indent-guides-auto-even-face-perc 75)
  (setq highlight-indent-guides-auto-character-face-perc 50))

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(use-package htmlize
  :straight t)

(use-package simple-httpd
  :straight t
  :config
  (setq httpd-port 7070))

(defun markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 800px;margin: 0 auto;padding: 45px;\">%s</article></body><script src=\"/mathjax/es5/tex-chtml-full.js\"></script></html>" (buffer-string))))
   (current-buffer)))

(use-package impatient-mode
  :straight t)

(defun markdown-live-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(defun my-programming-mode-hook ()
  "Custom configurations for programming modes."
  (hs-minor-mode 1)
  (local-set-key (kbd "C-<tab>") 'hs-toggle-hiding))

(add-hook 'prog-mode-hook 'my-programming-mode-hook)

(use-package company
  :straight t
  :config
  (setq company-idle-delay 0.1
	company-minimum-prefix-length 1))

(use-package comint-mime
  :straight t)

(use-package pyenv
  :straight t
  :init
  (setenv "PYENV_VERSION" "3.13.0")
  :config
  (global-pyenv-mode)
  )

(use-package elpy
  :straight t
  :init
  (elpy-enable))

(use-package pytest
  :straight t
  )

(use-package quarto-mode
  :straight t
  :mode (("\\.Rmd" . poly-quarto-mode))
  :config
  (define-key poly-quarto-mode-map (kbd "TAB") 'md-tab-behaviour)
  (define-key markdown-mode-map (kbd "C-<tab>") 'backward-word)
  )


(custom-set-variables
 ;;
 ;; Other custom values...
 ;;
 '(markdown-command "/usr/local/bin/pandoc --mathjax")
 '(markdown-display-remote-images t)
 '(markdown-enable-math t)
 '(httpd-root "~/www")
 ;;
 ;; ...
 )

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

(setq ess-ask-for-ess-directory nil)
(setq ess-startup-directory nil)

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :straight t
  :interpreter ("scala" . scala-mode))

(use-package csv-mode
  :straight t)

(defun csv-highlight (&optional separator)
  (interactive (list (when current-prefix-arg (read-char "Separator: "))))
  (font-lock-mode 1)
  (let* ((separator (or separator ?\,))
         (n (count-matches (string separator) (pos-bol) (pos-eol)))
         (colors (cl-loop for i from 0 to 1.0 by (/ 2.0 n)
                          collect (apply #'color-rgb-to-hex 
                                         (color-hsl-to-rgb i 0.3 0.5)))))
    (cl-loop for i from 2 to n by 2 
             for c in colors
             for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
             do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))

(add-hook 'csv-mode-hook 'csv-highlight)
(add-hook 'csv-mode-hook 'csv-align-mode)
(add-hook 'csv-mode-hook '(lambda () (interactive) (toggle-truncate-lines nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (R . t)
   (latex . t)
   (haskell . t)
   (python . t)
   (shell . t)
   )
 )

;; disable the confirmation message
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

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

(use-package no-littering)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-archive-location "~/orgfiles/archive.org::")

;;(setq org-image-actual-width t) ;; Sets the width of image previewq in org-mode
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
(global-auto-revert-mode)

;;(global-visual-line-mode)

(require 'org-inlinetask)

(use-package org-download
  :straight t
  )

(use-package org-modern
  :straight t
)

;;(global-org-modern-mode)

(setq org-modern-priority
    (quote ((?A . "🔴")
            (?B . "🟡")
            (?C . "🟢"))))

;; Makes code blocks much more easily distinguishable!
(custom-set-faces
 '(org-block-begin-line
   ((t (:foreground "#073642" :background "#93a1a1" :extend t))))
;; '(org-block
;;   ((t (:background "#002b36" :extend t))))
 '(org-block-end-line
   ((t (:foreground "#073642" :background "#93a1a1" :extend t))))
 )

(setq org-latex-pdf-process (list "latexmk -shell-escape -bibtex -f -pdf %f"))

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
  (ivy-mode)) ; ivy-mode can be a pain)

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

(use-package ox-gfm
  :straight t)

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
                "^\\*chatgpt*\\*$"  chatgpt-shell-mode  ;chatgpt as a popup
                "example"  ess-r-mode  ;R as a popup
		"\\*R*\\*"  ess-r-mode  ;R as a popup
                )))
  (popper-mode +1)
  (popper-echo-mode +1))                ; For echo area hints

(setq prettify-symbols-alist
      '(
        ;;("lambda" . 955) ; λ
        ("->" . 8594)    ; →
        ("=>" . 8658)    ; ⇒
        ("map" . 8614)    ; ↦
       )
)

(global-prettify-symbols-mode 1)

(use-package projectile
  :straight t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(setq projectile-project-test-cmd "~/CPUJAX/bin/pytest")

(use-package smartparens-mode
  :straight smartparens  ;; install the package
  :hook (prog-mode text-mode markdown-mode org-mode inferior-ess-mode) ;; add `smartparens-mode` to these hooks
  :config
  ;; load default config
  (require 'smartparens-config)
  (sp-pair "$" "$")
  )

(smartparens-global-mode)
(sp-pair "$" "$")

(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (elisp-mode . rainbow-delimiters-mode)
  (latex-mode . rainbow-delimiters-mode)
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

(use-package vterm
  :straight t)

(use-package yasnippet
  :straight t
  :config
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (yas-global-mode 1)
;; :hook
;;  (org-mode . yas-minor-mode)
  )

(defun my-yas-org-fold-drawer-after-insert ()
  "Fold drawer just inserted by a yasnippet in org-mode."
  (when (eq major-mode 'org-mode)
    (save-excursion
      (org-cycle)))) ;; Fold the drawer.

(add-hook 'yas-after-exit-snippet-hook #'my-yas-org-fold-drawer-after-insert)
;;(add-hook 'org-mode-hook 'org-fold-hide-drawer-all)
