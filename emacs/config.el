(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
        ;; Enable :elpaca use-package keyword.
        (elpaca-use-package-mode)
        ;; Assume :elpaca t unless otherwise specified.
        (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
(use-package general :demand t)
(elpaca-wait)


;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
;; (elpaca nil (message "deferred"))

(add-to-list 'load-path "~/.config/emacs/lisp/")

;; Display the line numbers at the side of the window
(global-set-key (kbd "C-c l") 'display-line-numbers-mode)

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
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<right>") 'windmove-right)

;; Todo
(global-set-key (kbd "C-c t") 'org-toggle-item)
(global-set-key (kbd "C-c d") 'org-todo)

;; Return to dashboard
(global-set-key (kbd "C-c <return>") 'dashboard-open)

;; Org-Agenda Shortcut
(global-set-key (kbd "C-c o") 'org-agenda)

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
                    :font "ProggyCleanNerdFont"
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

;; For a bit of added spice (seems broken with ProggyClean)
(set-face-attribute 'font-lock-comment-face nil
                    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
                      :slant 'italic)

;; and to make sure client windows open with these fonts
(add-to-list 'default-frame-alist '(font . "ProggyCleanNerdFont"))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-dracula t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; In this house, we use shortcuts damnit!!!'

;; Get rid of pesky GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq default-frame-alist '((undecorated . t)))

;; Some nice transparency
(add-to-list 'default-frame-alist '(alpha-background . 95))

;; Make the modeline pretty
(use-package solaire-mode
  :config (solaire-global-mode))

;; not sure where to put this lol
(delete-selection-mode 1)

;; Margin Adjust
(setq left-margin-width 3)
(setq right-margin-width 3)

(setq org-agenda-files
      '("~/MyProjects/Notes/APTS" "~/MyProjects/Notes/APTS/StatsComputation" "~/MyProjects/Notes/APTS/StatsInference" "~/orgfiles"))

(setq org-agenda-custom-commands
      '(("v" "PhD Tasks"
         ((tags "general"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "General Statistics Tasks")))
          (tags "APTS"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks from the Warwick APTS Course")))
          (tags "org"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks relating to org and the config file")))
          (tags "reading"
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "Tasks relating to the reading list")))
          (agenda "")
          (alltodo "")))))

(defadvice find-file (before make-directory-maybe (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))



(use-package nerd-icons)

(use-package dashboard
    :ensure t 
    :init
    (setq initial-buffer-choice 'dashboard-open)
    (setq dashboard-set-heading-icons t)
    (setq dashboard-set-file-icons t)
    (setq dashboard-banner-logo-title "woah what how did he get here")
    ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
    (setq dashboard-startup-banner "/home/tate/Tatemacs/wohhowdidhegethere/toby.gif")  ;; use custom image as banner
    (setq dashboard-center-content nil) ;; set to 't' for centered content
    (setq dashboard-items '((recents . 50)
                            (bookmarks . 10)))
    :custom
    (dashboard-modify-heading-icons '((recents . "file-text")
                                      ))
    :config
    (dashboard-setup-startup-hook)
    )

(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
(setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package

(use-package beacon
  :ensure t
  :config (beacon-mode))

(global-display-line-numbers-mode 0)
(global-visual-line-mode t)

(use-package neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-window-width 50)

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; clean up the mode-line
(use-package diminish)

(use-package smart-mode-line
  :config (sml/setup))

(use-package mode-icons
  :config (mode-icons-mode))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :diminish
  :config
  (ivy-mode))

;;(use-package all-the-icons-ivy-rich
  ;;:ensure t
  ;;:init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode))
  )
(setq markdown-enable-math t)

(use-package auctex
:defer t
:ensure t)
(setq org-highlight-latex-and-related '(native))

(use-package cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)
(add-hook 'latex-mode-hook 'turn-on-cdlatex)
(add-hook 'org-mode-hook #'turn-on-org-cdlatex)

;; Line below currently breaks things
;; (add-hook 'after-save-hook #'org-latex-export-to-pdf)

(use-package ess)

(require 'package)

;; Add melpa to your packages repositories
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Install use-package if not already installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Enable defer and ensure by default for use-package Keep
;; auto-save/backup files separate from source code:
;; https://github.com/scalameta/metals/issues/1027
(setq use-package-always-defer t
      use-package-always-ensure t
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :interpreter ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
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
  :diminish
  :init (global-flycheck-mode))

(use-package lsp-mode
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
(use-package lsp-metals)

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
(use-package lsp-ui)

;; lsp-mode supports snippets, but in order for them to work you need to use yasnippet
;; If you don't want to use snippets set lsp-enable-snippet to nil in your lsp-mode settings
;; to avoid odd behavior with snippets and indentation
(use-package yasnippet)
(use-package ivy-yasnippet
  :hook (yas-minor-mode . ivy-yasnippet))

;; Use company-capf as a completion provider.
;;
;; To Company-lsp users:
;;   Company-lsp is no longer maintained and has been removed from MELPA.
;;   Please migrate to company-capf.
(use-package company
  :diminish
  :hook (scala-mode . company-mode)
  :config
  (setq lsp-completion-provider :capf))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(org-babel-do-load-languages
  'org-babel-load-languages
  '(
    (R . t)
    (latex . t)
))

;; disable the confirmation message
(setq org-confirm-babel-evaluate nil)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

;; Make the different levels indented
(add-hook 'org-mode-hook 'org-indent-mode)

;; Use Bullets instead of Aterickses
;;(use-package org-bullets)
;;(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(electric-indent-mode -1)

(require 'org-tempo) ;; now we can write '<s' then press <TAB> for immediate src action!

(setq org-image-actual-width 500) ;; Sets the width of image previewq in org-mode

;; Sets the size of LaTeX previews 
(setq org-format-latex-options (plist-put org-format-latex-options :scale 0.9))

(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(use-package math-preview
  :load-path "/home/tate/.asdf/plugins/math-preview"
  :custom (math-preview-command "/home/tate/.asdf/plugins/math-preview/math-preview.js"))

(setq org-preview-latex-image-directory ".ltximg/")

(use-package ox-pandoc)
(use-package auto-org-md)

(use-package no-littering)



(use-package sublimity
  :config (sublimity-mode))

(use-package sudo-edit)

(use-package which-key
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

(setq org-agenda-file-menu-enabled t)

(setq org-agenda-files
        '("~/MyProjects/Notes/APTS" "~/MyProjects/Notes/APTS/StatsComputation" "~/MyProjects/Notes/APTS/StatsInference" "~/orgfiles"))
