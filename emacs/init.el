(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)

(use-package exec-path-from-shell
  :straight t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("95e934b092694a2624adb653043d1dc016a6712fa27b788f9ff4dffb8ee08290" "b5fd9c7429d52190235f2383e47d340d7ff769f141cd8f9e7a4629a81abc6b19" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" default))
 '(highlight-indent-guides-method 'character)
 '(httpd-root "~/www")
 '(org-babel-python-command-session "ipython")
 '(org-export-backends '(html latex md gfm))
 '(org-export-show-temporary-export-buffer nil)
 '(org-export-use-babel nil)
 '(org-hide-emphasis-markers t)
 '(org-highlight-latex-and-related '(native latex script))
 '(org-image-align 'center)
 '(org-latex-hyperref-template nil)
 '(org-latex-prefer-user-labels t)
 '(org-latex-preview-live-display-type 'buffer)
 '(org-latex-src-block-backend 'minted)
 '(org-roam-capture-templates
   '(("d" "default" plain "%?" :target
      (file+head "${slug}.org" "#+title: ${title}\12")
      :unnarrowed t)))
 '(org-roam-dailies-capture-templates
   '(("d" "default" entry "* %?" :target
      (file+head ".org" "#+title: %<%Y-%m-%d>\12"))))
 '(org-src-preserve-indentation t)
 '(org-startup-indented t)
 '(org-startup-with-latex-preview t)
 '(package-selected-packages '(edit-indirect)))

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'abbrev-mode)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block-begin-line ((t (:foreground "#073642" :background "#93a1a1" :extend t))))
 '(org-block-end-line ((t (:foreground "#073642" :background "#93a1a1" :extend t)))))

(define-key global-map (kbd "C-j") 'next-line) ; this keybinding likes to be taken over by various modes...
;; (define-key treemacs-mode-map (kbd "C-j") 'next-line) ; treemacs can be a pain
(define-key ivy-mode-map (kbd "C-j") 'next-line) ; ivy can be a pain
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(scroll-bar-mode -1)
(put 'narrow-to-region 'disabled nil)
