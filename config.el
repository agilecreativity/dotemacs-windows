
(require 'cl)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  ;; if not we install 'use-package before use
  (package-install 'use-package))

;; Always install the missing package if required by 'use-package
(setq use-package-always-ensure t)

;; Personal Information
(setq use-full-name "Burin Choomnuan"
      use-email-address "agilecreativity@gmail.com")

;; Make sure that we dont' clutter the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Custom elisp directory
(setq custom-elisp-dir
      (expand-file-name "elisp" user-emacs-directory))

(add-to-list 'load-path custom-elisp-dir)

;; Answer just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Let make use of UTF-8 for all
(setq locale-encoding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

;; Show the rows/columns number
(setq line-number-mode t)
(setq column-number-mode t)

;; Line should be 80 chars wide not 70
(setq fill-column 80)

;; Save a list of recent files visited (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Explicitly load the file from the custom-elisp directory
(load-file (concat custom-elisp-dir "/buffer-defuncs.el"))

;; Explicitly load the file from the custom-elisp directory
(load-file (concat custom-elisp-dir "/agc-perspective.el"))

;; Sensible defaults
(load-file (concat custom-elisp-dir "/sensible-defaults.el"))
(progn
  (sensible-defaults/increase-gc-threshold)
  (sensible-defaults/backup-to-temp-directory)
  (sensible-defaults/delete-trailing-whitespace)
  (sensible-defaults/treat-camelcase-as-separate-words)
  (sensible-defaults/automatically-follow-symlinks)
  (sensible-defaults/single-space-after-periods)
  (sensible-defaults/offer-to-create-parent-directories-on-save)
  (sensible-defaults/apply-changes-to-highlighted-region)
  (sensible-defaults/overwrite-selected-text))

;; Don't show any menud
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(use-package guide-key
  :ensure t
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
    (guide-key-mode 1)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-eighties t)))

;; Required by the functions below
(use-package dash
  :ensure t)

(use-package s
  :ensure t)

;; For the function like =sp-copy-sexp= etc
(use-package smartparens
  :ensure t)

(use-package paredit
  :ensure t
  :init
  ;; Hook to the right mode
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))

  ;; Now some functions
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round)
    (insert " ")
    (forward-char -1))

  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))

  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))

  (defun paredit-kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (paredit-backward-kill-word)))

  ;; Then bind the paredit for extra functionality
  :bind (("M-("  . paredit-wrap-round)
         ("M-)"  . paredit-wrap-round-from-behind)
         ("M-s-8"  . paredit-wrap-square)
         ("M-s-9" . paredit-wrap-square-from-behind)
         ("M-s-(" . paredit-wrap-curly)
         ("M-s-)" . paredit-wrap-curly-from-behind)
         ("C-w" . paredit-kill-region-or-backward-word)
         ("M-C-<backspace>" . backward-kill-sexp)))

(scroll-bar-mode -1)

(setq initial-scratch-message "")

(setq visible-bell nil)

(global-hl-line-mode 1)

(show-paren-mode 1)

(setq user-full-name "Burin Choomnuan"
      user-email-address "agilecreativity@gmail.com")

;; Make sure that we dont' clutter the init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Custom elisp directory
(setq custom-elisp-dir
      (expand-file-name "elisp" user-emacs-directory))

(add-to-list 'load-path custom-elisp-dir)

;; Answer just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Let make use of UTF-8 for all
(setq locale-encoding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq-default indent-tabs-mode nil)

;; Show the rows/columns number
(setq line-number-mode t)
(setq column-number-mode t)

;; Line should be 80 chars wide not 70
(setq fill-column 80)

;; Save a list of recent files visited (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Explicitly load the file from the custom-elisp directory
(load-file (concat custom-elisp-dir "/buffer-defuncs.el"))

;; Explicitly load the file from the custom-elisp directory
(load-file (concat custom-elisp-dir "/agc-perspective.el"))

;; Sensible defaults
(load-file (concat custom-elisp-dir "/sensible-defaults.el"))
(progn
  (sensible-defaults/increase-gc-threshold)
  (sensible-defaults/backup-to-temp-directory)
  (sensible-defaults/delete-trailing-whitespace)
  (sensible-defaults/treat-camelcase-as-separate-words)
  (sensible-defaults/automatically-follow-symlinks)
  (sensible-defaults/single-space-after-periods)
  (sensible-defaults/offer-to-create-parent-directories-on-save)
  (sensible-defaults/apply-changes-to-highlighted-region)
  (sensible-defaults/overwrite-selected-text))

;; Don't show any menud
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(use-package zenburn-theme
  :ensure t)

(use-package guide-key
  :ensure t
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
    (guide-key-mode 1)))

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (progn
    (load-theme 'sanityinc-tomorrow-night t)))

;; Required by the functions below
(use-package dash
  :ensure t)

(use-package s
  :ensure t)

;; For the function like =sp-copy-sexp= etc
(use-package smartparens
  :ensure t)

(use-package paredit
  :ensure t
  :init
  ;; Hook to the right mode
  (add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'clojure-mode-hook (lambda () (paredit-mode 1)))
  (add-hook 'cider-repl-mode-hook (lambda () (paredit-mode 1)))

  ;; Now some functions
  (defun paredit-wrap-round-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-round)
    (insert " ")
    (forward-char -1))

  (defun paredit-wrap-square-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-square))

  (defun paredit-wrap-curly-from-behind ()
    (interactive)
    (forward-sexp -1)
    (paredit-wrap-curly))

  (defun paredit-kill-region-or-backward-word ()
    (interactive)
    (if (region-active-p)
        (kill-region (region-beginning) (region-end))
      (paredit-backward-kill-word)))

  ;; Then bind the paredit for extra functionality
  :bind (("M-(" . paredit-wrap-round)
         ("M-)" . paredit-wrap-round-from-behind)
         ("M-s-8" . paredit-wrap-square)
         ("M-s-9" . paredit-wrap-square-from-behind)
         ("M-s-(" . paredit-wrap-curly)
         ("M-s-)" . paredit-wrap-curly-from-behind)
         ("C-w" . paredit-kill-region-or-backward-word)
         ("M-C-<backspace>" . backward-kill-sexp)))

(scroll-bar-mode -1)

(setq initial-scratch-message "")

(setq visible-bell nil)

(global-hl-line-mode 1)

(show-paren-mode 1)

(setq inhibit-startup-message t)

(use-package dash
  :ensure t
  :config (eval-after-load "dash" '(dash-enable-font-lock)))

(use-package s
  :ensure t)

(use-package f
  :ensure t)

;; Note: for this to work one must install "Source Code Pro" fonts
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "Source Code Pro" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro Medium 10"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro Medium 10"))))

 ((string-equal system-type "darwin") ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))))

 ((string-equal system-type "gnu/lsinux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "Source Code Pro Medium 12"))
    (add-to-list 'default-frame-alist '(font . "Source Code Pro Bold 12")))))

(add-to-list 'default-frame-alist ' (fullscreen . maximized))

(use-package org
  :config
  ;; Need to install `graphviz' locally first
  ;; (add-to-list 'org-src-lang-modes '("dot" . "graphviz-dot"))

  ;; Add languages that we can use with org-babel
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((sh . t)
                                 (js . t)
                                 (emacs-lisp . t)
                                 (clojure . t)
                                 (python . t)
                                 (dot . t)
                                 (ruby . t)
                                 (java . t)
                                 ;; (dot . t)
                                 (css . t)))
  :init
  (bind-key "C-c c" 'org-capture)
  (setq org-default-notes-file "~/Dropbox/org/notes.org"))

(defun my-org-confirm-babel-evaluate (lang body)
  "Do not confirm evaluation for these languages."
  (not (or (string= lang "java")
           (string= lang "python")
           (string= lang "emacs-lisp")
           (string= lang "clojure"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

;; Optional but nice to installed packages
(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :ensure t
  :init
  (bind-key "C-x g" 'magit-status))

;; Let add something like Helm and Ag for searching
(use-package helm
  :ensure t)

(use-package ag
  :ensure t)

;; Note: need to have the-silver-searcher install, try choco install ag
;; Or also choco install ripgrep, pt
(use-package helm-ag
  :ensure t)

;;;; Tips: http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
;;(let* ((app-data-dir (getenv "APPDATA"))
;;       (server-auth-dir-path (concat app-data-dir "/.emacs.d/server")))
;; (when (and (eq window-system 'w32) (file-exists-p app-data-dir))
;;   ;; Suppress error "directory ~/.emacs.d/server is unsafe" on Windows
;;   (defun server-ensure-safe-dir (dir) "Noop" t))
;;  (server-start))


;; Better default
(setq scroll-margin 5
      scroll-preserve-screen-position 1)

(use-package expand-region
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package wgrep-ag
  :ensure t)

(use-package change-inner
  :ensure t)

;; Smart M-x is smart
(use-package smex
  :ensure t)
(smex-initialize)

;; See: https://github.com/davazp/graphql-mode
(use-package graphql-mode
  :ensure t
  :init)

;; https://github.com/sensorflo/adoc-mode/wiki
;; For : https://github.com/clojure-cookbook/clojure-cookbook
(use-package adoc-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist (cons "\\.asciidoc\\'" 'adoc-mode)))

(use-package salt-mode
  :ensure t
  :init)

(use-package terraform-mode
  :ensure t)

;; ibuffer is the improved version of list-buffers
(defalias 'list-buffers 'ibuffer)

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Install company and enable it globally
(use-package company
  :ensure t
  :bind (("C-c /" . company-auto-complete))
  :config
  (global-company-mode t))
