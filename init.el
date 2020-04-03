;;; init.el --- Emacs init file
;;; Commentary:
;;; Code:

;; Raise GC limits during startup for speed increase, then reset it
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda() (setq gc-cons-threshold 800000)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

(message "Starting Emacs...")

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Macros to keep it clean
(defmacro when-darwin (&rest body)
  "Evaluate BODY when the system is darwin."
  `(when (eq system-type 'darwin)
     ,@body))

(defmacro when-linux (&rest body)
  "Evaluate BODY when the system is GNU/Linux."
  `(when (eq system-type 'gnu/linux)
     ,@body))

(defmacro when-windows (&rest body)
  "Evaluate BODY when the system is Windows."
  `(when (eq system-type 'windows-nt)
     ,@body))

;; Variables for the same thing
(defvar my-darwin-p (eq system-type 'darwin)
  "Is this machine darwin?")

(defvar my-linux-p (eq system-type 'gnu/linux)
  "Is this machine linux?")

(defvar my-windows-p (eq system-type 'windows-nt)
  "Is this machine windows?")

;; Custom and bad elisp getter
(defun my-install-elisp (git-repo dir)
  "Clone from GIT-REPO into DIR inside of `user-emacs-directory'."
  (let ((elisp-dir (concat user-emacs-directory dir)))
    (unless (file-exists-p elisp-dir)
      (message (concat "Cloning " git-repo))
      (shell-command (concat "git clone " git-repo " " elisp-dir)))))

(defun my-update-elisp (git-dir)
  "Run git pull in GIT-DIR inside of `user-emacs-directory'."
  (message (concat "Pulling changes to " git-dir))
  (shell-command (concat "git -C " (expand-file-name user-emacs-directory) git-dir " pull origin master")))


;; Get my elisp
(my-install-elisp "https://github.com/dantecatalfamo/elisp" "elisp")
(my-install-elisp "https://github.com/dantecatalfamo/ejson-mode" "ejson-mode")
(my-install-elisp "https://github.com/dantecatalfamo/sysctl.el" "sysctl")
(my-install-elisp "https://github.com/Shopify/shadowenv.el" "shadowenv")


;; Start emacs daemon
;; (server-start)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elisp/")
  (add-to-list 'load-path "~/.emacs.d/ejson-mode/")
  (add-to-list 'load-path "~/.emacs.d/sysctl/")
  (require 'use-package))

;; Package declaration

(use-package 0x0
  :ensure t
  :commands (0x0-upload
             0x0-upload-file
             0x0-upload-string))


(use-package ace-jump-mode
  :ensure t
  :bind (("C-c <SPC>" . ace-jump-mode)))


(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil :foreground "red" :height 4.0))


(use-package all-the-icons
  :ensure t)


(use-package bytes-constant             ; Custom elisp
  :load-path "~/.emacs.d/elisp/bytes-constant.el"
  :commands (bytes-constant byteconst))


(use-package calfw
  :ensure t)


(use-package calfw-org
  :ensure t)


(use-package chess
  :ensure t
  :commands chess
  :init
  (defvar chess-images-default-size 72)
  (setq chess-default-display '(chess-ics1 chess-plain chess-images)))


(use-package com-css-sort
  :ensure t
  :after css-mode
  :bind (:map css-mode-map
              ("C-c C-s" . com-css-sort-attributes-block))
  :init
  (setq com-css-sort-sort-type 'alphabetic-sort))


(use-package company
  :ensure t
  :diminish
  :init
  (setq company-idle-delay .15)
  (setq company-echo-delay 0)
  (setq company-begin-commands '(self-insert-command))
  (setq company-lighter-base "Co")
  (setq company-minimum-prefix-length 2)
  :config
  (global-company-mode))


(use-package company-go
  :ensure t
  :after company
  :init
  (add-to-list 'company-backends 'company-go))


(use-package company-nginx
  :ensure t
  :after nginx-mode
  :hook (nginx-mode #'company-nginx-keywords))


;; (use-package company-quickhelp
;;   :ensure t
;;   :after company
;;   :config
;;   (company-quickhelp-mode))


(use-package cperl-mode
  :ensure t)


(use-package css-mode
  :mode "\\.css\\'"
  :init
  (setq css-indent-offset 2)) ; css-mode has it's own tab settings


(use-package csv-mode
  :ensure t)


(use-package descr-text
  :bind (("C-h T" . describe-char)))


(use-package dev-helper       ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :commands (dev-open-pr)
  :bind (("C-c e" . dev-change-project)))


(use-package diff-hl
  :ensure t
  :defer nil
  :hook  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))


(use-package diminish
  :ensure t)


(use-package dockerfile-mode
  :ensure t)


(use-package docker-tramp
  :ensure t)


(use-package doom-themes
  :ensure t)


(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))


(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))


(use-package ejson-mode           ; Custom elisp
  :load-path "~/.emacs.d/ejson-mode"
  :mode "\\.ejson\\'")


(use-package eldoc
  :diminish)


(use-package elfeed
  :after elfeed-goodies
  :ensure t
  :commands elfeed
  :config
  (elfeed-goodies/setup)
  (add-hook 'elfeed-show-mode-hook (lambda()
                                     (setq-local shr-width 75)
                                     (text-scale-set 2))))


(use-package elfeed-goodies
  :ensure t
  :commands elfeed-goodies/setup
  :init
  (setq elfeed-goodies/entry-pane-position 'bottom))


(use-package elisp-mode
  :hook (emacs-lisp-mode . prettify-symbols-mode))


(use-package elpy
  :ensure t)


(use-package htmlize
  :ensure t)


(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby")


(use-package epa
  :config
  (when-darwin
   (setq epa-pinentry-mode 'loopback)))


(use-package exec-path-from-shell
  :ensure t
  :if (not (eq system-type 'windows-nt))
  :config
  (when-darwin
   (setq exec-path-from-shell-variables '("PATH"
                                          "MAN_PATH"
                                          "NIX_PATH"
                                          "NIX_SSL_CERT_FILE")))
  (exec-path-from-shell-initialize))


(use-package erc
  :commands (erc)
  :init
  (setq erc-hide-list '("JOIN" "PART" "QUIT"))
  (defun my-erc-switch-to-buffer-all ()
    "Lke erc-switch-to-buffer but show all buffers by default."
    (interactive)
    (erc-switch-to-buffer t))

  :bind (:map erc-mode-map
              ("C-c C-b" . my-erc-switch-to-buffer-all)))


(use-package erc-hl-nicks
  :ensure t
  :after erc
  :init
  (setq erc-hl-nicks-mode t))


(use-package eshell
  :init
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 1048576))


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))


(use-package eyebrowse
  :ensure t
  :hook (after-init . eyebrowse-mode))


(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-mode-line-prefix "FC"))


(use-package flymd
  :ensure t
  :commands (flymd-flyit))


(use-package flyspell
  :hook (text-mode . flyspell-mode))


(use-package forge
  :ensure t
  :after magit)


(use-package frame
  :config
  (blink-cursor-mode t)) ; For emacsclient


(use-package git-timemachine
  :ensure t)


(use-package go-mode
  :ensure t
  :mode "\\.go\\'")


(use-package grip-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))


(use-package helm
  :ensure t
  :defer nil
  :diminish helm-mode
  :bind (("C-x c" . helm-command-prefix-key)
         ("C-c i" . helm-imenu)
         ("C-c o" . helm-occur)
         ("C-c m" . helm-all-mark-rings)
         ("C-x b" . helm-mini)
         ("C-c r" . helm-regexp)
         ("C-x C-f" . helm-find-files)
         ("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :config
  (helm-mode t))


(use-package helm-ag
  :ensure t
  :bind ("C-c g" . helm-projectile-ag))


(use-package helm-projectile
  :after (helm projectile)
  :ensure t)


(use-package helm-tramp
  :ensure t
  :commands (helm-tramp))


(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)))


(use-package holy-buffers    ; Custom elisp
  :load-path "~/.emacs.d/elisp/"
  :bind ("C-x k" . holy-buffers-kill-buffer))


(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-bs-show)))


(use-package imenu
  :after treemacs
  :commands imenu
  :init
  (setq imenu-auto-rescan t))


(use-package imenu-list
  :ensure t
  :commands (imenu-list))


(use-package init-file     ; Custom elisp
  :load-path "~/.emacs.d/elisp")


(use-package company-jedi
  :ensure t
  :after python-mode
  :config
  (add-to-list 'company-backends 'company-jedi))


(use-package js
  :config
  (setq js-indent-level 2))


(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")


(use-package keyfreq
  :ensure t
  :init
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))


(use-package kmacro
  :bind (("C-x (" . #'kmacro-start-macro-or-insert-counter)
         ("C-x )" . #'kmacro-end-or-call-macro)))


(use-package lua-mode
  :ensure t)


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t))


(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")


(use-package minitest
  :ensure t
  :diminish "MT"
  :hook (enh-ruby-mode . minitest-mode))


(use-package midnight
  :config
  (add-to-list 'clean-buffer-list-kill-regexps "\\`\\*helpful ")
  (midnight-mode))


(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-M->" . mc/mark-next-like-this)
         ("C-M-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))


(use-package nginx-mode
  :ensure t)


(use-package org
  :defer nil
  ;; :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :init
  (setq org-log-done 'time)  ; Can be 'time or 'note
  (setq org-directory "~/Org")
  ;; (setq org-special-ctrl-a/e t)
  ;; (setq org-hide-leading-stars t)
  :config
  (add-hook 'org-mode-hook 'auto-fill-mode))


(use-package org-export-config     ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :config
  (setq my/org-export-config-ssh-header
        "IgnoreUnknown AddKeysToAgent,UseKeychain

Host *
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_rsa
"))


(use-package org-journal
  :ensure t
  :bind (("C-c j" . my-org-journal-covid))
  :init
  (setq org-journal-dir "~/Org/Journal/")
  (setq org-journal-file-type 'monthly))


(use-package org-tree-slide
  :ensure t
  :commands (org-tree-slide-mode))


(use-package org-tv     ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :after org)


(use-package ox-gfm
  :ensure t)


(use-package paren
  :config
  (show-paren-mode t))


(use-package pdf-tools
  :ensure t
  :hook ((after-init . pdf-tools-install)))


(use-package perl6-mode
  :ensure t)

;; (use-package persp-mode
;;   :ensure t
;;   :hook ((after-init . persp-mode))
;;   :config
;;   (persp-def-auto-persp "erc" :mode 'erc-mode :hooks '(after-switch-to-buffer-functions)))


(use-package projectile
  :ensure t
  :after helm
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq-default projectile-mode-line-prefix " Proj") ; Shorten Projectile line mode prefix
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode t))


(use-package python
  :config
  (setq python-indent-offset 4))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package restclient
  :ensure t
  :commands (restclient-mode))


(use-package recentf
  :config
  (setq recentf-max-saved-items 200))


(use-package rjsx-mode
  :ensure t
  :mode "[cC]omponents\\/.*\\.js\\'")


(use-package robe
  :ensure t
  :after company
  :hook (enh-ruby-mode . robe-mode)
  :init
  (add-to-list 'company-backends 'company-robe))


(use-package rubocop
  :ensure t
  :hook (enh-ruby-mode . rubocop-mode))


(use-package ruby-frozen-string-literal ; custom elisp
  :load-path "~/.emacs.d/elisp"
  :hook enh-ruby-mode)


(use-package savehist
  :hook (after-init . savehist-mode))


(use-package shadowenv  ; Custom elisp
  :load-path "~/.emacs.d/shadowenv"
  :if my-darwin-p
  :hook (after-init . shadowenv-global-mode))


(use-package slime
  :ensure t
  :commands (slime)
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-quicklisp slime-asdf slime-company)))


(use-package slime-company
  :ensure t)


(use-package smartparens
  :ensure t
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(lisp-mode) "`" "`" :actions nil))


(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))


;; (use-package symon
;;   :ensure t
;;   :config
;;   (setq symon-delay 15)
;;   (when battery-status-function
;;    (pcase system-type
;;     ('darwin (add-to-list 'symon-monitors 'symon-darwin-battery-monitor t))
;;     ('gnu/linux (add-to-list 'symon-monitors 'symon-linux-battery-monitor t))
;;     ('windows-nt (add-to-list 'symon-monitors 'symon-windows-battery-monitor t))))
;;   (symon-mode))


(use-package sysctl   ; Custom elisp
  :load-path "~/.emacs.d/sysctl"
  :commands (sysctl))


(use-package tide
  :ensure t
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode)))


(use-package treemacs
  :ensure t
  :after imenu
  :bind (("C-c n" . treemacs)))


(use-package treemacs-projectile
  :after treemacs
  :ensure t)


(use-package undo-tree
  :ensure t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (setq undo-tree-mode-lighter " UT"))    ; Shorten Undo-Tree's line mode indicator


(use-package visual-regexp
  :ensure t
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))


(use-package vterm
  :ensure t)


(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'")
  :init
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2))


(use-package which-key
  :ensure t
  :diminish ;; " WK"
  :hook (after-init . which-key-mode)
  :init
  (which-key-setup-side-window-right-bottom))


(use-package windmove
  :hook (after-init . windmove-default-keybindings))


(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))


(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")


(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "")
  :config
  (yas-global-mode))


(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)


(use-package znc   ; Custom elisp, patched fork
  :load-path "~/.emacs.d/elisp"
  :commands (znc-erc))


(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode (if my-darwin-p 1 -1)) ; Only show menu bar on MacOS
(display-time-mode t) ; time in mode line
(column-number-mode) ; Show column in modeline
(delete-selection-mode)

;; (setq visible-bell t) ; disable computer beep
(setq ring-bell-function 'ignore)
(setq eval-expression-print-length nil) ; print entire expression in scratch
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq-default tab-width 4)
(setq save-interprogram-paste-before-kill t)
(setq require-final-newline t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t     ; Don't mess up hard links
      version-control t       ; Use version numbers on backups
      delete-old-versions t   ; Don't keep everything for all time
      kept-new-versions 10    ; How many new versions to keep
      kept-old-versions 5)    ; How many old
(setq prettify-symbols-unprettify-at-point 'right-edge)
(setq dired-dwim-target t)
(setq frame-resize-pixelwise t)
(setq inhibit-x-resources t)  ; Fix emacsclient issues

;; HACK: Fix ELPA https refresh issue
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; MacOS titlebar
(when-darwin
 (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
 (setq ns-use-proxy-icon  nil))
;; (setq frame-title-format nil)

;; Use command key as meta in macos
(when-darwin
 (setq mac-option-modifier 'super
       mac-command-modifier 'meta))


;; Set the initial frame size
;; (when window-system
;;   (set-frame-size (selected-frame) 120 40))

;;; Mode line

(defvar my-global-mode-string-line
  '(:eval (if (display-graphic-p) "  [" "- ["))
  "Add space between the mode string and global mode stringg.")

;; ;; (setf (nthcdr 2 mode-line-format) (cons "%I " (nthcdr 2 mode-line-format))) ; Format default string
(setf (car global-mode-string) my-global-mode-string-line) ; Add bracket to beginning
(add-to-list 'global-mode-string "]"  t) ; Add closing bracket

(setq-default mode-line-format '("%e"
                                 mode-line-front-space
                                 "%I "
                                 mode-line-mule-info
                                 mode-line-client
                                 mode-line-modified
                                 mode-line-remote
                                 mode-line-frame-identification
                                 mode-line-buffer-identification
                                 "   "
                                 mode-line-position
                                 (vc-mode vc-mode)
                                 "  "
                                 mode-line-modes
                                 mode-line-misc-info
                                 mode-line-end-spaces))

;;; eshell commands

;; from https://www.reddit.com/r/emacs/comments/blo25q/weekly_tipstricketc_thread/emt5zw4/
(defun eshell/ccat (file)
  "Like `cat' for FILE but output with Emacs syntax highlighting."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (delay-mode-hooks
        (set-auto-mode)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))
    (buffer-string)))


;;; Functions

(defun my-org-journal-covid ()
  "Open org-journal with covid directory."
  (interactive)
  (let ((org-journal-dir "~/Org/COVID/"))
    (org-journal-new-entry nil)
    (org-journal-mode)
    (org-cycle)))

(defun my-diff-this-buffer-with-file ()
  "Diffs the current buffer with the saved file."
  (interactive)
  (if (not buffer-file-name)
      (message "Buffer has no file on disk.")
    (diff-buffer-with-file (current-buffer))
    (switch-to-buffer-other-frame "*Diff*")))

(defun my-delete-completion-buffer ()
  "Deletes the *Completion* buffer."
  (let ((buffer "*Completions*"))
    (and (get-buffer buffer)
         (kill-buffer buffer))))

(defun my-erc-clear-modified-buffers ()
  "Clears the modified buffersd notifications from the modeline."
  (interactive)
  (setq erc-modified-channels-alist '())
  (setq erc-modified-channels-object nil))

(defun quick-copy-line ()
  "Copy the whole line that point is on and move to beginning of the next line.
Consecutive calls to this command append each line to the
`kill-ring'."
  (interactive)
  (let ((beg (line-beginning-position 1))
        (end (line-beginning-position 2)))
    (if (eq last-command 'quick-copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-new (buffer-substring beg end))))
  (beginning-of-line 2))

; Taken from https://endlessparentheses.com/get-in-the-habit-of-using-sharp-quote.html
(defun endless/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(bind-key "C-c d" #'my-diff-this-buffer-with-file)
(bind-key "C-s" #'isearch-forward-regexp)
(bind-key "C-r" #'isearch-backward-regexp)
(bind-key "C-M-s" #'isearch-forward)
(bind-key "C-M-r" #'isearch-backward)
(bind-key "C-x p" #'other-window)
(bind-key "C->" #'indent-rigidly-right-to-tab-stop)
(bind-key "C-<" #'indent-rigidly-left-to-tab-stop)
(bind-key "C-c x" #'quick-copy-line)
(bind-key "C-c k" #'bury-buffer)
(bind-key "#" #'endless/sharp emacs-lisp-mode-map)
(bind-key "#" #'endless/sharp lisp-mode-map)

;;; Old Hooks

(add-hook 'prog-mode-hook (lambda() (setq display-line-numbers t)))

(add-hook 'go-mode-hook (lambda() (setq-local tab-width 4)))
(add-hook 'go-mode-hook (lambda() (add-hook 'before-save-hook 'gofmt-before-save)) t) ; lint go before saving

(add-hook 'web-mode-hook (lambda() (setq tab-width 2))) ; 2 space tabs for js and web
(add-hook 'css-mode-hook (lambda() (setq tab-width 2))) ; 2 space tabs for css and scss

(add-hook 'eshell-mode-hook (lambda() (company-mode -1)))  ; disable company mode in eshell because of TRAMP performance

(add-hook 'prog-mode-hook (lambda() (add-hook 'write-contents-functions
                                         'delete-trailing-whitespace
                                         nil t)))	 ; Strip trailing whitespace for all code

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook 'my-delete-completion-buffer)

(unless my-darwin-p
  (set-face-attribute
   'default nil :inherit nil :height 92))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)

;;; init.el ends here
