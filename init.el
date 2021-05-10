;;; init.el --- Emacs init file
;;; Commentary:
;;; Code:

;; Raise GC limits during startup for speed increase, then reset it
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda() (setq gc-cons-threshold 800000)))

(when (version< emacs-version "27")
  (package-initialize)) ; Called implicitly in Emacs 27+

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Load customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))

(setq user-full-name "Dante Catalfamo"
      user-mail-address "dante.catalfamo@gmail.com")

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
(defvar darwin-p (eq system-type 'darwin)
  "Is this machine darwin?")

(defvar linux-p (eq system-type 'gnu/linux)
  "Is this machine linux?")

(defvar windows-p (eq system-type 'windows-nt)
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


;; Start emacs daemon
;; (server-start)

(eval-when-compile
  (add-to-list 'load-path "~/.emacs.d/elisp/")
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
  :bind (("C-x o" . ace-window)
         ("M-o" . other-window))
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :config
  (set-face-attribute
   'aw-leading-char-face nil :foreground "red" :height 4.0))


(use-package ag
  :ensure t
  :defer t)


(use-package all-the-icons
  :ensure t
  :defer t)


(use-package async
  :ensure t
  :config
  (dired-async-mode t)
  (async-bytecomp-package-mode 1))


(use-package autorevert
  :custom
  (auto-revert-mode-text " ⟲"))


(use-package bytes-constant             ; Custom elisp
  :load-path "~/.emacs.d/elisp/bytes-constant.el"
  :commands (bytes-constant byteconst))


(use-package browse-at-remote
  :ensure t
  :commands browse-at-remote)


(use-package calfw
  :defer t
  :ensure t)


(use-package calfw-org
  :ensure t
  :commands cfw:open-org-calendar)


(use-package cc-mode
  :init
  (setq-default c-basic-offset 4))


(use-package ccls
  :ensure t
  :defer t)


(use-package chess
  :ensure t
  :commands chess
  :init
  ;; (setq chess-default-display '(chess-ics1 chess-plain chess-images))
  (defvar chess-images-default-size 72))


(use-package com-css-sort
  :ensure t
  :after css-mode
  :bind (:map css-mode-map
              ("C-c C-s" . com-css-sort-attributes-block))
  :init
  (setq com-css-sort-sort-type 'alphabetic-sort))


(use-package comint
  :hook (comint-mode . (lambda () (company-mode -1))))


(use-package company
  :ensure t
  :defer nil
  :diminish
  :bind (("C-<tab>" . company-complete))
  :init
  (setq company-idle-delay .15)
  (setq company-echo-delay 0)
  ;; (setq company-begin-commands '(self-insert-command))
  (setq company-show-numbers t)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  :config
  (global-company-mode))


(use-package company-jedi
  :ensure t
  :after python-mode
  :config
  (add-to-list 'company-backends 'company-jedi))


(use-package company-nginx
  :ensure t
  :after nginx-mode
  :hook (nginx-mode 'company-nginx-keywords))


(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (company-quickhelp-mode))


(use-package css-mode
  :mode "\\.css\\'"
  :hook (css-mode-hook . (lambda() (setq-local tab-width 2)))
  :init
  (setq css-indent-offset 2)) ; css-mode has it's own tab settings


(use-package csv-mode
  :ensure t
  :mode "\\.[Cc][Ss][Vv]\\'")


(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))


(use-package decide
  :ensure t
  :bind-keymap ("C-c t" . decide-prefix-map))


(use-package delsel
  :config
  (delete-selection-mode))


(use-package descr-text
  :bind (("C-h T" . describe-char)))


(use-package repo-helper       ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :commands (repo-open-pr)
  :bind (("C-c e" . repo-change-project)))


(use-package diff
  :bind ("C-c d" . my-diff-this-buffer-with-file))


(use-package diff-hl
  :ensure t
  :defer nil
  :hook  (magit-post-refresh . diff-hl-magit-post-refresh)
  :config
  (global-diff-hl-mode))


(use-package diminish
  :ensure t)


(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")


(use-package docker-tramp
  :ensure t
  :defer t)


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t))


(use-package dired
  :config
  (setq dired-dwim-target t))


(use-package dired-filter
  :ensure t
  :defer t
  :init ; maps are autoloaded
  (define-key dired-mode-map (kbd "/") dired-filter-map)
  (define-key dired-mode-map (kbd ",") dired-filter-mark-map))


(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("C-c C-c" . dired-ranger-copy)
              ("C-c C-p" . dired-ranger-paste)
              ("C-c C-m" . dired-ranger-move)))


(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)))


(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))


(use-package ejson-mode
  :ensure t
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


(use-package elpher
  :ensure t
  :commands (elpher elpher-go))


(use-package elpy
  :hook (python-mode . elpy-enable)
  :ensure t)


(use-package emoji-cheat-sheet-plus
  :ensure t)


(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))


(use-package htmlize
  :ensure t
  :defer t)


(use-package enh-ruby-mode
  :ensure t
  :mode "\\.rb\\'"
  :interpreter "ruby")


(use-package epa
  :config
  (when-darwin
   (setq epg-pinentry-mode 'loopback)))


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
  :commands (erc erc-tls)
  :bind (:map erc-mode-map
              ("C-c C-b" . my-erc-switch-to-buffer-all))
  :custom
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-modules
   '(autojoin button completion fill irccontrols keep-place list match
              menu move-to-prompt netsplit networks noncommands notifications
              readonly ring scrolltobottom stamp spelling track hl-nicks)))


(use-package erc-hl-nicks
  :ensure t
  :after erc
  :init
  (erc-hl-nicks-mode +1))


(use-package eshell
  :hook ((eshell-mode . my-disable-tramp-company)
         (eshell-mode . my-disable-company))
  :config
  (setq eshell-destroy-buffer-when-process-dies t)
  (setq eshell-hist-ignoredups t)
  (setq eshell-history-size 1048576))


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))


(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-mode-line-prefix "FC"))


(use-package flycheck-package
  :ensure t
  :defer t)


(use-package flycheck-perl6
  :ensure t
  :defer t)


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


(use-package gcmh
  :ensure t
  :diminish
  :init
  (setq gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (setq gcmh-idle-delay 5)
  (gcmh-mode))


(use-package gdb-mi
  :init
  (setq gdb-many-windows t))


(use-package gdscript-mode
  :ensure t
  :defer t)


(use-package git-timemachine
  :ensure t
  :commands git-timemachine)


(use-package go
  :ensure t
  :commands go-play)


(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook (go-mode-hook . (lambda() (setq-local tab-width 4))))


(use-package goto-addr
  :defer t)


(use-package grip-mode
  :ensure t
  :after markdown-mode
  :bind (:map markdown-mode-command-map
              ("g" . grip-mode)))


(use-package graphql-mode
  :ensure t
  :defer t)


(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.dot\\'" "\\.gv\\'"))


(use-package helm
  :ensure t
  :defer nil
  :diminish helm-mode
  :bind (("C-x c" . helm-command-prefix-key)
         ("C-c i" . helm-imenu)
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


(use-package helm-atoms
  :ensure t
  :defer t)


(use-package helm-descbinds
  :ensure t)


(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode))


(use-package helm-projectile
  :after (helm projectile)
  :ensure t)


(use-package helm-system-packages
  :ensure t
  :defer t)


(use-package helm-tramp
  :ensure t
  :commands (helm-tramp))


(use-package helpful
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))


(use-package holy-buffers    ; Custom elisp
  :load-path "~/.emacs.d/elisp/"
  :bind ("C-x k" . holy-buffers-kill-buffer))


(use-package ibuffer
  :bind (("C-x C-b" . ibuffer-bs-show)))


(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))


(use-package imenu
  :after treemacs
  :commands imenu
  :init
  (setq imenu-auto-rescan t))


(use-package imenu-list
  :ensure t
  :bind ("<f5>" . imenu-list-smart-toggle)
  :config
  (setq imenu-list-focus-after-activation t)
  (setq imenu-list-auto-resize nil)
  (setq imenu-list-size 33))


(use-package indent
  :bind (("C->" . indent-rigidly-right-to-tab-stop)
         ("C-<" . indent-rigidly-left-to-tab-stop)))


(use-package init-file     ; Custom elisp
  :load-path "~/.emacs.d/elisp")


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
  :bind (("C-x (" . kmacro-start-macro-or-insert-counter)
         ("C-x )" . kmacro-end-or-call-macro)))


(use-package latex-preview-pane
  :ensure t
  :commands latex-preview-pane-mode)


(use-package lorem-ipsum
  :ensure t)


(use-package lua-mode
  :ensure t
  :defer t)


(use-package lsp-mode
  :ensure t
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (c-mode . lsp-deferred)
         (c-mode . my-lsp-install-save-hooks)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-mode . my-lsp-install-save-hooks))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c s"))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :ensure t)


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t))


(use-package magit-gitflow
  :ensure t
  :hook (magit-mode . turn-on-magit-gitflow))


(use-package magit-todos
  :ensure t
  :after magit
  :config
  (setq magit-todos-exclude-globs '("*.js.map" "TAGS" "*.lock" "archive-contents" "elpa/"))
  (setq magit-todos-keyword-suffix "\\(?:([^)]+)\\)?[: ]") ; make colon optional
  (magit-todos-mode))


(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'")


(use-package menu-bar
  :config
  (menu-bar-mode (if darwin-p 1 -1))) ; Only show menu bar on MacOS


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
  :ensure t
  :defer t)


(use-package ns-win ;; Only available on MacOS builds
  :if darwin-p
  :config
  ;; Use command key as meta in macos
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))


(use-package org
  :defer nil
  ;; :ensure org-plus-contrib
  :hook ((org-mode . auto-fill-mode)
         (org-mode . my-add-whitespace-hook))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-log-done 'time)  ; Can be 'time or 'note
  (setq org-directory "~/Org")
  (setq org-default-notes-file "~/Org/Notes.org")
  ;; (setq org-special-ctrl-a/e t)
  ;; (setq org-hide-leading-stars t)
  ;; (setq org-hide-emphasis-markers t)
  ;; (setq org-agenda-span 'fortnight)
  (setq org-agenda-hide-tags-regexp "noexport")
  ;; (setq org-agenda-start-day nil)
  (setq org-agenda-start-on-weekday nil)
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 3)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-tab-acts-natively nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-load-languages '((emacs-lisp . t) (shell . t) (python . t))))


(use-package org-compat
  :config
  (setq org-imenu-depth 4))


(use-package org-journal
  :ensure t
  :bind (:prefix "C-c j"
         :prefix-map my-org-journal-map
         ("j" . org-journal-new-entry)
         ("c" . my-org-journal-covid))
  :init
  (setq org-journal-dir "~/Org/Journal/")
  (setq org-journal-file-type 'monthly))


(use-package org-roll  ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :after org)


(use-package org-table-dates  ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :after org)


(use-package org-tree-slide
  :ensure t
  :commands (org-tree-slide-mode))


(use-package org-tv     ; Custom elisp
  :load-path "~/.emacs.d/elisp"
  :after org)


(use-package ox-gfm
  :ensure t
  :after org)


(use-package ox-ssh
  :ensure t
  :after org
  :config
  (setq org-ssh-header "IgnoreUnknown AddKeysToAgent,UseKeychain

Host *
  AddKeysToAgent yes
  UseKeychain yes
  IdentityFile ~/.ssh/id_rsa
"))


(use-package package
  :init
  (setq package-quickstart t))


(use-package package-lint
  :ensure t
  :commands package-lint-current-buffer)


(use-package page-break-lines
  :ensure t
  :config
  (global-page-break-lines-mode))


(use-package paren
  :config
  (show-paren-mode t))


(use-package paredit
  :ensure t
  :diminish " Pe"
  :hook ((emacs-lisp-mode . paredit-mode)
         (ielm-mode . paredit-mode)
         (sly-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)))


(use-package pdf-tools
  :ensure t
  :hook ((after-init . pdf-loader-install)))


(use-package platformio-mode
  :ensure t
  :hook ((c++-mode . platformio-conditionally-enable)))


;; (use-package persp-mode
;;   :ensure t
;;   :hook ((after-init . persp-mode))
;;   :config
;;   (persp-def-auto-persp "erc" :mode 'erc-mode :hooks '(after-switch-to-buffer-functions)))


(use-package prettier-js
  :ensure t
  :hook (web-mode . prettier-js-mode))


(use-package prog-mode
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . my-add-whitespace-hook)
         (prog-mode . my-prog-auto-fill)))


(use-package projectile
  :ensure t
  :after helm
  :commands (projectile-project-root)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode t)
  :custom
  (projectile-mode-line-prefix " P"))


(use-package python
  :config
  (setq python-indent-offset 4))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package raku-mode
  :ensure t
  :defer t)


(use-package restclient
  :ensure t
  :commands (restclient-mode))


(use-package recentf
  :config
  (setq recentf-max-saved-items 200))


(use-package rjsx-mode
  :ensure t
  :mode (rx (or ?c ?C) "omponents/" (* nonl) ".js" (? ?x) eos))


(use-package rmsbolt
  :ensure t)


(use-package robe
  :ensure t
  :after company
  :hook (enh-ruby-mode . robe-mode)
  :init
  (add-to-list 'company-backends 'company-robe))


(use-package rubocop
  :ensure t
  :hook (enh-ruby-mode . rubocop-mode)
  :config
  (setq rubocop-autocorrect-on-save t))


(use-package ruby-frozen-string-literal ; custom elisp
  :load-path "~/.emacs.d/elisp"
  :hook enh-ruby-mode)


(use-package savehist
  :hook (after-init . savehist-mode))


(use-package saveplace
  :config
  (save-place-mode))


(use-package scroll-bar
  :config
  (scroll-bar-mode 0))


(use-package shadowenv
  :ensure t
  :if darwin-p
  :hook (after-init . shadowenv-global-mode)
  :custom
  (shadowenv-lighter "S"))


(use-package shopify-dev  ; custom elisp
  :load-path "~/.emacs.d/elisp"
  :commands (shopify-dev-up
             shopify-dev-down
             shopify-dev-server))


(use-package simple
  :config
  (column-number-mode)  ; Show column in modeline
  (setq eval-expression-print-length nil)  ; print entire expression in scratch
  (setq save-interprogram-paste-before-kill t))



(use-package spin
  :load-path "~/src/github.com/Shopify/spin.el"
  :if darwin-p)


(use-package string-inflection
  :ensure t
  :defer t)


(use-package sly
  :ensure t
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))


(use-package sly-macrostep
  :ensure t
  :after sly)


(use-package smartparens
  :ensure t
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(lisp-mode) "`" "`" :actions nil))


;; (use-package smartscan
;;   :ensure t
;;   :config
;;   (global-smartscan-mode))


(use-package so-long   ; Prevent files with long lines from freezing emacs
  :if (version<= "27.1" emacs-version)
  :hook (after-init . global-so-long-mode))


(use-package sudo-edit
  :ensure t
  :commands (sudo-edit))


(use-package swiper-helm
  :ensure t
  :bind (("C-c o" . swiper-helm)))


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


(use-package sysctl
  :ensure t
  :commands (sysctl))


(use-package tab-bar
  :if (version<= "27" emacs-version)
  :bind (("C-x t >" . tab-bar-history-forward)
         ("C-x t <" . tab-bar-history-back))
  :init
  (tab-bar-mode)
  (tab-bar-history-mode))


(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))


(use-package text-mode
  :hook (text-mode . (lambda () (setq truncate-lines nil))))


(use-package tide
  :ensure t
  :hook ((rjsx-mode . tide-setup)
         (rjsx-mode . tide-hl-identifier-mode)))


(use-package tool-bar
  :config
  (tool-bar-mode 0))


(use-package transient-z   ; custom elisp
  :load-path "~/.emacs.d/elisp"
  :bind ("C-z" . transient-z))


(use-package treemacs
  :ensure t
  :bind (("C-c n" . treemacs)))


(use-package treemacs-projectile
  :after treemacs
  :ensure t)

(use-package typescript-mode
  :ensure t
  :defer t)


(use-package undo-tree
  :ensure t
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree")))
  (setq undo-tree-mode-lighter " UT"))    ; Shorten Undo-Tree's line mode indicator


(use-package unfill
  :ensure t
  :defer t)


(use-package visual-regexp
  :ensure t
  :bind (("C-c v r" . vr/replace)
         ("C-c v q" . vr/query-replace)
         ("C-c v m" . vr/mc-mark)))


(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 5000)
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-exit-functions
        (lambda (_buffer _signal)
          (when (cdr (window-list))
            (delete-window)))))


(use-package vterm-toggle
  :ensure t
  :bind (("<f6>" . vterm-toggle)
         ("C-<f6>" . vterm-toggle-cd)
         :map vterm-mode-map
         ("<f6>" . my-vterm-toggle-or-cd))
  :config
  (setq vterm-toggle-cd-auto-create-buffer t))


(use-package web-mode
  :ensure t
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.tsx?\\'")
  :hook ((web-mode . (lambda() (setq-local tab-width 2)))
         (web-mode . my-web-mode-tide-setup))
  :init
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-auto-close-style 2))


(use-package wgrep
  :ensure t
  :defer t)


(use-package wgrep-ag
  :ensure t
  :defer t)


(use-package which-key
  :ensure t
  :diminish ;; " WK"
  :hook (after-init . which-key-mode)
  :init
  (which-key-setup-side-window-right-bottom))


(use-package windmove
  :hook (after-init . windmove-default-keybindings))


(use-package whole-line-or-region
  :ensure t
  :config
  (whole-line-or-region-global-mode 1)
  ;; Can't diminish using :diminish, symbol is different
  (diminish 'whole-line-or-region-local-mode ""))


(use-package workgroups2
  :ensure t
  :config
  (setq wg-prefix-key (kbd "C-c z"))
  (workgroups-mode 1))


(use-package xclip
  :ensure t
  :config
  (xclip-mode 1))


(use-package xr
  :ensure t)


(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")


(use-package yasnippet
  :ensure t
  :diminish (yas-minor-mode . "")
  :hook (after-init . yas-global-mode))


(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)


(use-package zig-mode
  :ensure t)


(use-package znc   ; Custom elisp, patched fork
  :load-path "~/.emacs.d/elisp"
  :commands (znc-erc))



;; Variables with no package

;; (setq visible-bell t) ; disable computer beep
(setq ring-bell-function 'ignore)
(setq-default indent-tabs-mode nil)
(setq inhibit-splash-screen t)
(setq-default tab-width 4)
(setq require-final-newline t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying t     ; Don't mess up hard links
      version-control t       ; Use version numbers on backups
      delete-old-versions t   ; Don't keep everything for all time
      kept-new-versions 10    ; How many new versions to keep
      kept-old-versions 5)    ; How many old
(setq frame-resize-pixelwise t) ; Pixel perfect window resize
(setq inhibit-x-resources t)  ; Fix emacsclient issues
(setq initial-scratch-message nil) ; Start scratch buffer empty
(setq sentence-end-double-space nil) ; Only one space between senteces for auto-fill.
(setq echo-keystrokes 0.1) ; Don't wait long before showing keystrokes
(setq-default auto-hscroll-mode 'current-line) ; Only horizontally scroll the current line
(setq-default truncate-lines t) ; Truncate lines instead of wrapping
(setq word-wrap t) ; Word-wrap instead of wrapping in the middle of words


;; MacOS titlebar
(when-darwin
 (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
 (setq ns-use-proxy-icon  nil))
;; (setq frame-title-format nil)


;; Set the initial frame size
;; (when window-system
;;   (set-frame-size (selected-frame) 120 40))

;;; Mode line

(defvar my-global-mode-string-line
  '(:eval (if (display-graphic-p) "  [" "- ["))
  "Add space between the mode string and global mode stringg.")

;; ;; (setf (nthcdr 2 mode-line-format) (cons "%I " (nthcdr 2 mode-line-format))) ; Format default string
;; (unless darwin-p
;;   (setf (car global-mode-string) my-global-mode-string-line) ; Add bracket to beginning
;;   (add-to-list 'global-mode-string "]"  t)) ; Add closing bracket

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

(defun align-non-space (BEG END)
  "Align non-space columns in region BEG END."
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\S-+" 1 1 t)
  (save-excursion
    (goto-char END)
    (forward-line)
    (indent-region BEG (point))))

(defun my-web-mode-tide-setup ()
  "Initialize tide-mode in web-mode when required."
  (when (string-match-p "tsx?" (file-name-extension buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

(defun my-copy-unfilled (start end)
  "Copy region defined by START END unfilled."
  (interactive "r")
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring old-buffer start end)
      (unfill-region (point-min) (point-max))
      (kill-region (point-min) (point-max))))
  (message "Region unfilled and copied"))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY.
https://lists.gnu.org/archive/html/help-gnu-emacs/2008-06/msg00087.html"
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

(defun my-prog-auto-fill ()
  "Auto fill only comments in `prog-mode'. Used as a hook."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode))

(defun my-vterm-toggle-or-cd ()
  "If vterm was just opened, call `vterm-toggle-insert-cd`, otherwise toggle."
  (interactive)
  (if (eq last-command 'vterm-toggle)
      (vterm-toggle-insert-cd)
    (vterm-toggle)))

(defun my-hugo-server ()
  "Run hugo server for current project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (display-buffer
     (make-comint "Hugo Server" "hugo" nil "-D" "server")))
  (browse-url "http://localhost:1313"))

(defun my-hugo-deploy ()
  "Run `./deploy.sh' in my hugo project."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (display-buffer
     (make-comint "Hugo Deploy" "sh" nil "./deploy.sh"))))

(defun my-insert-iso-date ()
  "Insert ISO 8601 formatted date.
Taken from http://ergoemacs.org/emacs/elisp_datetime.html"
  (interactive)
  (insert (concat
           (format-time-string "%Y-%m-%dT%T")
           ((lambda (x) (concat (substring x 0 3) ":" (substring x 3 5)))
            (format-time-string "%z")))))

(defun my-disable-tramp-company ()
  "Disable `company-mode' on TRAMP connections for performance reasons."
  (when (file-remote-p default-directory)
    (company-mode -1)))

(defun my-disable-company ()
  "Disable company-mode."
  (company-mode -1))

(defun my-lsp-install-save-hooks ()
  "Add hooks for lsp-mode."
  ;(add-hook 'before-save-hook #'lsp-format-buffer nil t)
  (add-hook 'before-save-hook #'lsp-organize-imports nil t))

(defun my-add-whitespace-hook ()
  "Add a hook to cleanup whitespace on save."
  ; previously used write-contents-functions, interesting hook.
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

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

(defun my-erc-switch-to-buffer-all ()
    "Lke erc-switch-to-buffer but show all buffers by default."
    (interactive)
    (erc-switch-to-buffer t))

(defun my-erc-clear-modified-buffers ()
  "Clears the modified buffersd notifications from the modeline."
  (interactive)
  (setq erc-modified-channels-alist '())
  (setq erc-modified-channels-object nil))

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

(bind-key "C-c k" #'bury-buffer)
(bind-key "#" #'endless/sharp emacs-lisp-mode-map)
(bind-key "#" #'endless/sharp lisp-mode-map)

;;; Old Hooks

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook 'my-delete-completion-buffer)

(unless darwin-p
  (set-face-attribute
   'default nil :inherit nil :height 92))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
;;; init.el ends here
