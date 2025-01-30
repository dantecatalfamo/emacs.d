;;; init.el --- Emacs init file  -*- lexical-binding: t -*-
;;; Commentary:
;; Remember to use `\\[set-selective-display]' (C-x $)
;;; Code:

;; Always download packages we need
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; MELPA
(use-package package
  :config
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))

(message "Starting Emacs...")

;; Load customizations
(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-exists-p custom-file)
    (load custom-file))


;; Custom functions
(defvar darwin-p (eq system-type 'darwin)
  "Is this machine darwin?")

(defmacro when-darwin (&rest body)
  "Evaluate BODY when the system is darwin."
  `(when (eq system-type 'darwin)
     ,@body))

(defun my-add-whitespace-hook ()
  "Add a hook to cleanup whitespace on save."
  ; previously used write-contents-functions, interesting hook.
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(defun my-prog-auto-fill ()
  "Auto fill only comments in `prog-mode'. Used as a hook."
  (setq-local comment-auto-fill-only-comments t)
  (auto-fill-mode))

(defun my-git-configure-user ()
  "Configure the current git repository with my details."
  (interactive)
  (shell-command (concat "git config --local user.name dantecatalfamo && git config --local user.email " user-mail-address))
  (message "Git configured with username dantecatalfamo and email %s" user-mail-address))


;; Package configuration
(use-package async
  :init
  (async-bytecomp-package-mode))


(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))


(use-package browse-at-remote
  :commands browse-at-remote)


(use-package cc-mode
  :ensure nil
  :defer t
  :config
  (setq-default c-basic-offset 4))


(use-package company
  :defer nil
  :bind (("C-<tab>" . company-complete))
  :config
  (setq company-idle-delay .15)
  (setq company-echo-delay 0)
  (setq company-show-quick-access t)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 2)
  (setq company-dabbrev-downcase nil)
  (setq company-transformers '(company-sort-by-occurrence
                               company-sort-by-backend-importance))
  :hook (after-init . global-company-mode))

(use-package consult
  :bind (("C-x b" . #'consult-buffer)
	     ("M-y" . #'consult-yank-from-kill-ring)
         ("C-c g" . #'consult-ripgrep)
         ("C-c i" . #'consult-imenu)
         ("C-c o" . #'consult-line)))


(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode))


(use-package descr-text
  :ensure nil
  :bind (("C-h T" . describe-char)))


(use-package diminish)


(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t))


(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tomorrow-night t))


(use-package eldoc
  :ensure nil
  :diminish)

(use-package emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers t)
  :init
  ;; (setq visible-bell t) ; disable computer beep
  (setq ring-bell-function 'ignore)
  (setq-default indent-tabs-mode nil)
  (setq-default show-trailing-whitespace t)
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
  (setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent
  (setq sentence-end-double-space nil) ; Only one space between senteces for auto-fill.
  (setq echo-keystrokes 0.1) ; Don't wait long before showing keystrokes
  (setq-default auto-hscroll-mode 'current-line) ; Only horizontally scroll the current line
  (setq-default truncate-lines t) ; Truncate lines instead of wrapping
  (setq word-wrap t) ; Word-wrap instead of wrapping in the middle of words
  (setq user-full-name "Dante Catalfamo")
  (setq user-mail-address "dante.catalfamo@gmail.com")

  ;; MacOS titlebar and emojis
  (when-darwin
   (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
   (setq ns-use-proxy-icon  nil)
   (set-fontset-font t '(#x1f000 . #x1faff) (font-spec :family "Apple Color Emoji")))

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (defvar my-global-mode-string-line
    '(:eval (if (display-graphic-p) "  [" "- ["))
    "Add space between the mode string and global mode stringg.")

  ;; ;; (setf (nthcdr 2 mode-line-format) (cons "%I " (nthcdr 2 mode-line-format))) ; Format default string
  ;; (unless darwin-p
  ;;   (setf (car global-mode-string) my-global-mode-string-line) ; Add bracket to beginning
  ;;   (add-to-list 'global-mode-string "]"  t)) ; Add closing bracket

  (setq frame-title-format '("%b@" (:eval (or (file-remote-p default-directory 'host) system-name)) " — Emacs"))

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
                                   mode-line-end-spaces)))



(use-package embark
  :bind (("C-." . embark-act)))


(use-package hl-line
  :config
  (global-hl-line-mode))


(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)))


(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))


(use-package flyspell
  :ensure nil
  :hook (text-mode . flyspell-mode))


(use-package frame
  :ensure nil
  :config
  (blink-cursor-mode t)) ; For emacsclient


(use-package gcmh
  :diminish
  :config
  (setq gcmh-high-cons-threshold (* 16 1024 1024)) ; 16mb
  (setq gcmh-idle-delay 5)
  (gcmh-mode))


(use-package gdb-mi
  :init
  (setq gdb-many-windows t))


(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . (lambda() (setq-local tab-width 4)))
         (before-save . gofmt-before-save)))


(use-package goto-addr
  :init
  (goto-address-mode))


(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-c C-d" . helpful-at-point)))


(use-package iedit
  :bind ("C-;" . iedit-mode))


(use-package isearch
  :ensure nil
  :custom
  (isearch-wrap-pause 'no)
  (isearch-lazy-count t))


(use-package imenu
  :ensure nil
  :commands imenu
  :init
  (setq imenu-auto-rescan t))


(use-package kmacro
  :ensure nil
  :bind (("C-x (" . kmacro-start-macro-or-insert-counter)
         ("C-x )" . kmacro-end-or-call-macro)))


(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
       ; (c-mode . my-lsp-conditionally-defer)
       ; (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (go-mode . my-lsp-install-save-hooks)
         (zig-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
       ; (web-mode . lsp-deferred)
       ; (web-mode . my-lsp-install-save-hooks)
         (typescript-mode . lsp-deferred)
         (typescript-mode . my-lsp-install-save-hooks))
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-file-watch-threshold 5000)
  (setq lsp-keymap-prefix "C-c s")
  (setq read-process-output-max (* 1024 1024)))


(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode))


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-diff-refine-hunk 'all
        magit-diff-refine-ignore-whitespace t))


(use-package marginalia
  :init
  (marginalia-mode))


(use-package midnight
  :ensure nil
  :config
  (add-to-list 'clean-buffer-list-kill-regexps "\\`\\*helpful ")
  (midnight-mode))


(use-package mouse
  :ensure nil
  :config
  (when (null (display-graphic-p))
    (xterm-mouse-mode))
  (context-menu-mode))


(use-package mwheel
  :ensure nil
  :config
  ;; Enable horizontal scrolling
  (setq mouse-wheel-tilt-scroll t)
  (setq mouse-wheel-flip-direction t))


(use-package ns-win ;; Only available on MacOS builds
  :ensure nil
  :if darwin-p
  :config
  ;; Use command key as meta in macos
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta))


(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil))


(use-package ox-ssh
  :ensure t
  :after org
  :config
  (setq org-ssh-header "IgnoreUnknown AddKeysToAgent,UseKeychain

Host *
  AddKeysToAgent yes
  UseKeychain yes
  IdentityAgent ~/.1password/agent.sock
"))



(use-package org
  :ensure nil
  ;; :defer nil
  ;; :ensure org-plus-contrib
  :hook ((org-mode . auto-fill-mode)
         (org-mode . my-add-whitespace-hook))
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link))
  :config
  (setq org-log-done 'time)             ; Can be 'time or 'note
  (setq org-directory "~/Org")
  (setq org-default-notes-file "~/Org/Notes.org")
  (setq org-capture-templates
        '(("t" "Todo (Notes)" entry (file "") "* TODO %?\n  %i\n  %a")
          ("n" "Note (Generic)" entry (file "") "* %?")))
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
  (setq org-adapt-indentation t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-src-tab-acts-natively nil)
  (setq org-edit-src-content-indentation 0)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t)
                                   (python . t)
                                   (ruby . t)
                                   (http . t)))
  (setq org-startup-folded t))


(use-package org-journal
  :defer
  :bind (:prefix "C-c j"
         :prefix-map my-org-journal-map
         ("j" . org-journal-new-entry)
         ("f" . my-org-journal-fleet))
  :init
  (defvar org-journal-dir nil)
  (setq org-journal-dir "~/Org/Journal/")
  (setq org-journal-file-type 'monthly))


(use-package package-lint
  :commands package-lint-current-buffer)


(use-package pdf-tools
  :ensure t
  :hook ((after-init . pdf-loader-install)))


(use-package prog-mode
  :ensure nil
  :hook ((prog-mode . display-line-numbers-mode)
         (prog-mode . my-add-whitespace-hook)
         (prog-mode . my-prog-auto-fill)))


(use-package python
  :ensure nil
  :defer t
  :config
  (setq python-indent-offset 4))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))


(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  :init
  (recentf-mode)
  (add-to-list 'midnight-hook #'recentf-save-list))


(use-package repo-helper
  :ensure nil
  :load-path "~/.emacs.d/elisp")


(use-package rmsbolt
  :defer t)


(use-package rubocop
  :hook (ruby-mode . rubocop-mode)
  :config
  (setq rubocop-autocorrect-on-save t))


(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode))


(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))


(use-package simple
  :ensure nil
  :config
  (column-number-mode)  ; Show column in modeline
  (setq eval-expression-print-length nil)  ; print entire expression in scratch
  (setq save-interprogram-paste-before-kill t))


(use-package sly
  :defer t
  :init
  (setq inferior-lisp-program "sbcl"))


(use-package sly-macrostep
  :ensure t
  :after sly)


(use-package sly-overlay
  :ensure t
  :after sly)


(use-package smartparens
  :diminish
  :hook (prog-mode . smartparens-mode)
  :config
  (sp-local-pair '(emacs-lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(emacs-lisp-mode) "`" "`" :actions nil)
  (sp-local-pair '(lisp-mode) "'" "'" :actions nil)
  (sp-local-pair '(lisp-mode) "`" "`" :actions nil))


(use-package so-long   ; Prevent files with long lines from freezing emacs
  :ensure nil
  :hook (after-init . global-so-long-mode))


(use-package solaire-mode
  :after doom-themes
  :config
  (solaire-global-mode))


(use-package tab-bar
  :ensure nil
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


(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree"))))


(use-package unfill
  :defer t
  :bind (("C-M-q" . unfill-paragraph)))


(use-package vertico
  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-multiform-mode)
  :custom
  (vertico-count 35)
  (vertico-cycle t))


(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"
         "\\.tsx?\\'")
  :hook ((web-mode . (lambda() (setq-local tab-width 2
                                           standard-indent 2)))
         ;(web-mode . my-web-mode-tide-setup)
         )
  :init
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-auto-close-style 2))


(use-package wgrep
  :defer t)


(use-package which-key
  :diminish ;; " WK"
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom)
  (setq which-key-idle-delay 2.0)
  (setq which-key-max-description-length 40))


(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode 1)
  ;; Can't diminish using :diminish, symbol is different
  (diminish 'whole-line-or-region-local-mode ""))


(use-package window
  :ensure nil
  :bind (("M-o" . #'other-window)))


(use-package xref
  :ensure nil
  :config
  (when (version<= "28.1" emacs-version)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read)))


(use-package yasnippet
  :diminish (yas-minor-mode . "")
  :hook (after-init . yas-global-mode))


(use-package zig-mode
  :ensure t
  :config
  (setq zig-format-on-save nil))



(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
;;; init.el ends here
