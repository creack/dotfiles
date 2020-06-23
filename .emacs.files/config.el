(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq use-package-always-ensure t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(add-hook 'after-init-hook
          (lambda () (message "loaded in %s" (emacs-init-time))))

(setq gc-cons-threshold 10000000)

;; Restore after startup
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)
            (message "gc-cons-threshold restored to %S"
                     gc-cons-threshold)))

(defun find-config ()
  "Edit config.org"
  (interactive)
  (find-file "~/dotfiles/config.org"))

(global-set-key (kbd "C-c I") 'find-config)

(setq custom-file (make-temp-file "emacs-custom"))

(add-to-list 'load-path "~/.emacs.d/lisp/")

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(setq inhibit-startup-screen t)

(setq mac-command-modifier 'meta
      mac-option-modifier 'none)

(global-visual-line-mode 1)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq ring-bell-function 'ignore)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )

(setq-default indent-tabs-mode nil)

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package evil
  :config
  ;; (evil-mode 1)
  (evil-set-initial-state 'NeoTree 'emacs))

(use-package god-mode
  :disabled
  :bind (("<escape>" . god-local-mode)
         ("C-x C-1" . delete-other-windows)
         ("C-x C-2" . split-window-below)
         ("C-x C-3" . split-window-right)
         ("C-x C-0" . delete-window)))

(defun my-update-cursor ()
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'box
                        'bar)))

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(use-package goto-last-change
  :bind (("C-;" . goto-last-change)))

(use-package ivy
    :config
    (ivy-mode t))

(setq ivy-initial-inputs-alist nil)

(use-package counsel
  :bind (("M-x" . counsel-M-x)))

(use-package prescient)
(use-package ivy-prescient
  :config
  (ivy-prescient-mode t))

(use-package swiper
  :bind (("M-s" . counsel-grep-or-swiper)))

(use-package ivy-hydra)

(use-package major-mode-hydra
  :bind
  ("C-M-SPC" . major-mode-hydra)
  :config
  (major-mode-hydra-define org-mode
    ()
    ("Tools"
     (("l" org-lint "lint")))))

(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode))

(use-package undo-tree
  :defer 5
  :config
  (global-undo-tree-mode 1))

(use-package avy)

(use-package ace-window
   :config
   (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(defun mac-toggle-max-window ()
  (interactive)
  (set-frame-parameter
   nil
   'fullscreen
   (if (frame-parameter nil 'fullscreen)
       nil
     'fullboth)))

(use-package panda-theme
  :disabled
  :config
  (load-theme 'panda t))

(use-package solarized-theme
  :config
  (load-theme 'solarized-dark t))

(set-frame-font "Fira Code 12" nil t)
;; (set-frame-font "Inconsolata 13" nil t)
;; (set-frame-font "SF Mono 12" nil t)

(use-package feebleline
  :config
  (feebleline-mode 't))

(use-package emojify)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(use-package smartparens
  :config
  (add-hook 'prog-mode-hook 'smartparens-mode))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(add-hook 'prog-mode-hook 'electric-pair-mode)

(use-package fzf)

(use-package deadgrep)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package dumb-jump
  :bind (("C-M-g" . dumb-jump-go)
         ("C-M-p" . dumb-jump-back)
         ("C-M-q" . dumb-jump-quick-look)))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package git-gutter
  :config
  (global-git-gutter-mode 't))

(use-package flycheck
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'jc/use-eslint-from-node-modules)
  (add-to-list 'flycheck-checkers 'proselint)
  (setq-default flycheck-highlighting-mode 'lines)
  ;; Define fringe indicator / warning levels
  (define-fringe-bitmap 'flycheck-fringe-bitmap-ball
    (vector #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00011100
            #b00111110
            #b00111110
            #b00111110
            #b00011100
            #b00000000
            #b00000000
            #b00000000
            #b00000000
            #b00000000))
  (flycheck-define-error-level 'error
    :severity 2
    :overlay-category 'flycheck-error-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-error)
  (flycheck-define-error-level 'warning
    :severity 1
    :overlay-category 'flycheck-warning-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-warning)
  (flycheck-define-error-level 'info
    :severity 0
    :overlay-category 'flycheck-info-overlay
    :fringe-bitmap 'flycheck-fringe-bitmap-ball
    :fringe-face 'flycheck-fringe-info))

(flycheck-define-checker proselint
  "A linter for prose."
  :command ("proselint" source-inplace)
  :error-patterns
  ((warning line-start (file-name) ":" line ":" column ": "
            (id (one-or-more (not (any " "))))
            (message (one-or-more not-newline)
                     (zero-or-more "\n" (any " ") (one-or-more not-newline)))
            line-end))
  :modes (text-mode markdown-mode gfm-mode org-mode))

(use-package eglot
  :commands eglot
  :config
  (add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server" "--stdio"))))

(use-package yasnippet
    :config
    (add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
    (yas-global-mode 1))

(use-package yasnippet-snippets)

(setq-default js-indent-level 2)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (setq-default js2-ignored-warnings '("msg.extra.trailing.comma")))

(use-package js2-refactor
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package rjsx-mode)

(use-package prettier-js
  :config
  (setq prettier-js-args '(
                        "--trailing-comma" "es5"
                        "--single-quote" "true"
                        "--print-width" "100"
                        ))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

(use-package js-doc
  :bind (:map js2-mode-map
         ("C-c i" . js-doc-insert-function-doc)
         ("@" . js-doc-insert-tag))
  :config
  (setq js-doc-mail-address "jamiecollinson@gmail.com"
       js-doc-author (format "Jamie Collinson <%s>" js-doc-mail-address)
       js-doc-url "jamiecollinson.com"
       js-doc-license "MIT License"))

(defun jc/use-eslint-from-node-modules ()
  "Set local eslint if available."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(use-package add-node-modules-path)

(use-package web-mode
  :mode ("\\.html\\'")
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-engines-alist
        '(("django" . "focus/.*\\.html\\'")
          ("ctemplate" . "realtimecrm/.*\\.html\\'"))))

(use-package web-beautify
  :bind (:map web-mode-map
         ("C-c b" . web-beautify-html)
         :map js2-mode-map
         ("C-c b" . web-beautify-js)))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package haskell-mode)

(use-package hindent)

(use-package pyvenv)

(use-package blacken
  :config
  (add-hook 'python-mode-hook 'blacken-mode))

(use-package elixir-mode
  :config
  (use-package alchemist))

(use-package proof-general)

(use-package elm-mode
  :config
  (setq elm-format-on-save t))

(use-package csharp-mode)

(use-package rust-mode)

(setq org-startup-indented 'f)
(setq org-directory "~/org")
(setq org-special-ctrl-a/e 't)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cc" 'org-capture)
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-src-fontify-natively 't)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(let*
    ((base-font-color     (face-foreground 'default nil 'default))
     (headline           `(:foreground ,base-font-color)))

  (custom-theme-set-faces 'user
                          `(org-level-8 ((t (,@headline))))
                          `(org-level-7 ((t (,@headline))))
                          `(org-level-6 ((t (,@headline))))
                          `(org-level-5 ((t (,@headline))))
                          `(org-level-4 ((t (,@headline))))
                          `(org-level-3 ((t (,@headline :height 1.3))))
                          `(org-level-2 ((t (,@headline :height 1.3))))
                          `(org-level-1 ((t (,@headline :height 1.3 ))))
                          `(org-document-title ((t (,@headline :height 1))))))

(use-package writegood-mode
  :bind ("C-c g" . writegood-mode)
  :config
  (add-to-list 'writegood-weasel-words "actionable"))

(use-package notmuch)
