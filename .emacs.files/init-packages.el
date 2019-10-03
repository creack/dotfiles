;;; Package --- Init

;;; Commentary:
;;;

;;; Code:


;; Add melpa sources to the package manager.
(setq package-archives '(("gnu" . "~/elpaclone")
                        ("melpa" . "https://melpa.org/packages/")))

;; Activate package management.
(package-initialize)

(require 'package)

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always set :ensure.
(setq use-package-always-ensure t)

;; Setyo the use-package extensions.
(use-package use-package-ensure-system-package) ;; Enable external binary dependencies (like godef, gocode, gogetdoc, etc).
(use-package delight)                           ;; Enable delight to manage the mode-line.
;;;

;; Generic settings.
(use-package emacs
  :init
  ;; Helper to toggle trailing whitespaces.
  (defun disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

  ;; Global defaults.
  (setq-default indent-tabs-mode         nil ;; Don't use tabs to indent.
		tab-width                8   ;; But maintain correct appearance.
		show-trailing-whitespace t   ;; Show trailing spaces.
		truncate-lines           t   ;; Disable line wrapping.
		)

  ;; Common settings.
  (setq
   ring-bell-function      'ignore ;; Disable the bell.
   initial-scratch-message nil     ;; Remove the default text within the scratch buffer.
   require-final-newline   t       ;; Enforce end of file new line.
   inhibit-startup-message t       ;; Disable the splash screen.
   vc-follow-symlinks      t       ;; Skip yes/no prompt when opening symlinks.

   font-lock-maximum-decoration t ;; Improve syntax highlight.

   ;; Store temp file outside current dir.
   temporary-file-directory       "~/.emacs.d/"
   backup-directory-alist         `((".*" . , temporary-file-directory))
   auto-save-file-name-transforms `((".*" , temporary-file-directory t))
   create-lockfiles nil
  )

  :bind
  ([mouse-4] . (lambda() (interactive) (scroll-down 5))) ;; Mouse wheel suuport.
  ([mouse-5] . (lambda() (interactive) (scroll-up 5)))   ;; Mouse wheel suuport.

  :config
  (global-font-lock-mode 1) ;; Enable syntax colors.
  (xterm-mouse-mode      1) ;; Enable nouse support.
  (column-number-mode    1) ;; Display current line/column.
  (line-number-mode      1) ;; Display current line/column.
  (menu-bar-mode         0) ;; Disbale the menu bar.

  (fset 'yes-or-no-p 'y-or-n-p) ; Yes/No shortcut.

  :hook
  (minibuffer-setup . disable-trailing-whitespace)
  (compilation-mode . disable-trailing-whitespace)
  )

(use-package monokai-theme
  :config
  (load-theme 'monokai t))

;;

;; Highlight matching ( [ { } ] ).
(use-package paren
  :config
  (show-paren-mode t)
  )

(use-package compile
  :defer
  :bind
  ("C-c r"   . (lambda() (interactive) (save-some-buffers t) (recompile))) ;; Restart the compilation bufer.
  ("C-c k"   . kill-compilation)

  :init
  (setq
   compilation-always-kill   t ;; Don't ask about killing current process before restarting.
   compilation-scroll-output t ;; Autoscroll compilation buffer.
   )
  )

;; Enable winner to manage window layouts.
(use-package winner
  :config
  (winner-mode 1))

;; Better undo.
(use-package undo-tree
  :init
  ;; Autosave the undo-tree history.
  (setq undo-tree-history-directory-alist `((".*" . ,temporary-file-directory))
        undo-tree-auto-save-history t
        )
  :config
  (global-undo-tree-mode)
  )

;; Enable editorconofig.
(use-package editorconfig
  :config
  (editorconfig-mode 1)
  )


;; Keep track of recent files.
(use-package recentf
  :init
  (setq recentf-exclude '("~/\\.emacs.d" "~/go/pkg/mod" "\\.log$")
        )
  :config
  (recentf-mode 1)
  )

;; Enable ivy.
(use-package ivy
  :bind
  ("C-s" . swiper)
  :init
  (setq ivy-use-virtual-buffers t   ;; Load recent files in the buffer list.
        ivy-extra-directories   nil ;; Hide . and .. in file list.
        swiper-action-recenter  t   ;; Keep the cusor centered when searching.
        )
  :bind
  (:map ivy-minibuffer-map
        ("RET" . ivy-alt-done) ;; Navigate to subdir rather than open the directory.
        )

  :config
  (ivy-mode 1)
  )

(use-package counsel
  :init
  (setq counsel-yank-pop-separator "\n────────\n" ;; Add a clear separation between yanks.
        )
  :config
  (counsel-mode 1)
  )

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (setq ivy-rich-path-style    'abbrev
        ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode 1)
  )

;; Major modes.

;; Use conf-space for .conf and .setup files.
(use-package conf-mode
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  )

(use-package plantuml-mode ;; TODO: Autodownload plantuml.jar.
  :defer
  :config
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  :mode "\\.puml\\'" "\\.uml\\'"
  )

(use-package dockerfile-mode
  :mode "Dockerfile" "\\'Dockerfile."
  :hook
  (dockerfile-mode . set-fci-mode)
  (dockerfile-mode . highlight-indent-guides-mode)
  (dockerfile-mode . display-line-numbers-mode)
  )

(use-package makefile-mode
  :ensure nil
  :mode "Makefile" "\\.mk\\'"
  :hook
  (makefile-mode . set-fci-mode)
  (makefile-mode . highlight-indent-guides-mode)
  (makefile-mode . display-line-numbers-mode)
  )

(use-package markdown-mode)
(use-package json-mode)
(use-package feature-mode)
(use-package terraform-mode)

(use-package protobuf-mode
  :hook
  (protobuf-mode . (lambda() (c-add-style "pbstyle" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))
  (protobuf-mode . set-fci-mode)
  (protobuf-mode . highlight-indent-guides-mode)
  (protobuf-mode . display-line-numbers-mode)
  )

(use-package yaml-mode
  :hook
  (yaml-mode . set-fci-mode)
  (yaml-mode . highlight-indent-guides-mode)
  (yaml-mode . display-line-numbers-mode)
  )
