;;; init.el --- Personal Emacs config -*- lexical-binding: t -*-

;;; Commentary:
;; Modern use-package-based config. Bootstraps straight.el on first run.

;;; Code:

;; --------------------------------------------------------------------
;; Performance: defer GC during startup.
;; --------------------------------------------------------------------
(setq gc-cons-threshold (* 256 1024 1024)
      gc-cons-percentage 0.6
      read-process-output-max (* 1024 1024))
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 32 1024 1024)
                  gc-cons-percentage 0.1)))

;; --------------------------------------------------------------------
;; Package manager: straight.el + use-package.
;; --------------------------------------------------------------------
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; --------------------------------------------------------------------
;; Sensible defaults.
;; --------------------------------------------------------------------
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore
      use-short-answers t
      create-lockfiles nil
      make-backup-files nil
      auto-save-default nil
      require-final-newline t
      sentence-end-double-space nil
      vc-follow-symlinks t
      load-prefer-newer t)

(setq-default indent-tabs-mode nil
              tab-width 2
              fill-column 100)

;; Save every buffer, then re-run the last compilation (any mode).
(defun save-and-recompile ()
  "Save all buffers and re-run the last compilation."
  (interactive)
  (save-some-buffers t)
  (recompile))
(global-set-key (kbd "C-c r") #'save-and-recompile)

(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Mouse support in terminal Emacs (clicks + scroll wheel). No-op in GUI,
;; where the mouse already works.
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

(global-display-line-numbers-mode 1)
(column-number-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
(delete-selection-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(recentf-mode 1)

;; Keep customize out of init.el.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;; macOS niceties.
(when (eq system-type 'darwin)
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

;; --------------------------------------------------------------------
;; Theme + UI.
;; --------------------------------------------------------------------
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-gruvbox t)
  (doom-themes-org-config))

;; doom-modeline pulls in shrink-path, which is hosted on gitlab. Our global
;; git `insteadOf` rewrites gitlab https → ssh and we have no gitlab key, so the
;; clone fails and aborts the rest of init. Pin it to the github mirror, where
;; our ssh key works.
(use-package shrink-path
  :straight (shrink-path :type git :host github :repo "emacsmirror/shrink-path"))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 25))

(use-package nerd-icons)

;; --------------------------------------------------------------------
;; Completion: vertico + orderless + consult + marginalia.
;; --------------------------------------------------------------------
(use-package vertico
  :init (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init (marginalia-mode))

(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x p b" . consult-project-buffer)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-s r"   . consult-ripgrep)
         ("M-s l"   . consult-line)))

(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-cycle t))

;; --------------------------------------------------------------------
;; Editing.
;; --------------------------------------------------------------------
(use-package which-key
  :init (which-key-mode))

(use-package projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :bind (("C->"     . mc/mark-next-like-this)
         ("C-<"     . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; --------------------------------------------------------------------
;; Snippets: yasnippet + our own Go templates under .emacs.d/snippets.
;; --------------------------------------------------------------------
(use-package yasnippet
  :init (yas-global-mode 1)
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-reload-all))

(use-package yasnippet-snippets)

;; --------------------------------------------------------------------
;; LSP via eglot (built-in since Emacs 29).
;; --------------------------------------------------------------------
(use-package eglot
  :straight (:type built-in)
  :defer t
  :custom
  (eglot-autoshutdown t)
  (eglot-sync-connect 0)        ; non-blocking connect
  (eglot-events-buffer-size 0)  ; don't keep the chatty event log
  :config
  ;; gopls is the Go language server (brew install gopls).
  (add-to-list 'eglot-server-programs '((go-mode go-ts-mode) . ("gopls"))))

;; --------------------------------------------------------------------
;; Flymake — diagnostics backend used by eglot. Cycle errors with C-c arrows.
;; --------------------------------------------------------------------
(use-package flymake
  :straight (:type built-in)
  :bind (:map flymake-mode-map
              ("C-c <down>" . flymake-goto-next-error)
              ("C-c <up>"   . flymake-goto-prev-error)))

;; --------------------------------------------------------------------
;; Claude Code — run the CLI in an Emacs terminal, with MCP editor
;; integration so Claude sees the current buffer, region, and diagnostics
;; and can drive xref/ediff. Needs the `claude` CLI on PATH. C-c C-' opens
;; the transient menu. Uses the pure-elisp `eat` backend (no native module),
;; which works under emacsclient -t.
;; --------------------------------------------------------------------
(use-package eat
  :defer t)

(use-package claude-code-ide
  :straight (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu)
  :custom
  (claude-code-ide-terminal-backend 'eat)
  :config
  (claude-code-ide-emacs-tools-setup))

;; --------------------------------------------------------------------
;; EditorConfig — let .editorconfig drive indent / EOL / charset per file.
;; --------------------------------------------------------------------
(use-package editorconfig
  :init (editorconfig-mode 1))

;; --------------------------------------------------------------------
;; Languages.
;; --------------------------------------------------------------------
(use-package go-mode
  ;; tab-width / indent-tabs-mode come from .editorconfig.
  :hook ((go-mode . eglot-ensure)
         (go-mode . yas-minor-mode)
         (before-save . gofmt-before-save))
  :custom
  ;; Prefer goimports if installed (manages imports too); fall back to gofmt.
  (gofmt-command (or (executable-find "goimports") "gofmt"))
  :preface
  ;; Save every buffer first, then run the current package.
  (defun go-save-and-run ()
    "Save all buffers and run the current package."
    (interactive)
    (save-some-buffers t)
    (compile "go run ."))
  :bind (:map go-mode-map
              ("C-c f" . go-save-and-run)))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package yaml-mode)
(use-package json-mode)
(use-package dockerfile-mode)
(use-package typescript-mode)

;; --------------------------------------------------------------------
;; Flyspell (preserve old aspell setup).
;; --------------------------------------------------------------------
(use-package flyspell
  :straight (:type built-in)
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell"
          ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(provide 'init)
;;; init.el ends here
