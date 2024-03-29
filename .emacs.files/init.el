;; -*- lexical-binding: t; -*-
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ;; ("celpa" . "https://celpa.conao3.com/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         )
      )

(package-refresh-contents)

(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package)
  )

(eval-when-compile
  (require 'use-package)
  )

(setq use-package-always-ensure t)

(use-package benchmark-init :demand
  :disabled ;; TODO: Re-enable this. Broken with emacs28.
  :hook (after-init . (lambda () ; Stop the benchmark and print result after the init.
                  (benchmark-init/deactivate)
                  (message "Loaded in %s" (emacs-init-time))
                  )))

(setq package-native-compile t
      warning-suppress-log-types '((comp))
      warning-suppress-types '((comp))
 ;;comp-deferred-compilation t ; Enable async compilation to avoid blocking.
 )

(use-package hydra
  :config
  (use-package use-package-hydra)
  )

(unless package-archive-contents
  (package-refresh-contents))

(use-package use-package-ensure-system-package
  :custom
  (async-shell-command-buffer 'new-buffer) ; Automatically open a new buffer instead of asking about it.
  )

(use-package delight)

(setq inhibit-startup-screen  t
      initial-scratch-message nil)

;; (tool-bar-mode   -1)
;; (scroll-bar-mode -1)
(unless window-system (menu-bar-mode -1))

(setq ring-bell-function 'ignore)

(add-hook 'prog-mode-hook #'(lambda() (setq show-trailing-whitespace t)))

(setq-default truncate-lines        t
              require-final-newline t)

;; (setq-default indent-tabs-mode nil
;;             tab-width        8)

;; (setq font-lock-maximum-decoration t)
;; (global-font-lock-mode t)
(show-paren-mode t)

(column-number-mode t)
(line-number-mode t)

;; (transient-mark-mode t)

(use-package emacs
  :unless window-system
  :config
  (xterm-mouse-mode t)
  :custom
  (mouse-wheel-scroll-amount '(5 ((shift) . hscroll) ((meta)) ((control) . vscroll)))
  ;; :bind
  ;; ("<mouse-4>" . (lambda() (interactive) (scroll-down 5)))
  ;; ("<mouse-5>" . (lambda() (interactive) (scroll-up 5)))
  )

(winner-mode t)

(use-package eldoc :delight
  :config
 (global-eldoc-mode 1)
  )

(setq kill-ring-max 200
      kill-do-not-save-duplicates t
      save-interprogram-paste-before-kill t)

(bind-key "s-x" 'kill-region)
(bind-key "s-c" 'kill-ring-save)
(bind-key "s-v" 'yank)
(bind-key "s-z" 'undo)

(bind-key "C-c C-c" 'comment-region)
(bind-key "C-c C-u" 'uncomment-region)

(bind-key "C-c C-l" 'display-line-numbers-mode)

(setq backup-dir     "~/.emacs.tmp/backup"
      auto-saves-dir "~/.emacs.tmp/auto-saves/")
(dolist (dir (list backup-dir auto-saves-dir))
  (when (not (file-directory-p dir))
    (make-directory dir t)))

(setq backup-directory-alist         `(("." . ,backup-dir))
      auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
      auto-save-list-file-prefix     (concat auto-saves-dir ".saves-")
      tramp-backup-directory-alist   `((".*" . ,backup-dir))
      tramp-auto-save-directory      auto-saves-dir)

(setq create-lockfiles nil           ; Don't create lockfiles when editing a file.
      backup-by-copying t            ; Don't delink hardlinks.
      delete-old-versions t          ; Clean up the backups.
      version-control t              ; Use version numbers on backups.
      kept-new-versions 5            ; Keep some new versions
      kept-old-versions 2)           ; and some old ones, too.

(use-package recentf
  :custom
  ;; Define the common patterns we don't want to keep track of.
  (recentf-exclude '(
                     ".*/\\.emacs\\.d/.*"
                     ".*/go/pkg/mod/.*"
                     ".*/\\.emacs\\.tmp/.*"
                     ".*/node_modules/.*"
                     ".*/vendor/.*"
                     ".*/build/.*"
                     ".*/tmp/.*"
                     ".*/\\.tmp/.*"
                     ".*/out/.*"
                     ".*\\.el\\.gz$"
                     ".*/node_modules/.*"
                     ))
  (recentf-save-file       "~/.emacs.tmp/recentf") ; Store the recentf list outside the default so we keep the list when we reset ~/.emacs.d.
  (recentf-auto-cleanup    'never)
  (recentf-max-menu-items  100)
  (recentf-max-saved-items 2000)
  :config
  (recentf-mode t)
  )

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(use-package editorconfig
  :init (setq editorconfig--enable-20210221-testing t) ; Enable the testing branch to work around a bug causing too many reloads. Need to be in :init as it must be set before the package loads.
  :config (editorconfig-mode t)
  )

(use-package undo-tree :delight
  ;; :after hydra
  :demand
  :custom
  (undo-tree-history-directory-alist '((".*" . "~/.emacs.tmp/undo-tree")))
  (undo-tree-auto-save-history       t)
  (undo-tree-visualizer-timestamps   t)
  :config
  (global-undo-tree-mode)
  ;; :bind
  ;; (:map undo-tree-map
  ;;       ("C-_" . hydra-undo-tree/undo-tree-undo)
  ;;       )
  ;;    :hydra
  ;;    (hydra-undo-tree
  ;;     (:idle 2)
  ;;     "
  ;;  _p_: undo  _n_: redo _s_: save _l_: load   "
  ;;     ("p" undo-tree-undo)
  ;;     ("n" undo-tree-redo)
  ;;     ("s" undo-tree-save-history)
  ;;     ("l" undo-tree-load-history)
  ;;     ("u" undo-tree-visualize "visualize" :color blue)
  ;;     ("q" nil "quit" :color blue)
  ;;     )
  )

(use-package which-key :delight
  :config (which-key-mode t)
  )

(use-package flyspell :delight
  :disabled
  :ensure-system-package aspell
  :hook
  ((web-mode org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  ;(before-save . flyspell-buffer)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config (use-package flyspell-correct-ivy
    ;:disabled
    :bind ("C-M-:" . flyspell-correct-at-point)
    :config (setq flyspell-correct-interface #'flyspell-correct-ivy)))

;(use-package helm-flex)
(use-package lsp-mode
  :delight " LSP"
  :bind
  (:map lsp-mode-map
        ("C-c e" . lsp-rename)
        )

  :custom
  (lsp-file-watch-ignored '(
                            "[/\\\\].git$"
                            "[/\\\\]infrastructure$"
                            "[/\\\\]vendor$"
                            "[/\\\\]cli$"
                            "[/\\\\]internal$"
                            "[/\\\\]functions[/\\\\]migrations$"
                            "[/\\\\]tests[/\\\\]mocks$"
                            "[/\\\\]\\.cache$"
                            "[/\\\\]\\.gocache$"
                            "[/\\\\]_archives$"
                            "[/\\\\]node_modules$"
                            ))
  ;; (lsp-enable-links nil)
  (lsp-keymap-prefix "C-c l") ; Set the keymap prefix. (Default to s-l.)

  (lsp-prefer-flymake nil) ; Disable flymake in favor of flycheck.

  (gc-cons-threshold (* 100 1024 1024))     ; Increase emacs' garbage collector limit to 100M. LSP is demanding.
  (read-process-output-max (* 3 1024 1024)) ; Increase the emacs' subprocesses max output to 3MB.

  ;; (lsp-auto-guess-root t)      ; Auto detect project root, based on projectile.
  (lsp-keep-workspace-alive t) ; Don't auto close workspace.

  (lsp-lens-enable t) ; Enable codelenses.

  :config
  (use-package lsp-ui ;; Overlay UI components for LSP.
    :bind
    (:map lsp-ui-flycheck-list-mode-map ;; Fix the terminal mode bindings.
          ("RET"   . lsp-ui-flycheck-list--view)
          ("TAB"   . lsp-ui-flycheck-list--visit)
          )
    (:map lsp-ui-mode-map
          ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references]  . lsp-ui-peek-find-references)
          )

    :custom
    (lsp-ui-sideline-ignore-duplicate t)

    ;; (lsp-ui-doc-enable            nil) ; lsp-ui-doc breaks the mouse resize.
    ;; (lsp-ui-doc-include-signature t)
    (lsp-ui-doc-position          'at-point)
    )

  :hook
  (lsp-after-open . lsp-origami-try-enable)
  (lsp-mode       . lsp-enable-which-key-integration)
  )



(use-package company :defer :delight
  :bind
  (:map company-active-map
        ("<tab>" . company-complete)
        ("TAB"   . company-complete)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous)
        )
  (:map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next)
        )
  (:map company-mode-map
        ("<backtab>" . company-complete)
        )

  :custom
  ;; (company-echo-delay 0)
  (company-idle-delay                0.2) ; Make the idle delay quick, but not instant.
  (company-minimum-prefix-length     2)   ; Show company after the first char typed.
  (company-tooltip-align-annotations t)   ; Align the completion popu.
  (company-show-numbers              t)	  ; Easy navigation to candidates with M-<n>.
  (company-dabbrev-downcase          nil) ; Don't worry about case.

  :hook
  (after-init . global-company-mode)
  )

(use-package projectile
  :config (projectile-mode +1)
  :delight '(:eval (concat " <" (projectile-project-name) ">"))
  :custom
  (projectile-completion-system 'ivy)
  (projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map)
)

(setq ivy-use-virtual-buffers t)

(setq ivy-extra-directories nil)

(setq swiper-action-recenter t)

(use-package ivy :delight
  ;:disabled
  :config (ivy-mode t))

(use-package counsel :delight :after ivy
  ;:disabled
  :config (counsel-mode t))

(use-package ivy-rich :after ivy counsel
  ;:disabled
  :custom
  (ivy-rich-path-style    'abbrev)
  (ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode t))

(use-package prescient :after ivy
  ;; :disabled
  :config (prescient-persist-mode t))
(use-package ivy-prescient :after prescient
  ;; :disabled
  :config (ivy-prescient-mode t))

(use-package swiper
  ;; :disabled
  )

(bind-keys
  ("C-c v p" . ivy-push-view)
  ("C-c v o" . ivy-pop-view)
  ("C-c v ." . ivy-switch-view)
  ("C-s"     . counsel-grep-or-swiper))
(bind-keys :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done))

(use-package multiple-cursors
  ;; :after hydra
  :bind
  ("C-c h c" . hydra-multiple-cursors/body)
  ("M-n"     . mc/mark-next-like-this)        ; Add new cursor with matching region.
  ("M-p"     . mc/mark-previous-like-this)    ; Add new cursor with matching region.
  ("M-]"     . mc/mark-all-like-this)         ; Add new cursor with matching region.
  ("C-c SPC" . set-rectangular-region-anchor) ; Rectangular region with many cursors.
  ("M-SPC"   . set-rectangular-region-anchor) ; Rectangular region with many cursors.
  (:map mc/keymap
        ("C-y" . yank)
        )
  ;;   :hydra
  ;;   (hydra-multiple-cursors
  ;;    (:hint nil)
  ;;    "
  ;;  Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
  ;; ------------------------------------------------------------------
  ;;  [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
  ;;  [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
  ;;  [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
  ;;  [_|_] Align with input CHAR       [Click] Cursor at point"
  ;;    ("l"   mc/edit-lines :exit t)
  ;;    ("a"   mc/mark-all-like-this :exit t)
  ;;    ("n"   mc/mark-next-like-this)
  ;;    ("N"   mc/skip-to-next-like-this)
  ;;    ("M-n" mc/unmark-next-like-this)
  ;;    ("p"   mc/mark-previous-like-this)
  ;;    ("P"   mc/skip-to-previous-like-this)
  ;;    ("M-p" mc/unmark-previous-like-this)
  ;;    ("|"   mc/vertical-align)
  ;;    ("s"   mc/mark-all-in-region-regexp :exit t)
  ;;    ("0"   mc/insert-numbers :exit t)
  ;;    ("A"   mc/insert-letters :exit t)
  ;;    ("<mouse-1>" mc/add-cursor-on-click)
  ;;    ;; Help with click recognition in this hydra
  ;;    ("<down-mouse-1>" ignore)
  ;;    ("<drag-mouse-1>" ignore)
  ;;    ("q" nil)
  ;;    )
  )

;; (use-package monokai-theme     :config (load-theme 'monokai t))
;; (use-package darkokai-theme    :config (load-theme 'darkokai t))
;; (use-package monokai-pro-theme :config (load-theme 'monokai t))
(use-package gruvbox-theme     :config (load-theme 'gruvbox t))
;; (use-package gruvbox-theme     :config (load-theme 'gruvbox-dark-hard t))

;; (use-package nord-theme
;; :config
;; (unless (daemonp)
;;   (load-theme 'nord t))
;; :hook
;; (server-after-make-frame . (lambda () (load-theme 'nord t)))
;; )

(unless window-system
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))
  )

(setq compilation-always-kill   t
      compilation-scroll-output t)

(setq compilation-environment '("TERM=xterm-256color" "COLORTERM=truecolor"))
;; (setq compilation-environment '("TERM=dumb"))

(use-package xterm-color
  :preface
  (defun creack/compilation-color (proc)
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint.
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter proc (lambda (proc string)
                                 (funcall #'compilation-filter proc
                                          (xterm-color-filter string)
                                          )
                                 )
                          )
      )
    )
  :hook
  (compilation-start . creack/compilation-color)
  )

(use-package emacs
  :bind
  ("C-c r"  . (lambda() (interactive) (save-some-buffers t) (recompile)))
  ("C-c k" . kill-compilation)
  )

(use-package smartparens :delight
  :disabled
  :hook (prog-mode . smartparens-mode)
  )

(use-package rainbow-delimiters :delight
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package rainbow-mode :delight
  :custom (rainbow-x-colors nil)
  :hook (prog-mode . rainbow-mode)
  )

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle)
  )

(use-package magit
  :bind ("M-g s" . magit-status)
  )

;(use-package git-gutter-fringe
;  :config (global-git-gutter-mode t))

(use-package flycheck
  :after nvm ; flycheck needs various binaries from npm to setup js/ts linters.
  :preface
  ;; Add buffer local Flycheck checkers after LSP for different major modes.
  ;; From: https://github.com/flycheck/flycheck/issues/1762#issuecomment-749789589
  (defvar-local creack/flycheck-local-cache nil)
  (defun creack/flycheck-local-checker-get (fn checker property)
    ;; Only check the buffer local cache for the LSP checker, otherwise we get
    ;; infinite loops.
    (if
        (eq checker 'lsp)
        (or
         (alist-get property creack/flycheck-local-cache)
         (funcall fn checker property)
         )
      (funcall fn checker property)
      )
    )
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :config
  (advice-add 'flycheck-checker-get
              :around 'creack/flycheck-local-checker-get)
  )

(use-package flycheck-popup-tip
  :after (flycheck)
  :config
  (flycheck-popup-tip-mode t)
  :bind
  (:map flycheck-mode-map
        ("C-c <up>"   . flycheck-tip-cycle)         ; Ctrl-c up   to go to next error.
        ("C-c <down>" . flycheck-tip-cycle-reverse) ; Ctrl-c down to go to previous error.
        ("C-c C-n"    . flycheck-tip-cycle)
        ("C-c C-p"    . flycheck-tip-cycle-reverse)
        ("C-c l"      . flycheck-list-errors)
        )
  ;; ("C-c h f" . hydra-flycheck/body)
  ;; :hydra
  ;; (hydra-flycheck
  ;;  (:pre (flycheck-list-errors)
  ;;        :post (quit-windows-on "*Flycheck errors*")
  ;;        :hint nil)
  ;;  "Errors"
  ;;  ("f" flycheck-error-list-set-filter "Filter")
  ;;  ("j" flycheck-tip-cycle "Next")
  ;;  ("k" flycheck-tip-cycle-reverse "Previous")
  ;;  ("gg" flycheck-first-error "First")
  ;;  ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ;;  ("q" nil)
  ;;  )
  )

(use-package flycheck-projectile
  :after (projectile flycheck)
  )

(use-package yasnippet
  :after company
  :delight yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/.emacs.files/yasnippet")
  (yas-global-mode t)
  )

(use-package yasnippet-snippets
  :after yasnippet
  )

(use-package ini-mode :defer)
(use-package ssh-config-mode :defer)
(use-package nginx-mode :defer)
(use-package conf-mode :defer
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  )

(use-package json-mode
  :hook
  (json-mode . (lambda() (editorconfig-apply)))
  (json-mode . lsp)
  )

(use-package yaml-mode
  :after (nvm) ; Load after nvm to maek sure we have the YAML LSP binary.
  :config
  (use-package flycheck-yamllint
    :after (flycheck)
    :ensure-system-package
    (yamllint . "pip3 install yamllint")
    :config
    (flycheck-yamllint-setup)
    :hook
    (yaml-mode . flycheck-mode)
    (lsp-managed-mode . (lambda () (when (derived-mode-p 'yaml-mode)
                                     (setq creack/flycheck-local-cache '((next-checkers . (yaml-yamllint))))
                                     )
                          ))
    )
  :hook
  (yaml-mode . display-line-numbers-mode)
  (yaml-mode . lsp)
  )

(use-package gitconfig :defer)

(use-package markdown-mode :defer
  :commands (markdown-mode gfm-mode)
  :mode "\\.md\\'"
  )

(use-package grip-mode
  :when window-system
  :ensure-system-package (grip . "pip3 install grip")
  :bind
  (:map markdown-mode-command-map
        ("g" . grip-mode))
  )

(use-package dockerfile-mode :defer
  :mode "Dockerfile" "\\'Dockerfile."
  :hook
  (dockerfile-mode . display-line-numbers-mode)
  )

(use-package mermaid-mode
  :mode ("\\.puml\\'" "\\.uml\\'" "\\.mermaid\\'")
  )

(use-package makefile-mode :defer :ensure nil
  :mode "Makefile" "\\.mk\\'"
  :hook (makefile-mode . display-line-numbers-mode)
  )

(use-package feature-mode :defer)

(use-package protobuf-mode
  :hook
  (protobuf-mode . (lambda() (c-add-style "pbstyle" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))
  (protobuf-mode . display-line-numbers-mode)
  (protobuf-mode . yas-minor-mode)
  )

(use-package terraform-mode :defer
  ;;:disabled
  ;;:config
  ;;(use-package company-terraform
  ;;:config (company-terraform-init))
  :hook
  ;;(terraform-mode . yas-minor-mode)
  (terraform-mode . terraform-format-on-save-mode)
  (terraform-mode . lsp)
  )

(use-package nvm
  :ensure-system-package
  (
   ;; Make sure the required packaes are installed.
   (tsc                        . "npm install --global typescript typescript-plugin-css-modules")
   (typescript-language-server . "npm install --global typescript-language-server")
   (tsserver                   . "npm install --global tide")
   (prettier                   . "npm install --global prettier prettier-plugin-jsdoc")
   (eslint                     . "npm install --global eslint eslint-plugin-jest eslint-plugin-prettier eslint-plugin-jsdoc eslint-formatter-gitlab @babel/core @babel/eslint-parser @babel/plugin-proposal-class-properties")
   (yaml-language-server       . "npm install --global yaml-language-server")
   (bash-language-server       . "npm install --global bash-language-server")
   ;; Other language servers, to be tested.
   (html-languageserver        . "npm install --global vscode-html-languageserver-bin")
   (vscode-json-languageserver . "npm install --global vscode-json-languageserver")

   ;; Not needed by emacs, but might as well put it somewhere as it is quite useful.
   (npm-check-updates . "npm install --global npm-check-updates")
   (yarn              . "npm install --global yarn")
   (jsdoc             . "npm install --global jsdoc jsdoc-mermaid jsdoc-tsimport-plugin tsd-jsdoc tui-jsdoc-template")
   (nodemon           . "npm install --global nodemon")
   (create-react-app  . "npm install --global create-react-app")
   (openapi           . "npm install --global @redocly/openapi-cli")
   (redoc-cli         . "npm install --global redoc-cli")
   )

  :config
  (nvm-use "14") ; NOTE: The nvm package doesn't support "latest" or "--lts".
  )

(use-package tide
  :after (nvm web-mode company flycheck prettier)
  :bind
  (:map tide-mode-map
        ([C-down-mouse-1] . mouse-drag-region)
        ([C-mouse-1]      . tide-jump-to-definition)

        ("<f1>"  . tide-documentation-at-point)
        ("C-c e" . tide-rename-symbol)
        ("C-c o" . tide-organize-imports)

        ([remap xref-find-definitions] . tide-jump-to-definition)
        ([remap xref-find-references]  . tide-references)
        )
  :preface
  (defun creack/tide-save-hooks ()
    (when (eq major-mode 'web-mode)
      (tide-format-before-save)
      (prettier-prettify)
      )
    (when (eq major-mode 'typescript-mode)
      (tide-format-before-save)
      (prettier-prettify)
      )
    )
  :config
  (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'tsx-tide        'javascript-eslint 'append)
  (flycheck-add-next-checker 'jsx-tide        'javascript-eslint 'append)
  :custom
  (tide-sync-request-timeout 15)
  (tide-completion-detailed t)
  (tide-project-cleanup-delay 3600)
  :hook
  (web-mode         . tide-setup)                ; Start with web-mode.
  (tide-mode        . flycheck-mode)             ; Enable Flycheck.
  (tide-mode        . prettier-mode)             ; Enable Prettier.
  (tide-mode        . tide-hl-identifier-mode)   ; Enable identifier highlight.
  (before-save      . creack/tide-save-hooks)    ; Before-save hook.
  )

(use-package typescript-mode
  :after (nvm flycheck prettier)
  :preface
  (defun creack/tide-save-hooks ()
    (when (eq major-mode 'typescript-mode)
      (tide-format-before-save)
      (prettier-prettify)
      )
    )
  (defun creack/typescript-prettify-save ()
    (interactive)
    (prettier-prettify)
    (save-buffer)
    )
  :bind
  (:map typescript-mode-map
        ("C-c f"   . creack/typescript-prettify-save)
        )
  ;; :hook
  ;; (typescript-mode . lsp)
  :hook
  (typescript-mode .
                   (lambda()
                     (tide-setup)
                     (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append))
                   )
  (tide-mode . flycheck-mode)                 ; Enable Flycheck.
  (tide-mode . prettier-mode)                 ; Enable Prettier.
  (typescript-mode . tide-hl-identifier-mode) ; Enable identifier highlight.
  (before-save     . creack/tide-save-hooks)  ; Before-save hook.
  )

(use-package web-mode
  :after flycheck
  :mode "\\.js$" "\\.jsx$" "\\.tsx$"
  :preface
  (defun creack/web-prettify-save ()
    (interactive)
    (prettier-prettify)
    (save-buffer)
    )
  :bind
  (:map web-mode-map
        ("C-c C-l" . display-line-numbers-mode)
        ([mouse-2] . web-mode-fold-or-unfold)
        ("C-c f"   . creack/web-prettify-save)
        )
  :custom
  ;; TODO: Document this.
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset    2)
  (web-mode-code-indent-offset   2)
  (web-mode-comment-style        2)

  ;; Register the file extension we want to use with web-mode. (web-mode specific, does not overlap with :mode keyword).
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'") ("tsx" . "\\.ts[x]?\\'")))

  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)

  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  :hook
  ;;(web-mode . lsp)
   (web-mode . tide-mode)
  )

(use-package emacs
  :disabled
  :preface
  (defun creack/lsp-register-eslint ()
    (flycheck-add-next-checker 'lsp 'javascript-eslint 'append)
    )
  :hook
  (lsp-after-open . creack/lsp-register-eslint)
  )

(use-package prettier
  :after nvm
                                        ;:config
                                        ;(global-prettier-mode t)
  )

(use-package go-mode
  :ensure-system-package
  (
   (gopls     . "cd /tmp && GO111MODULE=on go install golang.org/x/tools/gopls@latest")
   (gofumpt   . "cd /tmp && GO111MODULE=on go install mvdan.cc/gofumpt@latest")
   )

  :preface
  ;; Compilation helper funcs.
  (defun creack/go-save-and-compile-program()
    "Save any unsaved buffers and compile."
    (interactive)
    (save-some-buffers t)
    (compile "sh -c 'go build -tags local -o /tmp/a.out && /tmp/a.out'")
    )
  (defun creack/go-save-and-go-generate()
    (interactive)
    (save-some-buffers t)
    (compile "go generate")
    )
  (defun creack/go-save-and-wire()
    (interactive)
    (save-some-buffers t)
    (compile "wire")
    )
  (defun creack/go-save-and-vendor()
    (interactive)
    (save-some-buffers t)
    (compile "sh -c 'go mod tidy && go mod vendor'")
    )
  (defun creack/go-save-and-test-program()
    "Save any unsaved buffers and run the tests."
    (interactive)
    (save-some-buffers t)
    (compile "go test -v -failfast -cover -coverprofile=/tmp/coverprofile -covermode=count")
    )
  (defun creack/go-load-coverage()
    (interactive)
    (go-coverage "/tmp/coverprofile")
    )

  ;; LSP before-save hook.
  (defun creack/lsp-go-save-hooks ()
    (when (eq major-mode 'go-mode)
      (lsp-format-buffer)
      (lsp-organize-imports)
      )
    )

  :bind
  (:map go-mode-map
        ("C-c f"   . creack/go-save-and-compile-program)
        ("C-c t"   . creack/go-save-and-test-program)
        ("C-c c"   . creack/go-load-coverage)
        ("C-c w"   . creack/go-save-and-wire)
        ("C-c i"   . gofmt)
        ("C-c d"   . lsp-ui-doc-mode)
        ("C-c C-d" . lsp-ui-doc-glance)

        ([remap godef-describe] . lsp-describe-thing-at-point)
        ([remap godef-jump]     . lsp-ui-peek-find-implementation)
        ([remap go-rename]      . lsp-rename)

        ;; ([C-down-mouse-1] . mouse-drag-region)
        ;; ([C-mouse-1]      . xref-find-definitions)
        )

  :config
  (use-package go-guru) ; Enable =guru= support.

  :custom ; LSP settings.
  (lsp-go-codelenses
   '(
     (vendor . t)
     (upgrade_dependency . t)
     (tidy . t)
     (test . t)
     (regenerate_cgo . t)
     (generate . t)
     ;; (gc_details . t)
     )
   )
  (lsp-go-link-target      "pkg.go.dev")
  (lsp-go-links-in-hover   nil)
  ;; (lsp-go-use-placeholders t)
  (lsp-go-use-gofumpt      t)
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" . t)))

  :custom ; Go-mode settings.
  (godoc-reuse-buffer t)         ; Use a single buffer for godoc instead of one per call.
  (gofmt-command      "gofumpt") ; Use gofumpt instead of gofmt.
  (gofmt-show-errors   nil)      ; Don't show errors. Use LSP instead.

  :hook
  (go-dot-mod-mode . lsp) ; Load LSP.
  (go-mode         . lsp) ; Load LSP.
  (before-save     . creack/lsp-go-save-hooks) ; Use LSP to format / manage imports.

  :hook
  (go-mode . projectile-mode) ; Enable projectile.
  (go-mode . yas-minor-mode)  ; Enable yas.

  :hook ; Enable Flycheck and configure the checkers.
  (go-mode . flycheck-mode)
  (lsp-managed-mode . (lambda () (when (derived-mode-p 'go-mode)
                                   (setq creack/flycheck-local-cache '((next-checkers . (golangci-lint))))
                                   )
                        ))
  )

(use-package sh-mode :ensure nil
  :after (flycheck nvm) ; Depend on nvm to make sure we have the Bash LSP binary.
  :ensure-system-package shellcheck
  :init
  ;; From https://www.flycheck.org/en/28/_downloads/flycheck.html (search for "shellcheck").
  (flycheck-define-checker sh-shellcheck ; Create a custom checker for shellcheck.
    "A shell script syntax and style checker using Shellcheck."
    :command ("shellcheck" "-f" "checkstyle" "-s" (eval (symbol-name sh-shell)) source)
    :modes sh-mode
    :error-parser flycheck-parse-checkstyle
    )
  :hook
  (sh-mode . lsp)
  (sh-mode . flycheck-mode)
  (lsp-managed-mode . (lambda () (when (derived-mode-p 'sh-mode)
                                   (setq creack/flycheck-local-cache '((next-checkers . (sh-shellcheck))))
                                   ))
                    )
  )

(setq org-startup-indented      'f
      org-directory             "~/org"
      org-special-ctrl-a/e      't
      org-default-notes-file    (concat org-directory "/notes.org")
      org-src-fontify-natively  't
      org-src-tab-acts-natively t
      org-src-window-setup      'current-window)

(use-package org)
;; :ensure org-plus-contrib)
(use-package ox-hugo
  :after ox)

(use-package keyfreq
  ;; NOTE: The default file used to store the stats is ~/.emacs.keyfreq, which is good to be outside ~/.emacs.d as we want to keep it.
  :config
  (keyfreq-mode          t)
  (keyfreq-autosave-mode t)

  :custom
  ;; Define the commands to exclude.
  (keyfreq-excluded-commands
   '(self-insert-command
     forward-char
     backward-char
     previous-line
     next-line)
   )
  )

(defun creack/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  ;(let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
  (setenv "SSH_AUTH_SOCK"
          (car
           (split-string
            (shell-command-to-string
             "ls -t $(find /tmp/ssh-* -group $USER -name 'agent.*' 2> /dev/null) | head -1"
             )
            )
           )
          )
    ;;   (message
    ;;    (format "SSH_AUTH_SOCK %s --> %s"
    ;;            ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))))
    )

(run-with-timer 0 (* 60 10) 'creack/ssh-refresh)
