(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-archives '(
                         ("melpa" . "https://melpa.org/packages/")
                         ("celpa" . "https://celpa.conao3.com/packages/")
                         ("org"   . "https://orgmode.org/elpa/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")
                         )
      )

(if (package-installed-p 'use-package)
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

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

;; (bind-key "C-c C-c" 'comment-region)
;; (bind-key "C-c C-u" 'uncomment-region)

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

(setq recentf-exclude '(
                        ".*/.emacs.d/.*"
                        ".*/go/pkg/mod/.*"
                        ".*/.emacs.tmp/.*"
                        ".*/node_modules/.*"
                        ".*/vendor/.*"
                        ".*\\.el.gz$"
                        )
      recentf-save-file "~/.emacs.tmp/recentf")
(recentf-mode t)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(use-package editorconfig :delight
  :init (setq editorconfig--enable-20210221-testing t) ; Enable the testing branch to work around a bug causing too many reloads. Need to be in :init as it must be set before the package loads.
  :config (editorconfig-mode t))

(use-package undo-tree :delight
  :after hydra
  :demand
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist '((".*" . "~/.emacs.tmp/undo-tree")))
  (undo-tree-visualizer-timestamps t)
  :config
  (global-undo-tree-mode)
  :bind (:map undo-tree-map ("C-_" . hydra-undo-tree/undo-tree-undo))
  :hydra (hydra-undo-tree (:idle 2)
          "
_p_: undo  _n_: redo _s_: save _l_: load   "
          ("p"   undo-tree-undo)
          ("n"   undo-tree-redo)
          ("s"   undo-tree-save-history)
          ("l"   undo-tree-load-history)
          ("u"   undo-tree-visualize "visualize" :color blue)
          ("q"   nil "quit" :color blue))
  )

(use-package which-key :delight
  :config (which-key-mode t)
  )

(use-package flyspell :delight
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
(use-package lsp-mode :delight " LSP"
  :hook
  (lsp-after-open . lsp-origami-try-enable)
  (lsp-mode       . lsp-enable-which-key-integration)
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
                            ".*node_modules.*"
                            ))
  (lsp-enable-links nil)
  (lsp-keymap-prefix "C-c l") ; Set the keymap prefix. (Default to s-l.)

  (lsp-prefer-flymake nil)       ; Disable flymake in favor of flycheck.
  ;; (lsp-diagnostic-package :none) ; Disable lsp flycheck in favor of regular flycheck.
  (lsp-diagnostics-flycheck-default-level 'info)
  (lsp-diagnostic-clean-after-change t)
  (gc-cons-threshold (* 100 1024 1024)) ; Increase emacs' garbage collector limit to 100M. LSP is demanding.
  (read-process-output-max (* 3 1024 1024)) ; Increase the emacs' subprocesses max output to 3MB.
  ;; (lsp-eldoc-enable-hover nil) ;; Disable eldoc. Redundant with lsp-ui-doc.

  (lsp-auto-guess-root t)        ; Auto detect project root, based on projectile.
  (lsp-keep-workspace-alive t)
  (lsp-enable-imenu nil)
  ;; (lsp-lens-enable t)
  (lsp-signature-auto-activate nil)                    ; Don't auto pop the signatures as we have it already in eldoc and lsp-ui.
  ;; (lsp-completion-enable nil)                       ; Disable completion from LSP in favor of tide.
  (lsp-display-inline-image nil)                       ; Make sure we don't try to display graphics in the terminal.
  ;; (lsp-document-sync-method 'lsp--sync-incremental) ; Set the sync mode to incremental to (try to) improve performance.
  (lsp-document-sync-method 'lsp--sync-full)           ; Set the sync mode to full to (try to) avoid LSP getting lost all the time.
  :bind
  (:map lsp-mode-map
        ("C-c e"   . lsp-rename)
        )
  :config
  (lsp-enable-which-key-integration)
  :config
  (use-package lsp-ui ;; Overlay UI components for LSP.
    :custom
    (lsp-ui-doc-position       'top)
    ;; (lsp-ui-doc-header         t)
    (lsp-ui-doc-use-childframe nil)
    ;; (lsp-ui-doc-enable         t)
    (lsp-ui-sideline-ignore-duplicate t)
    (lsp-ui-sideline-show-code-actions t)
    (lsp-ui-sideline-show-symbol t)
    (lsp-ui-sideline-show-hover t)
    (lsp-ui-doc-enable nil) ; lsp-ui-doc breaks the mouse resize.
    (lsp-ui-imenu-enable nil)

    ;; (lsp-ui-doc-include-signature t)
    ;; (lsp-ui-peek-fontify 'always)

    ;; For referemce:
    ;; (use-package lsp-ui
    ;;   :config
    ;;   (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
    ;;   (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
    ;;   (setq lsp-ui-sideline-enable nil
    ;;         lsp-ui-sideline-update-mode 'line
    ;;         lsp-ui-sideline-show-code-actions t
    ;;         lsp-ui-sideline-show-hover t
    ;;         lsp-ui-doc-enable nil
    ;;         lsp-ui-doc-include-signature t
    ;;         lsp-eldoc-enable-hover nil ; Disable eldoc displays in minibuffer
    ;;         lsp-ui-doc-position 'at-point
    ;;         lsp-ui-imenu-enable t
    ;;         lsp-ui-sideline-ignore-duplicate t))

    :bind
    (:map lsp-ui-flycheck-list-mode-map ;; Fix the terminal mode bindings.
          ("RET"   . lsp-ui-flycheck-list--view)
          ("TAB"   . lsp-ui-flycheck-list--visit)
          ("C-c l" . lsp-ui-flycheck-list--quit)
          )
    (:map lsp-ui-mode-map
          ;; ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
          ([remap xref-find-references]  . lsp-ui-peek-find-references)
          )
    )
  )

(use-package company :defer :delight
  :bind
  (:map company-active-map
        ("<tab>" . company-complete-common-or-cycle)
        ("TAB"   . company-complete-common-or-cycle)
        ("C-n"   . company-select-next)
        ("C-p"   . company-select-previous))
  (:map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))

  :custom
                                        ;(company-echo-delay 0)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)     ;; Show company after the first char typed.
  (company-tooltip-align-annotations t) ;; Align the completion popu.
  (company-show-numbers t)              ;; Easy navigation to candidates with M-<n>.
  (company-dabbrev-downcase nil)        ;; Don't worry about case.
  :hook
  (after-init . global-company-mode)

  :bind
  (:map company-mode-map
        ("TAB" . company-indent-or-complete-common))


  ;;:custom
  ;;(company-backends '()) ;; Clear the default backends.
  :config
  (use-package company-quickhelp
    :disabled
    :defines company-quickhelp-delay
    :bind (:map company-active-map
                ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.3)
    :config
    (use-package company-quickhelp-terminal
      :disabled
      ;:config
      ;(company-quickhelp-terminal-mode 1)
      )
    )

  ;; LSP completion.
  (use-package company-lsp
    :disabled
    :load-path "~/.emacs.files/libs"
    :preface
    ;; Work around from ht tps://github.com/tigersoldier/company-lsp/issues/145
    (defun lsp--sort-completions (completions)
      (lsp-completion--sort-completions completions))
    (defun lsp--annotate (item)
      (lsp-completion--annotate item))
    (defun lsp--resolve-completion (item)
      (lsp-completion--resolve item))
    :config
    (push 'company-lsp company-backends)
    :custom
    (company-lsp-cache-candidates t)
    (company-lsp-async t)
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t)
    )
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
  :after hydra
  :bind
  ("C-c h c"     . hydra-multiple-cursors/body)
  ("M-n"         . mc/mark-next-like-this)         ;; Add new cursor with matching region.
  ("M-p"         . mc/mark-previous-like-this)     ;; Add new cursor with matching region.
  ("M-]"         . mc/mark-all-like-this)          ;; Add new cursor with matching region.
  ("C-c SPC"     . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  ("M-SPC"       . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  (:map mc/keymap ("C-y" . yank))
  :hydra (hydra-multiple-cursors (:hint nil)
                                 "
 Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Next     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
                                 ("l" mc/edit-lines :exit t)
                                 ("a" mc/mark-all-like-this :exit t)
                                 ("n" mc/mark-next-like-this)
                                 ("N" mc/skip-to-next-like-this)
                                 ("M-n" mc/unmark-next-like-this)
                                 ("p" mc/mark-previous-like-this)
                                 ("P" mc/skip-to-previous-like-this)
                                 ("M-p" mc/unmark-previous-like-this)
                                 ("|" mc/vertical-align)
                                 ("s" mc/mark-all-in-region-regexp :exit t)
                                 ("0" mc/insert-numbers :exit t)
                                 ("A" mc/insert-letters :exit t)
                                 ("<mouse-1>" mc/add-cursor-on-click)
                                 ;; Help with click recognition in this hydra
                                 ("<down-mouse-1>" ignore)
                                 ("<drag-mouse-1>" ignore)
                                 ("q" nil))
  )

(use-package nord-theme
  ;; :config (load-theme 'nord t)
  )

(use-package monokai-theme
  ;; :config (load-theme 'monokai t)
  )

(use-package gruvbox-theme
  :config (load-theme 'monokai t)
  )

(unless window-system
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

(use-package emacs
  :when window-system
  :config
  (set-frame-font "Fira Code 12" nil t)
)

(use-package emacs
  :when window-system
  :config
  (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-8")
)

(use-package fira-code-mode
  :when window-system
  :custom
  (fira-code-mode-disabled-ligatures '(":" "[]" "#{" "#(" "#_" "#_(" "x")) ; List of ligatures to turn off
  :hook (prog-mode . fira-code-mode)
  )

(setq compilation-always-kill   t
      compilation-scroll-output t)

;; (setq compilation-environment '("TERM=xterm-truecolor" "COLORTERM=truecolor"))
(setq compilation-environment '("TERM=dumb"))

;(use-package xterm-color
;  :preface
;  (defun my/compilation-color (proc)
;    (lv-message "Starting new compilation!")
;    ;; We need to differentiate between compilation-mode buffers
;    ;; and running as part of comint.
;    (when (eq (process-filter proc) 'compilation-filter)
;      ;; This is a process associated with a compilation-mode buffer.
;      ;; We may call `xterm-color-filter' before its own filter function.
;      (set-process-filter proc (lambda (proc string)
;                                 (funcall #'compilation-filter proc
;                                          (xterm-color-filter string))))))
;  ;:hook (compilation-start . my/compilation-color)
;  )

(use-package emacs
  :disabled ;; Try without for a while see if still needed.
  :bind
  ("C-c r"  . (lambda() (interactive) (save-some-buffers t) (recompile)))
  ("C-c k" . kill-compilation)
  )

(use-package smartparens :delight
  :disabled
  :hook (prog-mode . smartparens-mode)
  )

(use-package rainbow-delimiters :delight
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode :delight
  :custom (rainbow-x-colors nil)
  :hook (prog-mode . rainbow-mode))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

(use-package magit
  :bind ("M-g s" . magit-status))

;(use-package git-gutter-fringe
;  :config (global-git-gutter-mode t))

(use-package flycheck
  :after nvm ; flycheck needs various binaries from npm to setup js/ts linters.
  :ensure-system-package shellcheck
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  ;; NOTE: Disabled in favor of flycheck-tip.
  ;; :bind
  ;; ("C-c <up>"   . flycheck-next-error)     ; Ctrl-c up   to go to next error.
  ;; ("C-c <down>" . flycheck-previous-error) ; Ctrl-c down to go to previous error.
  :init
  ;; From https://www.flycheck.org/en/28/_downloads/flycheck.html (search for "shellcheck").
  (flycheck-define-checker sh-shellcheck ; Create a custom checker for shellcheck.
    "A shell script syntax and style checker using Shellcheck."
    :command ("shellcheck" "-f" "checkstyle" "-s" (eval (symbol-name sh-shell)) source)
    :modes sh-mode
    :error-parser flycheck-parse-checkstyle)
  :hook
  (sh-mode . flycheck-mode)                                   ; Enable flycheck in sh-mode.
  )

(use-package flycheck-tip
  :after (flycheck hydra)
  :bind
  (:map flycheck-mode-map
        ("C-c <up>"   . hydra-flycheck/flycheck-tip-cycle)         ; Ctrl-c up   to go to next error.
        ("C-c <down>" . hydra-flycheck/flycheck-tip-cycle-reverse) ; Ctrl-c down to go to previous error.
        ("C-c C-n"    . flycheck-tip-cycle)
        ("C-c C-p"    . flycheck-tip-cycle-reverse)
        )
  ("C-c h f" . hydra-flycheck/body)
  :hydra (hydra-flycheck
          (:pre (flycheck-list-errors)
                :post (quit-windows-on "*Flycheck errors*")
                :hint nil)
          "Errors"
          ("f" flycheck-error-list-set-filter "Filter")
          ("j" flycheck-tip-cycle "Next")
          ("k" flycheck-tip-cycle-reverse "Previous")
          ("gg" flycheck-first-error "First")
          ("G" (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
          ("q" nil))
  )

(use-package flycheck-projectile
  :after (projectile flycheck)
  ;:bind (:map flycheck-mode-map
  ;            ("C-c l" . flycheck-projectile-list-errors)
  ;            )
  )

(use-package yasnippet
  :delight yas-minor-mode
  :config
  ;(add-to-list 'yas-snippet-dirs "~/.dotfiles/.emacs.files/yasnippet")
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
         ("\\.setup.*\\'" . conf-space-mode)))

(use-package json-mode :defer)
(use-package yaml-mode :defer
  :hook (yaml-mode . display-line-numbers-mode))

(use-package gitattributes-mode :defer)
(use-package gitconfig-mode :defer)
(use-package gitignore-mode :defer)

(use-package markdown-mode :defer
  :commands (markdown-mode gfm-mode)
  :mode "\\.md\\'")

(use-package grip-mode
  :when window-system
  :ensure-system-package (grip . "pip3 install grip")
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(use-package dockerfile-mode :defer
  :mode "Dockerfile" "\\'Dockerfile."
  :hook (dockerfile-mode . display-line-numbers-mode))
(use-package docker-compose-mode)

(use-package makefile-mode :defer :ensure nil
  :mode "Makefile" "\\.mk\\'"
  :hook (makefile-mode . display-line-numbers-mode))

(use-package feature-mode :defer)

(use-package protobuf-mode
  :hook
  (protobuf-mode . (lambda() (c-add-style "pbstyle" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))
  (protobuf-mode . display-line-numbers-mode)
  (protobuf-mode . yas-minor-mode))

(use-package terraform-mode :defer
  :hook
  (terraform-mode . yas-minor-mode)
  (terraform-mode . terraform-format-on-save-mode)
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
   ;; Other language servers, to be tested.
   (yaml-language-server       . "npm install --global yaml-language-server")
   (bash-language-server       . "npm install --global bash-language-server")
   (html-languageserver        . "npm install --global vscode-html-languageserver-bin")
   (vscode-json-languageserver . "npm install --global vscode-json-languageserver")

   ;; Not needed by emacs, but might as well put it somewhere as it is quite useful.
   (npm-check-updates . "npm install --global npm-check-updates")
   (yarn              . "npm install --global yarn")
   (jsdoc             . "npm install --global jsdoc jsdoc-mermaid jsdoc-tsimport-plugin tsd-jsdoc tui-jsdoc-template")
   (nodemon           . "npm install --global nodemon")
   (create-react-app  . "npm install --global create-react-app")
   )
  :config
  (nvm-use "14") ; NOTE: The nvm package doesn't support "latest" or "--lts".
  )

(use-package tide
  :after (nvm web-mode typescript-mode company flycheck prettier)
  :bind
  (:map tide-mode-map
        ;; ("<backtab>"      . company-tide)
        ([C-down-mouse-1] . mouse-drag-region)
        ([C-mouse-1]      . tide-jump-to-definition)
        ("<f1>"           . tide-documentation-at-point)
        ("C-c e"          . tide-rename-symbol)
        ("C-c o"          . tide-organize-imports)
        ([remap xref-find-definitions] . tide-jump-to-definition)
        ([remap xref-find-references]  . tide-references)
        )
  :config
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'jsx-tide 'javascript-eslint 'append)
  :custom
  (tide-completion-detailed t)
  (tide-project-cleanup-delay 3600)
  :hook
  (web-mode    . tide-setup)                ; Start with web-mode.
  ;; (tide-mode   . git-gutter-mode)           ; Git info in the maring.
  ;; (tide-mode   . npm-mode)                  ; Set the npm mode to easily run scripts from package.json.
  (tide-mode   . flycheck-mode)             ; Enable flycheck.
  ;; (tide-mode   . display-line-numbers-mode) ; Show line numbers.
  (before-save . tide-format-before-save)   ; NOTE: tide-format-before-save checks that we are in tide-mode, so it is safe to hook to the global 'before-save.
  ;; (tide-mode   . lsp-deferred)           ; Start LSP.
  )

(use-package js2-mode
  ;; :mode "\\.js$"
  :custom
  (js2-global-externs (list "window" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "jQuery" "$"))
  ;; :hook
  ;; (js2-mode . setup-tide-mode)
  )

(use-package typescript-mode
  :after nvm
  ;; :mode "\\.ts$"
  ;; :hook
  ;; (typescript-mode . setup-tide-mode)
  )


(use-package web-mode
  :after flycheck
  :mode "\\.js$" "\\.jsx$" "\\.ts$" "\\.tsx$"
  :bind
  (:map web-mode-map
        ("C-c C-l" . display-line-numbers-mode)
        ([mouse-2] . web-mode-fold-or-unfold)
        )
  :custom
  ;; TODO: Document this.
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-comment-style 2)

  ;; (web-mode-enable-block-face t)
  ;; (web-mode-enable-comment-keywords t)
  ;; (web-mode-enable-heredoc-fontification t)

  ;; Register the file extension we want to use with web-mode. (web-mode specific, does not overlap with :mode keyword).
  (web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'") ("tsx" . "\\.ts[x]?\\'")))

  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-pairing t)

  ;; Enable symbol highlight.
  ;; (web-mode-enable-current-element-highlight t)
  ;; (web-mode-enable-current-column-highlight t) ;; Conflicts with origami. See if lsp-fold helps.
  :config
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  :hook
  ;; (web-mode . lsp-deferred)
  (web-mode . tide-setup)
  ;; (web-mode . git-gutter-mode)           ; Git info in the maring.
  (web-mode . npm-mode)                  ; Set the npm mode to easily run scripts from package.json.
  ;; (web-mode . display-line-numbers-mode) ; Show line numbers.
  (before-save . (lambda ()
                   (when (eq major-mode 'web-mode)
                     ;; (lsp-format-buffer)
                     ;; (lsp-organize-imports)
                     )
                   )
               )
  )

(use-package prettier :delight
  :after nvm
  :config
  (global-prettier-mode t)
  )

(use-package go-mode
  :ensure-system-package
  (gopls . "cd /tmp && GO111MODULE=on go get golang.org/x/tools/gopls@latest")

  :preface
  ;; Compilation helper funcs.
  (defun go-save-and-compile-program()
    "Save any unsaved buffers and compile."
    (interactive)
    (save-some-buffers t)
    (compile "sh -c 'go build -o /tmp/a.out && /tmp/a.out'")
    )
  (defun go-save-and-go-generate()
    (interactive)
    (save-some-buffers t)
    (compile "go generate")
    )
  (defun go-save-and-wire()
    (interactive)
    (save-some-buffers t)
    (compile "wire")
    )
  (defun go-save-and-vendor()
    (interactive)
    (save-some-buffers t)
    (compile "sh -c 'go mod tidy && go mod vendor'")
    )
  (defun go-save-and-test-program()
    "Save any unsaved buffers and compile."
    (interactive)
    (save-some-buffers t)
    (compile "go test -v -failfast -cover -coverprofile=/tmp/coverprofile -covermode=count")
    )

  :bind
  (:map go-mode-map
    ([mouse-8] . xref-pop-marker-stack)
    ([mouse-9] . godef-jump)
    ("C-c f"   . go-save-and-compile-program)
    ("C-c g"   . go-save-and-go-generate)
    ("C-c w"   . go-save-and-wire)
    ("C-c i"   . gofmt)
    ("C-c t"   . go-save-and-test-program)
    ("C-c c"   . (lambda() (interactive) (go-coverage "/tmp/coverprofile")))
    ([remap godef-describe]          . lsp-describe-thing-at-point)
    ([remap godef-jump]              . lsp-ui-peek-find-implementation)
    ([remap godef-jump-other-window] . go-guru-definition-other-window)
    ([remap go-rename]               . lsp-rename)
    )

  :config
  (use-package go-guru) ;; Enable =guru= support.
  :config
  (setq
    gofmt-command           "goimports"            ;; Use goimprots instead of gofmt.
    gofmt-args              (quote ("-local=bitbucket.org/mlcloud,github.magicleap.com"))
    gofmt-show-errors       nil                    ;; Don't show errors. Use LSP instead.
    lsp-clients-go-library-directories (quote ("~/go/pkg/mod" ;; Ignore stdlib, go mod cache and go path from LSP.
                                                "~/goroot"
                                                "~/go"
                                                "~/go/src/google.golang.org"
                                                "~/go/src/golang.org"
                                                "~/go/src/gopkg.in"
                                                ))
    )

  :hook
  (go-mode     . lsp)                          ;; Load LSP.
  (go-mode     . yas-minor-mode)               ;; Enable yas.
  (before-save . (lambda()             ;; Format the code with LSP before save.
                   (when (eq major-mode 'go-mode)
                     (lsp-format-buffer)
                     (lsp-organize-imports))))         ;; Let LSP handle imports.
  )

(setq org-startup-indented 'f)
(setq org-directory "~/org")
(setq org-special-ctrl-a/e 't)
(setq org-default-notes-file (concat org-directory "/notes.org"))
;; (define-key global-map "\C-cc" 'org-capture)
(setq org-src-fontify-natively 't)
(setq org-src-tab-acts-natively t)
(setq org-src-window-setup 'current-window)

(use-package org)
 ; :ensure org-plus-contrib)
(use-package ox-hugo
  :after ox)

(use-package ox-gfm)

(use-package keyfreq
  ;; NOTE: The default file used to store the stats is ~/.emacs.keyfreq, which is good to be outside ~/.emacs.d as we want to keep it.
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t)
  :custom
  ;; Define the commands to exclude.
  (keyfreq-excluded-commands '(self-insert-command
                               forward-char
                               backward-char
                               previous-line
                               next-line))
  )

(defun my/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  ;(let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
  (setenv "SSH_AUTH_SOCK"
          (car (split-string
                (shell-command-to-string
                 "ls -t $(find /tmp/ssh-* -group $USER -name 'agent.*' 2> /dev/null) | head -1"))))
    ;;   (message
    ;;    (format "SSH_AUTH_SOCK %s --> %s"
    ;;            ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))))
    )

(run-with-timer 0 (* 60 10) 'my/ssh-refresh)
