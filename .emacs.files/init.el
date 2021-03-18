(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      package-enable-at-startup nil
      package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org"   . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  (eval-when-compile (require 'use-package)))

(setq use-package-always-ensure t)

(use-package benchmark-init :demand
  :hook (after-init . (lambda () ; Stop the benchmark and print result after the init.
                  (benchmark-init/deactivate)
                  (message "Loaded in %s" (emacs-init-time))
                  )))

(unless package-archive-contents
  (package-refresh-contents))

(use-package use-package-ensure-system-package)

(use-package delight)

(setq inhibit-startup-screen  t
      initial-scratch-message nil)

(tool-bar-mode   -1)
;(scroll-bar-mode -1)
(unless window-system (menu-bar-mode -1))

(setq ring-bell-function 'ignore)

(setq-default truncate-lines        t
              require-final-newline t)
(add-hook 'prog-mode-hook '(lambda() (setq show-trailing-whitespace t)))

(setq-default truncate-lines        t
              require-final-newline t)

(setq-default indent-tabs-mode nil
              tab-width        8)

(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(show-paren-mode t)

(column-number-mode t)
(line-number-mode t)

(transient-mark-mode t)

(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") (lambda() (interactive) (scroll-down 5)))
  (global-set-key (kbd "<mouse-5>") (lambda() (interactive) (scroll-up 5))))

(winner-mode t)

(global-eldoc-mode -1)

(bind-key "s-x" 'kill-region)
(bind-key "s-c" 'kill-ring-save)
(bind-key "s-v" 'yank)
(bind-key "s-z" 'undo)

(bind-key "C-c C-c" 'comment-region)
(bind-key "C-c C-u" 'uncomment-region)

(bind-key "C-c C-l" 'display-line-numbers-mode)

(setq backup-dir     "~/.emacs.d/.tmp/backup"
      auto-saves-dir "~/.emacs.d/.tmp/auto-saves/")
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

(setq recentf-exclude '(".*/.emacs.d/.*" ".*/go/pkg/mod/.*"))
(recentf-mode t)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

(fset 'yes-or-no-p 'y-or-n-p)

(setq vc-follow-symlinks t)

(use-package editorconfig :delight
  :init (setq editorconfig--enable-20210221-testing t) ; Enable the testing branch to work around a bug causing too many reloads. Need to be in :init as it must be set before the package loads.
  :config (editorconfig-mode t))

(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '((".*" . "~/.emacs.d/.tmp")))

(use-package undo-tree :delight
  :config (global-undo-tree-mode t))

(use-package which-key :delight
  :config (which-key-mode t))

(use-package flyspell :delight
  :ensure-system-package aspell
  :hook
  ((org-mode yaml-mode markdown-mode git-commit-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (before-save-hook . flyspell-buffer)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  :config (use-package flyspell-correct-ivy
    :bind ("C-M-:" . flyspell-correct-at-point)
    :config (setq flyspell-correct-interface #'flyspell-correct-ivy)))

(use-package lsp-mode :delight " LSP"
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
  (lsp-prefer-flymake nil)   ;; Disable flymake in favor of flycheck.
  (lsp-eldoc-enable-hover t) ;; Disable eldoc. Redundant with lsp-ui-doc.
  ;(lsp-gopls-build-flags ["-tags=wireinject"]) ;; Use wire build tag.
  ;:config
  ;(lsp-register-custom-settings '(
  ;                                 ("gopls.completeUnimported" t t)
  ;                                 ("gopls.staticcheck" t t)
  ;                                 ))
  (use-package lsp-ui ;; Overlay UI components for LSP.
    :preface
    (defun creack/toggle-lsp-ui-doc ()
      (interactive)
      (if lsp-ui-doc-mode
        (progn
          (lsp-ui-doc-mode -1)
          (lsp-ui-doc--hide-frame))
        (lsp-ui-doc-mode 1)))

    :custom
    (lsp-ui-doc-position       'top)
    (lsp-ui-doc-header         t)
    (lsp-ui-doc-use-childframe t)
    (lsp-ui-doc-enable         t)

    :bind
    (:map lsp-ui-flycheck-list-mode-map ;; Fix the terminal mode bindings.
      ("RET"     . lsp-ui-flycheck-list--view)
      ("TAB"     . lsp-ui-flycheck-list--visit)
      ("C-c l"   . lsp-ui-flycheck-list--quit)
      )
    (:map lsp-ui-mode-map
      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
      ([remap xref-find-references]  . lsp-ui-peek-find-references)
      ("C-c d"   . creack/toggle-lsp-ui-doc)
      ("C-c C-d" . creack/toggle-lsp-ui-doc)
      )
    )
  )

(use-package company :defer :delight
  :bind
  (:map company-active-map
   ("C-n" . company-select-next)
   ("C-p" . company-select-previous)
   ("<tab>" . company-complete-common-or-cycle)
   :map company-search-map
   ("C-p" . company-select-previous)
   ("C-n" . company-select-next))

  :custom
  ;(company-echo-delay 0)
  (company-idle-delay 0)                ;; Show company right away when prefix match.
  (company-minimum-prefix-length 1)     ;; Show company after the first char typed.
  (company-tooltip-align-annotations t) ;; Align the completion popu.
  (company-show-numbers t)              ;; Easy navigation to candidates with M-<n>.
  (company-dabbrev-downcase nil)        ;; Don't worry about case.

  :hook
  (after-init . global-company-mode)

  :config
  ;; Show quick tooltip
  (use-package company-quickhelp
    :defines company-quickhelp-delay
    :bind (:map company-active-map
            ("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode . company-quickhelp-mode)
    :custom (company-quickhelp-delay 0.3))

  ;; Lsp completion
  (use-package company-lsp
    :custom
    (company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
    (company-lsp-async t)
    (company-lsp-enable-snippet t)
    (company-lsp-enable-recompletion t)))

(use-package projectile
  :config (projectile-mode +1)
  :delight '(:eval (concat " <" (projectile-project-name) ">")))

(setq ivy-use-virtual-buffers t)

(setq ivy-extra-directories nil)

(setq swiper-action-recenter t)

(use-package ivy :delight
  :config (ivy-mode t))

(use-package counsel :delight :after ivy
  :config (counsel-mode t))

(use-package ivy-rich :after ivy counsel
  :custom
  (ivy-rich-path-style    'abbrev)
  (ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode t))

(use-package prescient :after ivy
  :config (prescient-persist-mode t))
(use-package ivy-prescient :after prescient
  :config (ivy-prescient-mode t))

(use-package swiper)

(bind-keys
  ("C-c v p" . ivy-push-view)
  ("C-c v o" . ivy-pop-view)
  ("C-c v ." . ivy-switch-view)
  ("C-s"     . counsel-grep-or-swiper))
(bind-keys :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done))

(use-package multiple-cursors
  :bind
  ("M-n"         . mc/mark-next-like-this)         ;; Add new cursor with matching region.
  ("M-p"         . mc/mark-previous-like-this)     ;; Add new cursor with matching region.
  ("M-]"         . mc/mark-all-like-this)          ;; Add new cursor with matching region.
  ("C-c SPC"     . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  ("M-SPC"       . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  )

(use-package monokai-theme
  :config (load-theme 'monokai t))

(unless window-system
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

;(set-face-attribute 'default nil :font "Fira Code-12")
(set-frame-font "Fira Code 12" nil t)

(set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-8")

(use-package fira-code-mode :when window-system
  :custom
  (fira-code-mode-disabled-ligatures '(":" "[]" "#{" "#(" "#_" "#_(" "x")) ; List of ligatures to turn off
  :hook (prog-mode . fira-code-mode))

(setq compilation-always-kill   t
      compilation-scroll-output t)

(setq compilation-environment '("TERM=xterm-truecolor" "COLORTERM=truecolor"))

(use-package xterm-color
  :preface
  (defun my/compilation-color (proc)
    ;; We need to differentiate between compilation-mode buffers
    ;; and running as part of comint.
    (when (eq (process-filter proc) 'compilation-filter)
      ;; This is a process associated with a compilation-mode buffer.
      ;; We may call `xterm-color-filter' before its own filter function.
      (set-process-filter
       proc
       (lambda (proc string)
         (funcall 'compilation-filter proc
                  (xterm-color-filter string))))))
  :hook (compilation-start . my/compilation-color))

(bind-key "C-c r" '(lambda() (interactive) (save-some-buffers t) (recompile)))
(bind-key "C-c k" 'kill-compilation)

(use-package rainbow-delimiters :delight
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :custom (rainbow-x-colors nil)
  :hook (prog-mode . rainbow-mode))

(use-package git-timemachine
  :bind ("M-g t" . git-timemachine-toggle))

(use-package magit
  :bind ("M-g s" . magit-status))

;(use-package git-gutter-fringe
;  :config (global-git-gutter-mode t))

(use-package flycheck
  :ensure-system-package
  (shellcheck . "echo 'Missing shellcheck binary.' >&2; exit 1")
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled))
  :bind
  ("C-c <up>"   . flycheck-next-error)     ; Ctrl-c up   to go to next error.
  ("C-c <down>" . flycheck-previous-error) ; Ctrl-c down to go to previous error.
  :init
  ;; From https://www.flycheck.org/en/28/_downloads/flycheck.html (search for "shellcheck").
  (flycheck-define-checker sh-shellcheck ; Create a custom checker for shellcheck.
    "A shell script syntax and style checker using Shellcheck."
    :command ("shellcheck" "-f" "checkstyle" "-s" (eval (symbol-name sh-shell)) source)
    :modes sh-mode
    :error-parser flycheck-parse-checkstyle)
  :hook sh-mode ; Enable flycheck in sh-mode.
  )

(use-package yasnippet
  :delight yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/.emacs.files/yasnippet")
  (yas-global-mode t))

(use-package yasnippet-snippets)

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
  :ensure-system-package (grip . "pip3 install grip")
  :bind (:map markdown-mode-command-map
         ("g" . grip-mode)))

(use-package dockerfile-mode :defer
  :mode "Dockerfile" "\\'Dockerfile."
  :hook (dockerfile-mode . display-line-numbers-mode))
(use-package docker-compose-mode)

(use-package plantuml-mode :defer
  :ensure-system-package java
  :custom
  (plantuml-jar-path "~/.emacs.d/plantuml.jar")
  (plantuml-default-exec-mode 'jar)
  :mode ("\\.puml\\'" "\\.uml\\'")
  :config
  (unless (file-exists-p plantuml-jar-path)
    (plantuml-download-jar)))

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

(use-package go-guru)

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
    ("TAB"     . company-indent-or-complete-common)
    ("C-c e"   . lsp-rename)
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
  :config
  (keyfreq-mode t)
  (keyfreq-autosave-mode t))

(defun my/ssh-refresh ()
  "Reset the environment variable SSH_AUTH_SOCK"
  (interactive)
  (let (ssh-auth-sock-old (getenv "SSH_AUTH_SOCK"))
    (setenv "SSH_AUTH_SOCK"
            (car (split-string
                  (shell-command-to-string
                   "ls -t $(find /tmp/ssh-* -group $USER -name 'agent.*' 2> /dev/null) | head -1"))))
    (message
     (format "SSH_AUTH_SOCK %s --> %s"
             ssh-auth-sock-old (getenv "SSH_AUTH_SOCK")))))

(run-with-timer 0 (* 60 10) 'my/ssh-refresh)
