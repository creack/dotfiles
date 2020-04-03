;;; golang.el --- Golang configuration for emacs. ;;; -*- mode: elisp -*-

(use-package go-guru)

(use-package go-mode
  :ensure-system-package
  ((gogetdoc . "cd /tmp && GO111MODULE=on go get github.com/zmb3/gogetdoc@latest")
  (gopls    . "cd /tmp && GO111MODULE=on go get golang.org/x/tools/gopls@latest"))

  :init
  (global-unset-key (kbd "C-<mouse-1>")) ;; Unbind default ctrl-mouse so go-mode can use it for goto definition.

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
    (compile "go test -v -cover -coverprofile=/tmp/coverprofile -covermode=count")
    )

  :bind
  (:map go-mode-map
	  ("TAB"     . company-indent-or-complete-common)
	  ("C-c C-c" . comment-region)
	  ("C-c C-u" . uncomment-region)
	  ("C-c e"   . lsp-rename)
	  ("C-c f"   . go-save-and-compile-program)
	  ("C-c g"   . go-save-and-go-generate)
	  ("C-c w"   . go-save-and-wire)
	  ("C-c v"   . go-save-and-vendor)
	  ("C-c i"   . gofmt)
	  ("C-c t"   . go-save-and-test-program)
	  ("C-c c"   . (lambda() (interactive) (go-coverage "/tmp/coverprofile")))
	  ("C-c d"   . godoc-at-point)
	  ([remap godef-describe]          . lsp-describe-thing-at-point)
	  ([remap godef-jump]              . lsp-ui-peek-find-definitions)
	  ([remap godef-jump-other-window] . go-guru-definition-other-window)
	  ([remap go-rename]               . lsp-rename)
	  )

  :config
  (setq gofmt-command           "goimports"            ;; Use goimprots instead of gofmt.
	  gofmt-show-errors       nil                    ;; Don't show errors. Use LSP instead.
	  godoc-at-point-function (quote godoc-gogetdoc) ;; Use gogetdoc instead of godef for better docs.

    lsp-clients-go-library-directories (quote ("~/go/pkg/mod" ;; Ignore stdlib, go mod cache and go path from LSP.
						                                    "~/goroot"
						                                    "~/go"
						                                    "~/go/src/google.golang.org"
						                                    "~/go/src/golang.org"
						                                    "~/go/src/gopkg.in"
						                                    ))
    )

  :hook
  (go-mode     . lsp-deferred)                 ;; Load LSP.
  (go-mode     . display-line-numbers-mode)    ;; Show line number side pane.
  (go-mode     . yas-minor-mode)               ;; Enable yas.
  (before-save . lsp-format-buffer)            ;; Format the code with LSP before save.
  (before-save . lsp-organize-imports)         ;; Let LSP handle imports.
  )


(use-package yasnippet)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :config
  (setq
    lsp-prefer-flymake nil ;; Disable flymake in favor of flycheck.
    )
  ;; Cleaner mode line.
  :delight " LSP"
  )

;; Overlay UI components for LSP.
(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-position (quote at-point))
  :bind
  ((:map lsp-ui-flycheck-list-mode-map ;; Fix the terminal mode bindings.
	   ("RET"   . lsp-ui-flycheck-list--view)
	   ("TAB"   . lsp-ui-flycheck-list--visit)
	   ("C-c l" . lsp-ui-flycheck-list--quit)
	   )
    (:map lsp-ui-mode-map
	    ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	    ([remap xref-find-references]  . lsp-ui-peek-find-references)
	    )
    )
  )

;; Add LSP backend for company.
(use-package company-lsp
  :commands company-lsp
  )
