;;; golang-config.el --- Golang configuration for emacs. ;;; -*- mode: elisp -*-




(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config
  (flycheck-add-next-checker 'lsp 'golangci-lint)
  )


;(flycheck-add-next-checker 'lsp 'golangci-lint)
