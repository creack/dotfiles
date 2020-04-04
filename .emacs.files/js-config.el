;;; js-config.el --- Javascript configuration for emacs. ;;; -*- mode: elisp -*-

(use-package rjsx-mode
  :mode "\\.js\\'" "\\.jsx\\'"
  )

(use-package typescript-mode)

(use-package tide
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
          (typescript-mode . tide-hl-identifier-mode)
          (before-save . tide-format-before-save))
  :bind
  ("C-c <up>"   . flycheck-next-error)     ;; Ctrl-up   to go to next error.
  ("C-c <down>" . flycheck-previous-error) ;; Ctrl-down to go to previous error.
  ("C-c l"      . flycheck-list-errors)    ;; Ctrl-l    to display error list.
  ("C-c e"      . tide-rename-symbol)    ;; Ctrl-l    to display error list.
  ("M-?"        . tide-references)         ;; M-?       to display the references.
  ("<backtab>"  . company-complete)
  :init
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-mode)
  ;(flycheck-select-checker 'javascript-eslint)
  )

(use-package web-mode
  :mode "\\.tsx\\'"
  :hook (web-mode . tide-setup)
  )
