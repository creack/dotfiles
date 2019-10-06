;;; .emacs --- Emacs configuration ;;; -*- mode: elisp -*-


;;; Golang configuration. ;;;
(load-file "~/.emacs.files/golang-config.el")

;; Briefly highlight cursor position when moving around.
(use-package beacon
  :init
  (setq
    beacon-blink-when-point-moves-vertically 1
    beacon-blink-when-focused 1
    )
  :hook (after-init . beacon-mode)
  )

;; Show the line number on the side.
(use-package display-line-numbers
  :ensure nil
  :bind ("C-c C-l" . display-line-numbers-mode)
)

;; Highlight the indentation whitespaces.
(use-package highlight-indent-guides
  :delight
  :init
  (setq
    highlight-indent-guides-auto-enabled t
    highlight-indent-guides-responsive   t
    highlight-indent-guides-method       'character
    )
  )

;; Enable multiple cursors.
(use-package multiple-cursors
  :bind
  ("M-n"         . mc/mark-next-like-this)         ;; Add new cursor with matching region.
  ("M-p"         . mc/mark-previous-like-this)     ;; Add new cursor with matching region.
  ("M-]"         . mc/mark-all-like-this)          ;; Add new cursor with matching region.
  ("C-c SPC"     . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  ("M-SPC"       . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  )

;; Support ansi colors in compile-mode.
(use-package ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  :hook
  (compilation-filter . colorize-compilation-buffer)
  )

;; On-the-fly linter.
(use-package flycheck
  :after (lsp-ui)
  :bind
  ("C-c <up>"   . flycheck-next-error)     ;; Ctrl-up   to go to next error.
  ("C-c <down>" . flycheck-previous-error) ;; Ctrl-down to go to previous error.
  ("C-c l"      . lsp-ui-flycheck-list)    ;; Ctrl-l    to display error list.
  :config
  (setq flycheck-highlighting-mode (quote lines))
  )

;; Projectile for workspace management.
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)

  ;; Cleaner mode line.
  :delight '(:eval (concat " <" (projectile-project-name) ">"))
  )

;; Show current prefix / possible next action in minibuffer.
(use-package which-key
  :config
  (which-key-mode 1)
  :delight ;; Don't show the mode in the mode line.
  )

;; Auto insert/replace parens.
(use-package smartparens
  :defer
  )

;; Setup company mode for completion.
(use-package company
  :delight ;; Don't show the mode in the mode line.
  :defer
  :init (global-company-mode)
  :config
  (setq
   company-tooltip-align-annotations t ;; Align the completion popu.
   company-show-numbers t              ;; Easy navigation to candidates with M-<n>.
   company-dabbrev-downcase nil        ;; Don't worry about case.
   )
  )
