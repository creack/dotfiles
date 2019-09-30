;;; .emacs --- Emacs configuration ;;; -*- mode: elisp -*-


;;; Golang configuration. ;;;
(load-file "~/.emacs.files/golang-config.el")

(use-package multiple-cursors
  :bind
  ("M-n"         . mc/mark-next-like-this)         ;; Add new cursor with matching region.
  ("M-p"         . mc/mark-previous-like-this)     ;; Add new cursor with matching region.
  ("M-]"         . mc/mark-all-like-this)          ;; Add new cursor with matching region.
  ("C-c SPC"     . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  ("M-SPC"       . set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
  )

(use-package flycheck
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
