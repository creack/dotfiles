;;; .emacs --- Emacs configuration ;;; -*- mode: elisp -*-


;;;; Load & configure themes. ;;;
;;; Functions allow to easily switch between dark/light themes.
;(defun dark-theme()
;  (interactive)                    ;; Allow function call from M-x.
;  (disable-theme 'solarized-light) ;; Disable light theme.
;  (load-theme 'monokai t)          ;; Load Monokai.
;  (enable-theme 'monokai)          ;; Enable Monokai.
;  (powerline-center-theme)         ;; Powerline layout.
;  (custom-set-faces                ;; Tweak faces.
;   '(default ((t (:background "#101010"))))                                                          ;; Slightly increase contrast.
;   '(flycheck-error   ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold)))) ;; Improve flycheck render.
;   '(flycheck-info    ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold)))) ;; Improve flycheck render.
;   '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold)))) ;; Improve flycheck render.
;   )
;  )
;(defun light-theme()
;  (interactive)                    ;; Allow function call from M-x.
;  (disable-theme 'monokai)         ;; Disable dark theme.
;  (load-theme 'solarized-light t)  ;; Load Solarized.
;  (enable-theme 'solarized-light)  ;; Enable Solarized.
;  (powerline-center-theme)         ;; Powerline layout.
;  (custom-set-faces                ;; Reset default faces for solarized.
;   '(default ((t (:background "#FDF6E3"))))
;   '(flycheck-error   ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold))))
;   '(flycheck-info    ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold))))
;   '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold))))
;   )
;  )
;;; Default to dark theme.
;(dark-theme)

;;; Golang configuration. ;;;
(load-file "~/.emacs.files/golang-config.el")

;;; Ediff config. ;;;
;(setq-default ediff-highlight-all-diffs 'nil) ;; Only hilight current diff:
;(setq ediff-diff-options "-w")                ;; Turn off whitespace checking:
;(setq ediff-show-clashes-only t)              ;; Default to conflict diff.

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
