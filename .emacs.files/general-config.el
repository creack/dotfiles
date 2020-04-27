;;; .emacs --- Emacs configuration ;;; -*- mode: elisp -*-

;;; Major/minor mode configuration. ;;;
(load-file "~/.emacs.files/modes-config.el")

;;; Golang configuration. ;;;
(load-file "~/.emacs.files/golang-config.el")

;;; Graphical mode configuration. ;;;
(load-file "~/.emacs.files/graphic.el")

(load-file "~/.emacs.files/multiple-cursors.el")
(load-file "~/.emacs.files/git.el")
(load-file "~/.emacs.files/ivy.el")
(load-file "~/.emacs.files/treemacs.el")
(load-file "~/.emacs.files/flyspell.el")
(load-file "~/.emacs.files/smerge.el")
;(load-file "~/.emacs.files/perspective.el")


;; Show the line number on the side.
(use-package display-line-numbers
  :ensure nil
  :bind ("C-c C-l" . display-line-numbers-mode)
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

;; Smarter ctrl-a/ctrl-e.
(use-package mwim
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;; ;; Setup company mode for completion.
;; (use-package company
;;   :delight ;; Don't show the mode in the mode line.
;;   :defer
;;   :init (global-company-mode)
;;   :config
;;   (setq
;;     company-idle-delay 0
;;     company-minimum-prefix-length 1

;;     company-tooltip-align-annotations t ;; Align the completion popu.
;;     company-show-numbers t              ;; Easy navigation to candidates with M-<n>.
;;     company-dabbrev-downcase nil        ;; Don't worry about case.
;;     )
;;   )

(use-package company
  :delight ;; Don't show the mode in the mode line.
  :defer

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
