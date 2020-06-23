;;; .emacs --- Emacs configuration ;;; -*- mode: elisp -*-


;;; Golang configuration. ;;;

;;; Graphical mode configuration. ;;;

(load-file "~/.emacs.files/flyspell.el")
(load-file "~/.emacs.files/smerge.el")

(load-file "~/.emacs.files/paradox.el")

(load-file "~/.emacs.files/org.el")
;(load-file "~/.emacs.files/perspective.el")


;; On-the-fly linter.
(use-package flycheck
  :after lsp-ui
  :bind
  ("C-c <up>"   . flycheck-next-error)     ;; Ctrl-up   to go to next error.
  ("C-c <down>" . flycheck-previous-error) ;; Ctrl-down to go to previous error.
  ("C-c l"      . lsp-ui-flycheck-list)    ;; Ctrl-l    to display error list.
  )

;; Projectile for workspace management.
(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)

  ;; Cleaner mode line.
  :delight '(:eval (concat " <" (projectile-project-name) ">"))
  )

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
