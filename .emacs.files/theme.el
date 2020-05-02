;(use-package zerodark-theme
;  :ensure t
;  :config
;  (load-theme 'zerodark t nil)
;  (zerodark-setup-modeline-format))

(use-package monokai-theme
  :custom
  (monokai-user-variable-pitch t)
  :config
  (load-theme 'monokai t))

; (use-package doom-themes
;   :custom
;   (doom-themes-enable-bold t)
;   (doom-themes-enable-italic t)
;   ;(doom-themes-treemacs-theme "doom-colors") ; Use the colorful treemacs theme.
;
;   :init
;   (add-hook 'after-make-frame-functions
;     (lambda (frame)
;       (select-frame frame)
;       (when (display-graphic-p frame)
;         (set-cursor-color "gray"))))
;   (set-cursor-color "gray")
;   :config
;   (load-theme 'doom-monokai-classic t)
;   ;(doom-themes-treemacs-config)
;   (doom-themes-org-config) ;; Corrects (and improves) org-mode's native fonts.
;   )

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes t)
  :hook
  (after-init . doom-modeline-mode)
  )

(use-package mode-icons
  :config
  (mode-icons-mode 1))

(use-package hide-mode-line
  :hook
  ((treemacs-mode imenu-list-major-mode) . hide-mode-line-mode))

(use-package imenu-list
  :custom
  (imenu-list-size        0.2)
  (imenu-list-position    'right)
  (imenu-list-auto-resize t)
  ;:hook
  ;(after-init . imenu-list-minor-mode)
  ;(prog-mode . imenu-list-show-noselect)
  )
