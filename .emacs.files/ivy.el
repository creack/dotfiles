(use-package ivy
  :delight
  :config
  (ivy-mode)
  )

(use-package counsel
  :delight
  :ensure-system-package ag
  :after ivy
  :bind
  (("C-s" . swiper)
    ("M-s r" . ivy-resume)
    ("C-c v p" . ivy-push-view)
    ("C-c v o" . ivy-pop-view)
    ("C-c v ." . ivy-switch-view)
    ("M-s c" . counsel-ag)
    ("M-o f" . counsel-fzf)
    ("M-o r" . counsel-recentf)
    ("M-y" . counsel-yank-pop)
    ("C-x C-f" . counsel-find-file)
    ("M-x" . counsel-M-x)
    :map ivy-minibuffer-map
    ("C-w" . ivy-backward-kill-word)
    ("C-k" . ivy-kill-line)
    ("C-j" . ivy-immediate-done)
    ("RET" . ivy-alt-done)
    ("C-h" . ivy-backward-delete-char))
  :custom
  ;(ivy-use-selectable-prompt t)
  (ivy-use-virtual-buffers t)     ; Add recent files and window layouts to the switch buffer list.
  (ivy-on-del-error-function nil) ; Don't yield and error when deleting readonly part of the prompt.
  (ivy-extra-directories   nil)   ; Hide . and .. in file list.
  (swiper-action-recenter t)      ; Keep swiper centered.
  (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
  :config
  (use-package flx)         ;; Enhance fuzzy matching
  (use-package amx)         ;; Enhance M-x
  (counsel-mode)            ;; Enable counsel.
  (all-the-icons-ivy-setup) ;; Setup the icons.
  )


;; Extends ivy with more details.
(use-package ivy-rich
  :after (ivy counsel)
  :custom
  (ivy-rich-path-style    'abbrev)
  (ivy-virtual-abbreviate 'full)
  :config
  (ivy-rich-mode)
  (all-the-icons-ivy-setup)
  )

;; NOTE: ivy-rich icons break things like package-install details.
(use-package all-the-icons-ivy
  :defer
  :config (all-the-icons-ivy-setup))
