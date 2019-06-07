;;; .emacs --- Emacs configuration ;;; -*- mode: lisp -*-

;;; Debug mode. ;;;
;(setq debug-on-error t)

;;; Init package manager. ;;;
(load-file "~/.emacs.files/init-packages.el")

;;; Enable mouse support. ;;;
(xterm-mouse-mode t)

;; Wheel support.
(global-set-key [mouse-4] (lambda ()
                            (interactive)
                            (scroll-down 5)))
(global-set-key [mouse-5] (lambda ()
                            (interactive)
                            (scroll-up 5)))

;;; Emacs general config. ;;;

;; No bell.
(setq ring-bell-function 'ignore)
(setq bell-volume 0)

;; Hightline current line.
(global-hl-line-mode t)

;; Show trailing whitespaces.
(setq-default show-trailing-whitespace t)

;; Windows management.
(winner-mode 1)

(menu-bar-mode 0)                        ; No file/edit/blabla top menu.
(setq inhibit-startup-message t)         ; No splash display.
(setq-default truncate-lines t)          ; No wrapping.
(fset 'yes-or-no-p 'y-or-n-p)            ; Yes/No shortcut.
(when (require 'ido nil t) (ido-mode t)) ; (Much) Better file/buffer browsing.
(global-font-lock-mode t)                ; Default enable syntax coloration.
(setq initial-scratch-message "")        ; Remove the default text within the scratch buffer.

;; Display current line/column.
(column-number-mode 1)
(line-number-mode 1)

;; Enforce end of file new line.
(setq require-final-newline 1 mode-require-final-newline 1)

;; Highlight matching ( [ { } ] ).
(require 'paren)
(show-paren-mode t)

;; Store temp file outside current dir.
(setq temporary-file-directory "~/.emacs.files/tmp/")
(setq backup-directory-alist         `((".*" . , temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" , temporary-file-directory t)))
(setq create-lockfiles nil)

;;; Helm config. ;;;
;; (helm-mode 1)                                       ;; Enable helm major mode.
;; (global-set-key (kbd "M-x")   'helm-M-x)            ;; Use helm for M-x.
;; (global-set-key (kbd "C-x b") 'helm-mini)           ;; Use helm for buffer switch.
;; (global-set-key (kbd "M-y")   'helm-show-kill-ring) ;; Use heml for kill ring.

;;; Load & configure themes. ;;;
;; Functions allow to easily switch between dark/light themes.
(defun dark-theme()
  (interactive)                    ;; Allow function call from M-x.
  (load-theme 'monokai t)          ;; Load Monokai.
  (enable-theme 'monokai)          ;; Enable Monokai.
  (powerline-default-theme)        ;; Powerline layout.
  (custom-set-faces                ;; Tweak faces.
   '(default ((t (:background "#101010"))))                                                          ;; Slightly increase contrast.
   '(flycheck-error   ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold)))) ;; Improve flycheck render.
   '(flycheck-info    ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold)))) ;; Improve flycheck render.
   '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold)))) ;; Improve flycheck render.
   )
  )
;; Default to dark theme.
(dark-theme)

;;; Golang configuration. ;;;
(load-file "~/.emacs.files/golang-config.el")

;;; JS configuration. ;;;
;; (load-file "~/.emacs.files/js-config.el")

;;; Ediff config. ;;;
(setq-default ediff-highlight-all-diffs 'nil) ;; Only hilight current diff:
(setq ediff-diff-options "-w")                ;; Turn off whitespace checking:
(setq ediff-show-clashes-only t)              ;; Default to conflict diff.

;;; Flycheck config. ;;;
(add-hook 'after-init-hook 'global-flycheck-mode)            ;; Enable flycheck everywhere.
(global-set-key (kbd "C-c <up>")   'flycheck-next-error)     ;; Ctrl-up   to go to next error.
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error) ;; Ctrl-down to go to previous error.
(global-set-key (kbd "C-c l")      'flycheck-list-errors)    ;; Ctrl-l    to display error list.

;;; Auto-complete config. ;;;
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
;;(setq ac-delay 0.1)

;;; Key bindings. ;;;
(global-set-key (kbd "C-c C-k") 'kill-compilation) ;; Kill compilation buffer.
(global-set-key (kbd "C-c C-l") 'linum-mode)       ;; Toggle linum-mode.

;;; Compilation buffer config. ;;;
(setq compilation-always-kill t) ;; Don't ask about killing current process before restarting.

;; Small helper to tail compilation buffer whithout leaving current one.
(defun end-of-line-compile()
  (interactive)
  (setq curbuf (current-buffer))
  (pop-to-buffer "*compilation*")
  (end-of-buffer)
  (pop-to-buffer curbuf)
  )

;; Recompile helper to tail output.
(defun recompile-scroll()
        "Recompile and tail output."
        (interactive)
        (save-some-buffers t)
        (recompile)
	(end-of-line-compile))

;; Key binding for recompile.
(global-set-key (kbd "C-c r") 'recompile-scroll)

;;; Multiple cursor config. ;;;
(global-set-key (kbd "S-<mouse-1>") 'mc/add-cursor-on-click)         ;; Add new cursor with shift-click.
(global-set-key (kbd "M-n")         'mc/mark-next-like-this)         ;; Add new cursor with matching region.
(global-set-key (kbd "M-p")         'mc/mark-previous-like-this)     ;; Add new cursor with matching region.
(global-set-key (kbd "M-]")         'mc/mark-all-like-this)          ;; Add new cursor with matching region.
(global-set-key (kbd "C-c SPC")     'set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
(global-set-key (kbd "M-SPC")       'set-rectangular-region-anchor)  ;; Rectangular region with many cursors.
(setq mc/cmds-to-run-for-all
      '(
	backward-delete-char
	backward-delete-char-untabify
	yaml-backspace-function
	yaml-electric-backspace
        ))

;;; Tmux integration. ;;;
(global-set-key (kbd "C-x x") (lambda ()
                            (interactive)
                            (shell-command (concat "tmux split-window -v -p 20 -c " default-directory))
			    ))

;;; Hightlight long lines ;;;
(require 'whitespace)
(setq whitespace-line-column 120)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(defun cpp-format ()
  (setq-default c-basic-offset 2)
  (c-set-offset 'access-label -2)
  (c-set-offset 'inclass 4)
  (c-set-offset 'innamespace 0)
  )
(add-hook 'c++-mode-hook 'cpp-format)

;; Enable editormode.
(editorconfig-mode 1)

;; Load .puml files in plantuml-mode.
(add-to-list 'auto-mode-alist '("\\.puml\\'" . plantuml-mode))

;; Skip prompt when opening links.
(setq vc-follow-symlinks t)

;; Backup face customization. If ever needed.
;; Remove if not used before 06/01/2019.
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background "#101010"))))
;;  '(flycheck-error ((t (:background "#FF6E64" :foreground "#990A1B" :underline t :weight bold))))
;;  '(flycheck-info ((t (:background "#69B7F0" :foreground "#00629D" :underline t :weight bold))))
;;  '(flycheck-warning ((t (:background "#DEB542" :foreground "#7B6000" :underline t :weight bold))))
;;  '(popup-tip-face ((t (:background "color-100" :foreground "color-233" :weight bold)))))
(projectile-mode +1)

;; Test treemacs.
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python3") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         t
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 t
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   1
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)
