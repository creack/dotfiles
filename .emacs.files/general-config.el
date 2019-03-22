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
