;;; Package --- Init

;;; Commentary:
;;;

;;; Code:

;; Add melpa sources to the package manager.
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                          ("melpa" . "https://melpa.org/packages/")))

;; Activate package management.
(package-initialize)

(require 'package)

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

;; Always set :ensure.
(setq use-package-always-ensure t)

;; Setyo the use-package extensions.
(use-package use-package-ensure-system-package) ;; Enable external binary dependencies (like godef, gocode, gogetdoc, etc).
(use-package delight)                           ;; Enable delight to manage the mode-line.
;;;

;; Generic settings.
(use-package emacs
  :init
  ;; Helper to toggle trailing whitespaces.
  (defun disable-trailing-whitespace()
    (setq show-trailing-whitespace nil))

  (defun reload-ssh-agent()
    (interactive)
    (setenv "SSH_AUTH_SOCK" (substring (shell-command-to-string "tmux -S /tmp/.tmux-agent show-environment -t agent | \grep SSH_AUTH_SOCK | sed 's/.*=//'") 0 -1))
    (message (format "Reloaded SSH_AUTH_SOCK: %s" (getenv "SSH_AUTH_SOCK")))
    )

  ;; Reload ssh agent now and env every 10 minutes.
  (reload-ssh-agent)
  (run-with-timer 0 (* 60 10) 'reload-ssh-agent)

  ;; Global defaults.
  (setq-default
    indent-tabs-mode         nil ;; Don't use tabs to indent.
    tab-width                8   ;; But maintain correct appearance.
    show-trailing-whitespace t   ;; Show trailing spaces.
    truncate-lines           t   ;; Disable line wrapping.
    )

  ;; Common settings.
  (setq
    ring-bell-function      'ignore ;; Disable the bell.
    initial-scratch-message nil     ;; Remove the default text within the scratch buffer.
    require-final-newline   t       ;; Enforce end of file new line.
    inhibit-startup-message t       ;; Disable the splash screen.
    vc-follow-symlinks      t       ;; Skip yes/no prompt when opening symlinks.

    font-lock-maximum-decoration t ;; Improve syntax highlight.

    ;; Store temp file outside current dir.
    temporary-file-directory       "~/.emacs.d/"
    backup-directory-alist         `((".*" . , temporary-file-directory))
    auto-save-file-name-transforms `((".*" , temporary-file-directory t))
    create-lockfiles nil
    )

  :bind
  ;([mouse-4] . (lambda() (interactive) (scroll-down 5))) ;; Mouse wheel suuport.
  ;([mouse-5] . (lambda() (interactive) (scroll-up 5)))   ;; Mouse wheel suuport.
  ("C-c C-c" . comment-region)
  ("C-c C-u" . uncomment-region)
  ("s-x" . kill-region)
  ("s-c" . kill-ring-save)
  ("s-v" . yank)
  ("s-z" . undo)

  :config
  (global-font-lock-mode 1) ;; Enable syntax colors.
  ;(xterm-mouse-mode      1) ;; Enable mouse support.
  (column-number-mode    1) ;; Display current line/column.
  (line-number-mode      1) ;; Display current line/column.
  (menu-bar-mode         0) ;; Disbale the menu bar.
  (tool-bar-mode         0) ;; Disable the tool bar.
  (scroll-bar-mode       0) ;; Disable the scroll bar.
  (transient-mark-mode   1) ;; Enable highlight region.
  (fset 'yes-or-no-p 'y-or-n-p) ; Yes/No shortcut.

  :hook
  (minibuffer-setup . disable-trailing-whitespace)
  (compilation-mode . disable-trailing-whitespace)
  )

;;

;; Highlight matching ( [ { } ] ).
(use-package paren
  :config
  (show-paren-mode t)
  )

(use-package compile
  :defer
  :bind
  ("C-c r"   . (lambda() (interactive) (save-some-buffers t) (recompile))) ;; Restart the compilation bufer.
  ("C-c k"   . kill-compilation)

  :init
  (setq
    compilation-always-kill   t ;; Don't ask about killing current process before restarting.
    compilation-scroll-output t ;; Autoscroll compilation buffer.
    )
  )

;; Enable winner to manage window layouts.
(use-package winner
  :config
  (winner-mode 1))

;; Enable editorconofig.
(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1)
  )


;; Keep track of recent files.
(use-package recentf
  :init
  (setq recentf-exclude '("~/\\.emacs.d" "~/go/pkg/mod" "\\.log$")
    )
  :config
  (recentf-mode 1)
  )

;; Enable snippets
(use-package yasnippet
  :delight yas-minor-mode
  :config
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/.emacs.files/yasnippet")
  (yas-reload-all)
  )

(use-package yasnippet-snippets)

;; Pull the path from zshrc when in Graphical mode.
(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOROOT"))
  (exec-path-from-shell-initialize))

(use-package hydra)

(use-package ivy-hydra
  :after (ivy hydra))

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
    (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
