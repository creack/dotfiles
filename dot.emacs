;; Setup "path"
(setq load-path (cons "~/.emacs.files" load-path))
(setq load-path (cons "~/.emacs.files/autocomplete" load-path))

;;; Golang config ;;;

;; Load extern files (from ~/.emacs.files/
(load "go-mode-load")   ; golang-mode
(load "flymake-cursor") ; flymake on minibuffer
(load "goflymake")      ; flymake mode for golang

;; Load externals modes
(require 'go-autocomplete) ; autocomplete (with nfs daemon)
(require 'auto-complete-config)
(require 'go-flymake)
(require 'sr-speedbar)

;; Convenient binding for go
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
(global-set-key (kbd "C-c C-i") 'go-goto-imports)

;; Auto apply gofmt when saving a file
(add-hook 'before-save-hook 'gofmt-before-save)

;;; Flymake config ;;;
;; Better highlight colors
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;; '(flymake-errline ((((class color) (min-colors 89)) (:foreground "#990A1B" :background "#FF6E64" :weight bold :underline t))))
;; '(flymake-warnline ((((class color) (min-colors 89)) (:foreground "#7B6000" :background "#DEB542" :weight bold :underline t))))
;; '(font-lock-function-name-face ((t (:foreground "lightblue3")))))
 ;; better functions name color

;; Conveniant bindings
(global-set-key (kbd "C-c <up>") 'flymake-goto-next-error)
(global-set-key (kbd "C-c <down>") 'flymake-goto-prev-error)


;;; Speedbar config ;;;
(global-set-key (kbd "C-c C-s") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c s") 'sr-speedbar-select-window)

(setq speedbar-use-images nil)
(add-hook 'speedbar-mode-hook '(lambda () (linum-mode -1)))
(set-face-foreground 'speedbar-directory-face "lightblue3")
(setq sr-speedbar-right-side nil)				; Force speedbar at the right
(setq sr-speedbar-skip-other-window-p t)			; Skip speedbar when switching buffer
;(sr-speedbar-open)						; This works but also opens scratch buffer..
(speedbar-add-supported-extension ".go")
(setq sr-speedbar-auto-refresh nil)				; Disable the auto-refresh

;;; Emacs general config ;;;

;; Display line number on the left
(global-linum-mode 1)
(set-face-attribute 'linum nil :background "#222")
(setq linum-format "%4d\u2502")

;; Hightline current line
(global-hl-line-mode t)
;;(set-face-background hl-line-face "gray13")

;; Change the region background color
;;(set-face-background 'region "#770")


;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; windows management
(winner-mode 1)

;; Remove useless default emacs crap
(menu-bar-mode 0)                        ; no file/edit/blabla top menu
(setq inhibit-startup-message t)
(setq bell-volume 0)
(setq-default truncate-lines t)          ; no wrapping
(fset 'yes-or-no-p 'y-or-n-p)            ; yes/no shortcut
(when (require 'ido nil t) (ido-mode t)) ; (much) better file/buffer browsing
(global-font-lock-mode t)		 ; default enable syntax coloration
(xterm-mouse-mode t)			 ; default enable mouse
(setq initial-scratch-message "")	 ; remove the default text within the scratch buffer

;; Display current line/column
(column-number-mode 1)
(line-number-mode 1)

;; Enforce end of file new line
(setq require-final-newline 1 mode-require-final-newline 1)

;; Highlight matching ( [ { } ] )
(require 'paren)
(show-paren-mode t)


;;; VC config ;;;
(autoload 'magit-status "magit" nil t)

;;; Auto Save/Backup config ;;;

;(setq make-backup-files nil) ; stop ~ files
(setq temporary-file-directory "~/.emacs.files/tmp/")
(setq backup-directory-alist
          `((".*" . , temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" , temporary-file-directory t)))

;;; Marmalade (packet manager) ;;;
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;; Random conf to be checked


;(put 'narrow-to-region 'disabled nil)    ; enable...
;(put 'erase-buffer 'disabled nil)        ; ... useful things
;(when (fboundp file-name-shadow-mode)    ; emacs22+
;  (file-name-shadow-mode t))             ; be smart about filenames in mbuf
(setq custom-theme-load-path (cons "~/.emacs.files/themes/emacs-color-theme-solarized" custom-theme-load-path))
(load-theme 'solarized-dark t)
