(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))


; enable mouse
(require 'mouse)
(xterm-mouse-mode t)
;; Do the xterm mode in a lambda to allow emacsclient to load.
;;(add-hook 'after-make-frame-functions '
;;	  (lambda (frame) (unless window-system (xterm-mouse-mode))))
(defun track-mouse (e))
(setq mouse-sel-mode t)
(global-set-key [mouse-4] (lambda ()
			    (interactive)
			    (scroll-down 1)))
(global-set-key [mouse-5] (lambda ()
			    (interactive)
			    (scroll-up 1)))


;; Setup "path"
(setq load-path (cons "~/.emacs.files" load-path))


;;; Golang config ;;;
;; Load go-mode
(setq gofmt-command "goimports")
(add-to-list 'load-path "~/goroot/misc/emacs/")
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

;; Convenient binding for go
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-i") 'go-goto-imports)

;; Go helper for compilation
(setq compilation-always-kill t)
(global-set-key (kbd "C-c C-r") 'recompile)
(global-set-key (kbd "C-c C-k") 'kill-compilation)
(global-set-key (kbd "C-c C-f") 'save-and-compile-program)

;; save all files then run M-x compile
(defun save-and-compile-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "/usr/local/bin/fish -c 'clean; go install; and go build -o /tmp/a.out; and /tmp/a.out'"))

;; go-eldoc
(require 'go-eldoc) ;; Don't need to require, if you install by package.el
(add-hook 'go-mode-hook 'go-eldoc-setup)

;;; End of Golang config ;;


;;; Setup auto-complete ;;;
(setq load-path (cons "~/.emacs.files/auto-complete" load-path))
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.files/ac-dict")
(ac-config-default)


;;; Flycheck config ;;;
(setq load-path (cons "~/.emacs.files/flycheck" load-path))
(setq load-path (cons "~/go/src/github.com/dougm/goflymake" load-path))
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-c <up>") 'flycheck-next-error)
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error)
(global-set-key (kbd "C-c l") 'flycheck-list-errors)

;;; Emacs general config ;;;

;; No bell
(setq ring-bell-function 'ignore)

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

;; GDB helper
(eval-after-load "gud"
  '(progn
     (define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key gud-mode-map (kbd "<down>") 'comint-next-input)))

;;


;; highlight 80+columns
;(require 'whitespace)
;(setq whitespace-style '(face lines-tail trailing))
;(global-whitespace-mode t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:inherit error :inverse-video t :underline (:color foreground-color :style wave)))))
 '(flycheck-warning ((t (:inherit warning :inverse-video t :underline t)))))
