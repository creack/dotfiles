;;; .emacs --- Emacs configuration ;;; -*- mode: lisp -*-

;;; Commentary:

;;; Code:

;;(setq debug-on-error t)

;; Setup "path"
(setq load-path (cons "~/.emacs.files" load-path))

;; Setup emacs-server path
;;(setq server-socket-dir (format "/tmp/emacs%d" (user-uid)))

;;; git-glutter
;;(require 'git-gutter)

;; If you enable global minor mode
;;(global-git-gutter-mode t)

;; If you would like to use git-gutter.el and linum-mode
;;(git-gutter:linum-setup)

;;(global-set-key (kbd "C-x C-g") 'git-gutter:toggle)

;;; enable mouse  ;;;
(require 'mouse)
(xterm-mouse-mode t)
;; Do the xterm mode in a lambda to allow emacsclient to load.
(add-hook 'after-make-frame-functions '
	  (lambda (frame) (unless window-system
			    (global-linum-mode t)
(set-face-attribute 'linum nil :background "#222")
(setq linum-format "%4d\u2502")

			    )))
;; Suspend-frame sucks, disable it
;;(global-set-key (kbd "C-z") (kbd "C-x C-c"))
;;(global-set-key (kbd "C-x C-z") nil)
;;;;

 (defun track-mouse (e))
 (setq mouse-sel-mode t)
 (global-set-key [mouse-4] (lambda ()
 			    (interactive)
 			    (scroll-down 1)))
 (global-set-key [mouse-5] (lambda ()
 			    (interactive)
 			    (scroll-up 1)))


;;; End of mouse setup ;;;


;;; Marmalade (packet manager) ;;;
(require 'package)
(add-to-list 'package-archives
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)
;;; End of Marmalage ;;;


;;; Golang config ;;;
;; Load go-mode
(setq gofmt-command "goimports")
(add-to-list 'load-path "~/goroot/misc/emacs/")
(require 'go-mode-load)
(add-hook 'before-save-hook 'gofmt-before-save)

;; (package-install yasnippet)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs "~/go/src/github.com/dominikh/yasnippet-go")
(yas-global-mode 1)


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

;; go-oracle
(load-file "~/go/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)


;;; End of Golang config ;;


;;; Setup auto-complete ;;;
(setq load-path (cons "~/.emacs.files/auto-complete" load-path))
(require 'go-autocomplete)
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.files/ac-dict")
(setq ac-source-yasnippet nil)
(ac-config-default)
(setq ac-delay 0.1)


;;; Flycheck config ;;;
(setq load-path (cons "~/.emacs.files/flycheck" load-path))
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(global-set-key (kbd "C-c <up>") 'flycheck-next-error)
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error)
(global-set-key (kbd "C-c l") 'flycheck-list-errors)

;;; Emacs general config ;;;

;; No bell
(setq ring-bell-function 'ignore)

;; Display line number on the left
;; (global-linum-mode t)
;; (set-face-attribute 'linum nil :background "#222")
;; (setq linum-format "%4d\u2502")

;; Hightline current line
(global-hl-line-mode t)

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
 '(safe-local-variable-values (quote ((docker-image-name . "creack/apollo"))))
 '(send-mail-function (quote mailclient-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-error ((t (:inherit error :inverse-video t :underline (:color foreground-color :style wave)))))
 '(flycheck-warning ((t (:inherit warning :inverse-video t :underline t)))))


;; Numpad on OSX (24.3.1)
(global-set-key (kbd "M-O p") (kbd "0"))
(global-set-key (kbd "M-O q") (kbd "1"))
(global-set-key (kbd "M-O r") (kbd "2"))
(global-set-key (kbd "M-O s") (kbd "3"))
(global-set-key (kbd "M-O t") (kbd "4"))
(global-set-key (kbd "M-O u") (kbd "5"))
(global-set-key (kbd "M-O v") (kbd "6"))
(global-set-key (kbd "M-O w") (kbd "7"))
(global-set-key (kbd "M-O x") (kbd "8"))
(global-set-key (kbd "M-O y") (kbd "9"))
(global-set-key (kbd "M-O n") (kbd "."))
(global-set-key (kbd "M-O o") (kbd "/"))
(global-set-key (kbd "M-O j") (kbd "*"))
(global-set-key (kbd "M-O k") (kbd "+"))
(global-set-key (kbd "M-O m") (kbd "-"))
(global-set-key (kbd "M-O M") (kbd ""))

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))



;;; White mode
;; (set-face-attribute 'linum nil :background "#DDD")
;; (set-face-background hl-line-face "#EEE")
;; (load-theme 'solarized-light t)

;;; Dark mode
;; (set-face-attribute 'linum nil :background "#222")
;; (set-face-background hl-line-face "#222")
