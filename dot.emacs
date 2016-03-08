;;; .emacs --- Emacs configuration ;;; -*- mode: lisp -*-

;;; Commentary:

;;; Code:

;;(setq debug-on-error t)

;; Setup "path"
(defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
      (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq load-path (cons "~/.emacs.files" load-path))


;;(set-face-background 'highlight-indentation-face "#222")
;;(set-face-background 'highlight-indentation-current-column-face "#222")

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


;;; Package manager ;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(load-file "~/.emacs.files/init-packages.el")

;;; End of Package ;;;


;;; Golang config ;;;
;; Load go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;;(add-to-list 'yas-snippet-dirs "~/.emacs.files/yasnippet-go")
(yas-global-mode 1)


;; Convenient binding for go
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-i") 'go-goto-imports)
(global-set-key (kbd "C-c C-e") 'go-rename)
(global-set-key (kbd "C-c d") 'godoc-at-point)
(global-set-key (kbd "C-c c") '(lambda() (interactive) (go-coverage "coverprofile")))

;; Go helper for compilation
(setq compilation-always-kill t)
(global-set-key (kbd "C-c C-r") 'my-recompile)
(global-set-key (kbd "C-c C-k") 'kill-compilation)
(global-set-key (kbd "C-c C-f") 'save-and-compile-program)
(global-set-key (kbd "C-c C-t") 'save-and-test-program)
(global-set-key (kbd "C-c t")   'save-and-make-test-program)
(global-set-key (kbd "C-c b")   'save-and-make-clean-program)
(global-set-key (kbd "C-c C-m") 'save-and-make-program)

(global-set-key (kbd "C-c C-l") 'linum-mode)

(defun end-of-line-compile()
  (setq curbuf (current-buffer))
  (pop-to-buffer "*compilation*")
  (end-of-buffer)
  (pop-to-buffer curbuf)
  )

;; save all files then run M-x compile
(defun my-recompile()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (recompile)
	(end-of-line-compile))

(defun save-and-compile-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "bash -c 'go install && go build -o /tmp/a.out && /tmp/a.out'")
	(end-of-line-compile))

(defun save-and-test-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "go test -v -cover -coverprofile=coverprofile -covermode=count")
	(end-of-line-compile))

(defun save-and-make-test-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make test SKIP_FMT=1 NOPULL=1 TEST_OPTS='-v .'")
	(end-of-line-compile))


(defun save-and-make-clean-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make clean")
	(end-of-line-compile))

(defun save-and-make-program()
        "Save any unsaved buffers and compile"
        (interactive)
        (save-some-buffers t)
        (compile "make start NOPULL=1")
	(end-of-line-compile))

(add-hook 'go-mode-hook 'go-eldoc-setup)

;; go-oracle
(load-file "~/go/src/golang.org/x/tools/cmd/oracle/oracle.el")
;(add-hook 'go-mode-hook 'go-oracle)
(load-file "~/go/src/golang.org/x/tools/refactor/rename/go-rename.el")


;;; End of Golang config ;;

;;; Setup auto-complete ;;;
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
;;(setq ac-source-yasnippet nil)
(ac-config-default)
(setq ac-delay 0.1)


;;; Flycheck config ;;;
;;(setq load-path (cons "~/.emacs.files/flycheck" load-path))
;;(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

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
(setq col-highlight-vline-face-flag t)
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
;(autoload 'magit-status "magit" nil t)

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
;;(load-theme 'solarized-light t)
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


;; Numpad on OSX (emacs 24.3.1)
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

(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))



;;; White mode
;; (set-face-attribute 'linum nil :background "#DDD")
;; (set-face-background hl-line-face "#EEE")
;; (load-theme 'solarized-light t)

;;; Dark mode
;; (set-face-attribute 'linum nil :background "#222")
;; (set-face-background hl-line-face "#222")

(add-hook 'makefile-mode-hook
	  (function
	   (lambda ()
	     (setq tab-width 8)
	     )))


;; Configure to wait a bit longer after edits before starting
(setq-default flymake-no-changes-timeout '3)

;; To avoid having to mouse hover for the error message, these functions make flymake error messages
;; appear in the minibuffer
(defun show-fly-err-at-point ()
  "If the cursor is sitting on a flymake error, display the message in the minibuffer"
  (require 'cl)
  (interactive)
  (let ((line-no (line-number-at-pos)))
    (dolist (elem flymake-err-info)
      (if (eq (car elem) line-no)
      (let ((err (car (second elem))))
        (message "%s" (flymake-ler-text err)))))))

(add-hook 'post-command-hook 'show-fly-err-at-point)

;; platform vertica CFLAGS - OSX
(setenv "CGO_CFLAGS" (concat (getenv "CGO_CFLAGS") " " "-I /usr/local/Cellar/unixodbc/2.3.2_1/include"))
(setenv "CGO_LDFLAGS" (concat (getenv "CGO_LDFLAGS") " " "-L /usr/local/Cellar/unixodbc/2.3.2_1/lib"))

;; platform file regexp
(add-to-list 'compilation-error-regexp-alist '(".*? \\[.*?\\] [0-9]+/[0-9]+/[0-9]+ [0-9]+:[0-9]+:[0-9]+\.[0-9]+ \\(.*?\\):\\([0-9]+\\)" 1 2))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (tern-auto-complete tern js2-mode web-mode jsfmt jsx-mode yaml-mode gnuplot json-mode markdown-mode sr-speedbar elixir-mix elixir-mode go-snippets go-errcheck go-eldoc go-direx go-autocomplete flycheck dockerfile-mode direx sql-indent minimap magit jedi iedit exec-path-from-shell epc elpy cyberpunk-theme ctable concurrent company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))


(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")
            (message "now set to: %s" web-mode-content-type)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq js-indent-level 2)
            (setq jsx-indent-level 2)
            (setq indent-tabs-mode nil)
	    (global-set-key (kbd "C-c .") 'tern-ac-complete)
            ))

(add-hook 'web-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
