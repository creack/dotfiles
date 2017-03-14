;;; .emacs --- Emacs configuration ;;; -*- mode: lisp -*-

;;; Commentary:

;;; Code:

;;(setq debug-on-error t)

;; Setup "path"
(defun set-exec-path-from-shell-PATH ()
    "Set up Emacs' `exec-path' and PATH environment variable to match that used by the user's shell.
     This is particularly useful under Mac OSX, where GUI apps are not started from a shell."
    (interactive)
    (let ((path-from-shell (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string "$SHELL --login -i"))))
      (setenv "PATH" path-from-shell)
          (setq exec-path (split-string path-from-shell path-separator))))

;;; Package manager ;;;
(package-initialize)
(setq load-path (cons "~/.emacs.files" load-path))
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(load-file "~/.emacs.files/init-packages.el")
;;; End of Package ;;;


;;; enable mouse  ;;;
(require 'mouse)
(xterm-mouse-mode t)
(defun track-mouse (e))
(setq mouse-sel-mode t)
(global-set-key [mouse-4] (lambda ()
 			    (interactive)
 			    (scroll-down 5)))
(global-set-key [mouse-5] (lambda ()
 			    (interactive)
 			    (scroll-up 5)))
;;; End of mouse setup ;;;


;;; Golang config ;;;
;; Load go-mode
(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)
(global-set-key (kbd "M-*") 'pop-tag-mark)

;;(add-to-list 'yas-snippet-dirs "~/.emacs.files/yasnippet-go")
(yas-global-mode 1)


(add-hook 'go-mode-hook
	  (lambda()
	    ;; Convenient binding for go
	    (global-set-key (kbd "C-c C-i") 'go-goto-imports)
	    (global-set-key (kbd "C-c C-e") 'go-rename)
	    (global-set-key (kbd "C-c d") 'godoc-at-point)
	    (global-set-key (kbd "C-c c") '(lambda() (interactive) (go-coverage "coverprofile")))

	    ;; Go helper for compilation
	    (global-set-key (kbd "C-c f") 'save-and-compile-program)
	    (global-set-key (kbd "C-c C-t") 'save-and-test-program)
	    (global-set-key (kbd "C-c t")   'save-and-make-test-program)
	    (global-set-key (kbd "C-c b")   'save-and-make-clean-program)
	    (global-set-key (kbd "C-c C-m") 'save-and-make-program)

	    )
	  )

;; Kill compilation upon recompile.
(setq compilation-always-kill t)

(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)
(global-set-key (kbd "C-c C-r") 'my-recompile)
(global-set-key (kbd "C-c C-k") 'kill-compilation)
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
	(end-of-line-compile)
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
(add-hook 'go-mode-hook 'go-guru-hl-identifier-mode)

;;; End of Golang config ;;

;;; Setup auto-complete ;;;
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
;;(setq ac-source-yasnippet nil)
(ac-config-default)
(setq ac-delay 0.1)

;;; Flycheck config ;;;
(add-hook 'after-init-hook 'global-flycheck-mode)

(global-set-key (kbd "C-c <up>") 'flycheck-next-error)
(global-set-key (kbd "C-c <down>") 'flycheck-previous-error)
;(global-set-key (kbd "C-c l") 'flycheck-list-errors)
(eval-after-load 'flycheck
  '(define-key flycheck-mode-map (kbd "C-c l") 'helm-flycheck))

;;; Emacs general config ;;;

;; No bell
(setq ring-bell-function 'ignore)
(setq bell-volume 0)

;; Hightline current line
(global-hl-line-mode t)

;; Show trailing whitespaces
(setq-default show-trailing-whitespace t)

;; windows management
(winner-mode 1)

(menu-bar-mode 0)                        ; no file/edit/blabla top menu.
(setq inhibit-startup-message t)         ; no splash display.
(setq-default truncate-lines t)          ; no wrapping.
(fset 'yes-or-no-p 'y-or-n-p)            ; yes/no shortcut.
(when (require 'ido nil t) (ido-mode t)) ; (much) better file/buffer browsing.
(global-font-lock-mode t)		 ; default enable syntax coloration.
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

;;; Auto Save/Backup config ;;;
(setq temporary-file-directory "~/.emacs.files/tmp/")
(setq backup-directory-alist
          `((".*" . , temporary-file-directory)))
    (setq auto-save-file-name-transforms
          `((".*" , temporary-file-directory t)))

;; GDB helper
(eval-after-load "gud"
  '(progn
     (define-key gud-mode-map (kbd "<up>") 'comint-previous-input)
     (define-key gud-mode-map (kbd "<down>") 'comint-next-input)))

;;


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


;; Makefile tab size.
(add-hook 'makefile-mode-hook
  (function
    (lambda ()
      (setq tab-width 8
))))


;;(custom-set-faces
;; ;; custom-set-faces was added by Custom.
;; ;; If you edit it by hand, you could mess it up, so be careful.
;; ;; Your init file should contain only one such instance.
;; ;; If there is more than one, they won't work right.
;; '(go-guru-hl-identifier-face ((t (:inherit highlight :inverse-video t :underline t))))
;; '(hl-line ((t (:background "color-236"))))
;; '(web-mode-block-face ((t (:background "brightblue")))))

(defun start-fe-local()
        "Save any unsaved buffers and start the frontend"
        (interactive)
        (save-some-buffers t)
	(end-of-line-compile)
        (compile "bash -c 'cd $GOPATH/src/github.com/agrarianlabs/farm-dashboard && make local_dev NOPULL=1'")
	(end-of-line-compile))

(defun start-viz-local()
        "Save any unsaved buffers and start the frontend"
        (interactive)
        (save-some-buffers t)
	(end-of-line-compile)
        (compile "bash -c 'cd $GOPATH/src/github.com/agrarianlabs/viztool && make local_dev NOPULL=1 ENV_MODE=production'")
	(end-of-line-compile))


(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-set-content-type "jsx")

	    (setq web-mode-attr-indent-offset 2)
	    (setq web-mode-attr-value-indent-offset 2)
	    (setq web-mode-enable-control-block-indentation 2)
	    (setq web-mode-code-indent-offset 2)
	    (setq web-mode-css-indent-offset 2)
	    (setq web-mode-markup-indent-offset 2)
	    (setq web-mode-sql-indent-offset 2)

            (setq js-indent-level 2)
            (setq jsx-indent-level 2)
            (setq indent-tabs-mode nil)

	    (setq web-mode-enable-auto-indentation t)
	    (setq web-mode-enable-auto-closing t)
	    (setq web-mode-enable-auto-expanding t)
	    (setq web-mode-enable-auto-opening t)
	    (setq web-mode-enable-auto-pairing t)
;;	    (setq web-mode-enable-auto-quoting t)
	    (setq web-mode-enable-block-face t)
	    (setq web-mode-enable-comment-interpolation t)
	    (setq web-mode-enable-css-colorization t)
	    (setq web-mode-enable-current-column-highlight t)
	    (setq web-mode-enable-current-element-highlight t)
	    (setq web-mode-enable-element-content-fontification t)
	    (setq web-mode-enable-element-tag-fontification t)
	    (setq web-mode-enable-engine-detection t)
	    (setq web-mode-enable-heredoc-fontification t)
	    (setq web-mode-enable-html-entities-fontification t)
	    (setq web-mode-enable-inlays t)
	    (setq web-mode-enable-part-face t)
	    (setq web-mode-enable-sexp-functions t)
	    (setq web-mode-enable-sql-detection t)
	    (setq web-mode-enable-string-interpolation t)
	    ;(setq web-mode-enable-whitespace-fontification t)

	    (global-set-key (kbd "C-c .") 'tern-ac-complete)
	    (global-set-key (kbd "C-c r") 'my-recompile)
	    (global-set-key (kbd "C-c m") 'start-fe-local)
	    (global-set-key (kbd "C-c n") 'start-viz-local)
	    (global-set-key (kbd "C-c k") 'kill-compilation)
	    (flycheck-add-mode 'javascript-eslint 'web-mode)
	    ;; Disable jshint in favor of eslint.
	    (setq-default flycheck-disabled-checkers
			  (append flycheck-disabled-checkers
				  '(javascript-jshint)))
            )
	  )

(add-hook 'web-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))


(defun stop-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; SQL
(load-file "~/.emacs.files/sql-mode.el")

;; Experimental.

(helm-mode 1)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)

;; Force flycheck to push marker before jump.
(defadvice flycheck-next-error (around wh/flycheck-next-error-push-mark activate)
  (push-mark)
  ad-do-it)


(load-theme 'monokai t)
(setq compilation-scroll-output t)

;; Org-mode config.

;; Display date when closing a TODO.
(setq org-log-done 'time)

;; Setup modes.
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"      . web-mode))
(add-to-list 'auto-mode-alist '("\\.mk\\'"      . makefile-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'"       . octave-mode))
(add-to-list 'auto-mode-alist '("\\.geplo\\'"   . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'"     . web-mode))

;; Ediff config.
(setq-default ediff-highlight-all-diffs 'nil) ;; Only hilight current diff:
(setq ediff-diff-options "-w")                ;; Turn off whitespace checking:
(setq ediff-show-clashes-only t)              ;; Default to conflict diff.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (json-mode yaml-mode markdown-mode dockerfile-mode monokai-theme tern-auto-complete tern web-mode helm-flycheck helm-git helm-swoop helm flycheck-gometalinter go-rename go-guru go-snippets go-errcheck go-eldoc go-direx go-autocomplete flycheck auto-complete sql-indent highlight-indentation find-file-in-project exec-path-from-shell ctable))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
