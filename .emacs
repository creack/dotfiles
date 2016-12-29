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

; Autogenerated variables.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(background-color nil)
 '(background-mode dark)
 '(compilation-message-face (quote default))
 '(compilation-scroll-output t)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color nil)
 '(custom-safe-themes
   (quote
    ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "38ba6a938d67a452aeb1dada9d7cdeca4d9f18114e9fc8ed2b972573138d4664" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" default)))
 '(fci-rule-color "#20240E")
 '(foreground-color nil)
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#20240E" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#20240E" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (go-rename jenkins groovy-mode yaml-mode guru-mode go-guru format-sql helm-swoop helm-package helm-git helm-flycheck helm tern-auto-complete js2-mode jsfmt jsx-mode gnuplot json-mode go-snippets go-errcheck go-eldoc go-direx go-autocomplete flycheck dockerfile-mode direx sql-indent jedi iedit exec-path-from-shell epc elpy cyberpunk-theme ctable concurrent company)))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(solarized-broken-srgb t)
 '(solarized-diff-mode (quote normal))
 '(solarized-termcolors 256)
 '(solarized-underline t)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#F92672")
     (40 . "#CF4F1F")
     (60 . "#C26C0F")
     (80 . "#E6DB74")
     (100 . "#AB8C00")
     (120 . "#A18F00")
     (140 . "#989200")
     (160 . "#8E9500")
     (180 . "#A6E22E")
     (200 . "#729A1E")
     (220 . "#609C3C")
     (240 . "#4E9D5B")
     (260 . "#3C9F79")
     (280 . "#A1EFE4")
     (300 . "#299BA6")
     (320 . "#2896B5")
     (340 . "#2790C3")
     (360 . "#66D9EF"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#20240E" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(go-guru-hl-identifier-face ((t (:inherit highlight :inverse-video t :underline t))))
 '(hl-line ((t (:background "color-236"))))
 '(web-mode-block-face ((t (:background "brightblue")))))

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


;; Experimental.

(helm-mode 1)
(global-set-key (kbd "M-x")     'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b")   'helm-mini)
(global-set-key (kbd "M-y")     'helm-show-kill-ring)
(global-set-key (kbd "C-s")     'helm-swoop)

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
