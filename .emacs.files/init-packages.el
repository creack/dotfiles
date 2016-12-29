;;; package --- init

;;; Commentary:
;;;

;;; Code:

(require 'package)

;;(add-to-list 'package-archives
;;             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;;(add-to-list 'package-archives
;;             '("marmalade" . "http://marmalade-repo.org/packages/"))

;;(add-to-list 'package-archives
;;             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp/")

(defvar package-list)

; list the packages you want
(setq package-list
      '(
	ctable
	exec-path-from-shell
	find-file-in-project
	highlight-indentation
	let-alist
	sql
        sql-indent

	auto-complete
	dockerfile-mode
	flycheck
	go-autocomplete
	go-direx
	go-eldoc
	go-errcheck
	go-mode
	go-snippets
	go-guru
	yasnippet
	flycheck-gometalinter

	helm
	helm-swoop
	helm-git
	helm-flycheck

        web-mode
        tern
        tern-auto-complete

	monokai-theme
))


; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Add custom snippets
