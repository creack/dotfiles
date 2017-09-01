;;; Package --- Init

;;; Commentary:
;;;

;;; Code:

;; Activate package management.
(package-initialize)

;; Add melpa sources to the package manager.
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(require 'package)

;; List the packages we want.
(defvar package-list)
(setq package-list
      '(

	web-mode
	tern
        tern-auto-complete

	;;; General. ;;;
	flycheck         ;; Linter.
	yasnippet        ;; Snippet management.
	auto-complete    ;; Auto completion.
	neotree          ;; Sidebar dir tree.
	multiple-cursors ;; Multi cursor.

	;; For golang.
	go-mode         ;; Go major mode.
	go-eldoc        ;; Doc at point in minibuffer.
	go-autocomplete ;; Autocomplete mode for golang.
	go-errcheck     ;; Enforce errcheck.
	go-guru         ;; Guru integration.
	go-rename       ;; go-rename integration.
	; flycheck-gometalinter ;; Stricter Go linter module for flycheck.

	;;; Helm. ;;;
	helm

	;;; Themes. ;;;
	monokai-theme
	powerline

	;;; Various modes. ;;;
	dockerfile-mode
	markdown-mode
	yaml-mode
	json-mode
))

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

