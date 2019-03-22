;;; Package --- Init

;;; Commentary:
;;;

;;; Code:


;; Activate package management to have the package-archives variable available.
(package-initialize)

;; Add melpa sources to the package manager.
(add-to-list 'package-archives
             '("melpa-" . "https://melpa.org/packages/") t)

;; Refresh package management.
(package-initialize)

(require 'package)

;; List the packages we want.
(defvar package-list)
(setq package-list
      '(
	;; General.
	editorconfig     ;; .editorconfig support.
	multiple-cursors ;; Multiple cursor support.
	auto-complete    ;; auto-complete support.
	yasnippet        ;; snippet completion support.
	popup            ;; Lib to enable popup in terminal.

	;; Major modes.
	feature-mode    ;; Gherkin (cucumber) test formats.
	plantuml-mode   ;; PlantUML diagrams.
	terraform-mode  ;; Terraform.
	json-mode       ;; Json.
	yaml-mode       ;; Yaml.
	markdown-mode   ;; Markdown.
	dockerfile-mode ;; Dockerfiles.
	go-mode         ;; Golang.

	;; Themes.
	monokai-theme ;; Main theme.
	powerline     ;; Powerline style statusbar.

	;; Linters.
	flycheck-yamllint      ;; Yaml via flycheck
	flycheck-golangci-lint ;; Golang.
	flycheck-popup-tip     ;; Show the lint errors in popup.

	;; Golang.
	go-guru         ;; Static analysis with guru (formerly known as go-oracle).
	go-eldoc        ;; Show godoc while typing function calls.
	go-tag          ;; Autocreate tags for structs.
	go-rename       ;; Refactoring tool with go-rename.
	go-snippets     ;; Go specific snippets completion.
	go-autocomplete ;; Completion support.
))

;; Fetch the list of packages available.
(unless package-archive-contents
  (package-refresh-contents))

;; Install the missing packages.
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))
