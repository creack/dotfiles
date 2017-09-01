;;; golang.el --- Golang configuration for emacs. ;;; -*- mode: lisp -*-

(defun go-save-and-compile-program()
        "Save any unsaved buffers and compile."
        (interactive)
        (save-some-buffers t)
        (compile "sh -c 'go install && go build -o /tmp/a.out && /tmp/a.out'")
	)

(defun go-save-and-test-program()
        "Save any unsaved buffers and compile."
        (interactive)
        (save-some-buffers t)
        (compile "go test -v -cover -coverprofile=/tmp/coverprofile -covermode=count")
	)
(setq compilation-scroll-output t)


(add-hook 'go-mode-hook ;; Scope config to go-mode.
          (lambda()
            ;;; Formatting.
            (setq gofmt-command "goimports")                ;; Use goimports instead of gofmt.
            (add-hook 'before-save-hook 'gofmt-before-save) ;; Trigger goimports when saving.

            ;;; Useful key bindings.
            (global-set-key (kbd "M-*")     'pop-tag-mark)     ;; Pop mark after jumping to definition.
            (global-set-key (kbd "C-c C-i") 'go-goto-imports)  ;; Jump to import list.
            (global-set-key (kbd "C-c C-e") 'go-rename)        ;; Call go-rename at point.
            (global-set-key (kbd "C-c d")   'godoc-at-point)   ;; Open godoc from point in new pane.
	    (global-set-key (kbd "C-c C-c") 'comment-region)   ;; Comment region.
	    (global-set-key (kbd "C-c C-u") 'uncomment-region) ;; Uncomment region.

	    ;;; Compilation key bindings.
	    (global-set-key (kbd "C-c f")   'go-save-and-compile-program)
	    (global-set-key (kbd "C-c t") 'go-save-and-test-program)

	    ;; Call coverage binding.
	    (global-set-key (kbd "C-c c") '(lambda() (interactive) (go-coverage "/tmp/coverprofile")))

            ;; Snippets management.
	    (yas-global-mode 1)
	    (add-to-list 'yas-snippet-dirs "~/.emacs.files/yasnippet-go")

	    (go-eldoc-setup)             ;; Init eldoc.
	    (go-guru-hl-identifier-mode) ;; Enable symbol highlight.
            ))

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))
