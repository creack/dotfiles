;;; golang.el --- Golang configuration for emacs. ;;; -*- mode: lisp -*-

(defun go-save-and-compile-program()
  "Save any unsaved buffers and compile."
  (interactive)
  (save-some-buffers t)
  (compile "sh -c 'go build -o /tmp/a.out && /tmp/a.out'")
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
	    (setq godoc-and-godef-command "go doc")         ;; Fix godoc.
            (add-hook 'before-save-hook 'gofmt-before-save) ;; Trigger goimports when saving.

            ;;; Useful key bindings.
            (global-set-key (kbd "C-c C-e") 'go-rename)        ;; Call go-rename at point.
            (global-set-key (kbd "C-c d")   'godoc-at-point)   ;; Open godoc from point in new pane.
	    (global-set-key (kbd "C-c C-c") 'comment-region)   ;; Comment region.
	    (global-set-key (kbd "C-c C-u") 'uncomment-region) ;; Uncomment region.

	    ;;; Compilation key bindings.
	    (global-set-key (kbd "C-c f") 'go-save-and-compile-program)
	    (global-set-key (kbd "C-c t") 'go-save-and-test-program)

	    ;; Call coverage binding.
	    (global-set-key (kbd "C-c c") '(lambda() (interactive) (go-coverage "/tmp/coverprofile")))

            ;; Snippets management.
	    (yas-global-mode 1)

	    ;; Linter.
	    (setenv "GO111MODULE" "on")
	    (setq flycheck-golangci-lint-fast t)
	    (flycheck-golangci-lint-setup)

	    (go-eldoc-setup)             ;; Init eldoc.
	    (go-guru-hl-identifier-mode) ;; Enable symbol highlight.

	    ;; Make a lighter godoc-at-point using popup.
	    ;; TODO: Pass buffer content via stdin to make it work with edited buffer.
	    ;; TODO: Handle errors.
	    (global-set-key (kbd "C-c s")
			    (lambda ()
			      (interactive)
			      (setq rawdoc (shell-command-to-string
					    (concat "gogetdoc -json -pos "
						    buffer-file-name ":#"
						    (format "%s" (point)))))
			      (let* ((json-object-type 'hash-table)
				     (json-array-type 'list)
				     (json-key-type 'string)
				     (json (json-read-from-string rawdoc)))
				(popup-tip (concat "\n" (gethash "doc" json)))
				)))
            ))

(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(with-eval-after-load 'flycheck
  (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode))

(with-eval-after-load 'flycheck
  (custom-set-faces
   '(popup-tip-face ((t (:background "color-100" :foreground "color-233" :weight bold))))
   )
  )

(setq vc-follow-symlinks t)
;(setq-default flycheck-disabled-checkers '(go-vet))
