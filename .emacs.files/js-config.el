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

	    ;; Keybinding for autocomplete.
	    (global-set-key (kbd "C-c .") 'tern-ac-complete)

	    ;; Disable jshint in favor of eslint.
	    (flycheck-add-mode 'javascript-eslint 'web-mode)
	    (setq-default flycheck-disabled-checkers
			  (append flycheck-disabled-checkers
				  '(javascript-jshint)))
            )
	  )

;; Enable tern.
(add-hook 'web-mode-hook (lambda () (tern-mode t)))

;; Init tern auto completion.
(eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))


;; Highlight JSX tags.
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))


;; Helper to kill tern.
(defun stop-tern-process ()
  (interactive)
  (delete-process "Tern"))

;; Activate web-mode for .js files.
(add-to-list 'auto-mode-alist '("\\.js\\'"      . web-mode))
