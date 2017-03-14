;;; sql-mode --- Emacs SQL configuration

;;; Commentary:

;;; Code:

(add-hook 'sql-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
	    )
	  )

;;; sql-mode.el ends here
