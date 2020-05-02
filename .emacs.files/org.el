(use-package sqlite)

(use-package org-roam
  :init
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (when (display-graphic-p frame)
        (org-roam-mode))))
  :custom
  (org-roam-directory "~/org-roam/")
  :bind
  (:map org-roam-mode-map
    (("C-c n l" . org-roam)
      ("C-c n f" . org-roam-find-file)
      ("C-c n b" . org-roam-switch-to-buffer)
      ("C-c n g" . org-roam-graph))
    :map org-mode-map
    (("C-c n i" . org-roam-insert)))
  )

(setq org-plantuml-jar-path (expand-file-name "~/.emacs.d/plantuml.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
(org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("emacs-lisp" "plantuml"))))
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
	 (java . t)
	 (dot . t)
	 (ditaa . t)
	 (R . t)
	 (python . t)
	 (ruby . t)
	 (gnuplot . t)
	 (clojure . t)
	 (sh . t)
	 (ledger . t)
	 (org . t)
	 (plantuml . t)
	 (latex . t))))
