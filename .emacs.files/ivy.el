)
  :custom
  ;(ivy-use-selectable-prompt t)

  (ivy-on-del-error-function nil) ; Don't yield and error when deleting readonly part of the prompt.
  (counsel-grep-base-command "ag -S --noheading --nocolor --nofilename --numbers '%s' %s")
  :config

  (all-the-icons-ivy-setup) ;; Setup the icons.
  )


;; NOTE: ivy-rich icons break things like package-install details.
(use-package all-the-icons-ivy
  :defer
  :config (all-the-icons-ivy-setup))
