;;; modes-config.el --- Major/minor modes configuration for emacs. ;;; -*- mode: elisp -*-

;; Use conf-space for .conf and .setup files.
(use-package conf-mode
  :mode (("\\.conf\\'"    . conf-space-mode)
         ("\\.setup.*\\'" . conf-space-mode))
  )

(use-package plantuml-mode ;; TODO: Autodownload plantuml.jar.
  :defer
  :config
  (setq plantuml-jar-path "~/.emacs.d/plantuml.jar")
  :mode "\\.puml\\'" "\\.uml\\'"
  )

(use-package dockerfile-mode
  :mode "Dockerfile" "\\'Dockerfile."
  :hook
  (dockerfile-mode . display-line-numbers-mode)
  )
(use-package docker-compose-mode)


(use-package makefile-mode
  :ensure nil
  :mode "Makefile" "\\.mk\\'"
  :hook
  (makefile-mode . display-line-numbers-mode)
  )

(use-package markdown-mode)

(use-package json-mode)

(use-package feature-mode) ;; Cucumber / Gherkin.

(use-package terraform-mode
  :hook
  (terraform-mode . yas-minor-mode)
  )

(use-package protobuf-mode
  :hook
  (protobuf-mode . (lambda() (c-add-style "pbstyle" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))
  (protobuf-mode . display-line-numbers-mode)
  (protobuf-mode . yas-minor-mode)
  )

(use-package yaml-mode
  :hook
  (yaml-mode . display-line-numbers-mode)
  )

(use-package sh-mode
  :ensure f ;; Not an actual package, just a config.
  :hook
  (sh-mode . yas-minor-mode)
)
