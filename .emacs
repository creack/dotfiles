;; -*- lexical-binding: t; -*-

;; (toggle-debug-on-error)
(package-initialize)
(org-babel-load-file "~/.emacs.files/init.org")

(use-package emacs
  :disabled
  :preface
  (defun creack/check-features()
    (if (functionp 'json-serialize)
	(message "Native JSON is available")
      (message "Native JSON is *not* available")
      )

    (if (and
         (fboundp 'native-comp-available-p)
         (native-comp-available-p)
         )
	(message "Native compilation is available")
      (message "Native complation is *not* available")
      )
    )
  :config
  (creack/check-features)
  )

(use-package treemacs
  :bind
  ("<f5>" . treemacs)

  :preface
  (defun creack/treemacs-ignore-node_modules (_ absolute-path)
    (string-match-p ".*/node_modules/.*" absolute-path)
    )
  (defun creack/treemacs-ignore-vendor (_ absolute-path)
    (string-match-p ".*/vendor/.*" absolute-path)
    )

  :config
  (add-to-list 'treemacs-ignored-file-predicates #'creack/treemacs-ignore-node_modules)
  (add-to-list 'treemacs-ignored-file-predicates #'creack/treemacs-ignore-vendor)

  :config
  (use-package lsp-treemacs
    :hook
    (lsp-after-open . lsp-treemacs-sync-mode)
    )
  (use-package treemacs-projectile
    :after projectile
    )

  :custom
  (treemacs-is-never-other-window t)
  (treemacs-show-hidden-files     nil)
  (treemacs-width                 20)
  )

(use-package yasnippet
  :after hydra
  :bind
  ("<f2>"    . hydra-yas/body)
  ("C-c h y" . hydra-yas/body)
  :hydra
  (hydra-yas
   (:color blue :hint nil)
   "
               ^YASnippets^
 --------------------------------------------
   Modes:    Load/Visit:    Actions:

  _g_lobal  _d_irectory    _i_nsert
  _m_inor   _f_ile         _t_ryout
  _e_xtra   _l_ist         _n_ew
          _a_ll
 "
   ("d" yas-load-directory)
   ("e" yas-activate-extra-mode)
   ("i" yas-insert-snippet)
   ("f" yas-visit-snippet-file :color blue)
   ("n" yas-new-snippet)
   ("t" yas-tryout-snippet)
   ("l" yas-describe-tables)
   ("g" yas/global-mode)
   ("m" yas/minor-mode)
   ("a" yas-reload-all)))


(use-package origami
  :after hydra
  :pin celpa

  :bind
  ("C-c h o" . hydra-origami/body)
  ("<f3>"    . hydra-origami/body)
  (:map origami-mode-map
	("C-c C-f" . hydra-origami/origami-recursively-toggle-node)
	("C-c C-a" . hydra-origami/origami-toggle-all-nodes)

	([down-mouse-2] . mouse-drag-region)
	([mouse-2]      . origami-recursively-toggle-node)
	)

  ;; :config
  ;; (global-origami-mode)

  :hydra
  (hydra-origami
   (:color red :hint nil)
   "
               ^Origami^
 --------------------------------------------
 _t_: toggle        _r_: redo    _p_: prev        _c_: close all
 _a_: toggle all    _u_: undo    _n_: next        _o_: open all
 _q_: quit
 "
   ("t" origami-recursively-toggle-node)
   ("a" origami-toggle-all-nodes)
   ("u" origami-undo)
   ("r" origami-redo)
   ("p" origami-previous-fold)
   ("n" origami-next-fold)
   ("o" origami-open-all-nodes)
   ("c" origami-close-all-nodes)
   ("q" nil "Quit" :color blue)
   )
  )

(use-package lsp-origami
  :after origami
  )
					;(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(use-package ediff
  :bind
  ("C-c h d" . hydra-ediff/body)
  :hydra
  (hydra-ediff
   (:color blue :hint nil)
   "
 ^Buffers           Files           VC                     Ediff regions
 ----------------------------------------------------------------------
 _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
 _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                   _c_urrent file
 "
   ("b" ediff-buffers)
   ("B" ediff-buffers3)
   ("=" ediff-files)
   ("f" ediff-files)
   ("F" ediff-files3)
   ("c" ediff-current-file)
   ("r" ediff-revision)
   ("l" ediff-regions-linewise)
   ("w" ediff-regions-wordwise)
   )
  )

(use-package dotenv-mode
  )

(use-package manage-minor-mode
  :defer)

(use-package ivy-hydra
  :after ivy)

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-dispatch-always t)
  :bind
  ("M-o" . ace-window)
  )

(use-package tab-bar
  :after hydra
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t)
  :config
  ;; (tab-bar-mode)
  (tab-bar-history-mode)
  :bind
  ("C-x t"   . hydra-tab-bar/body)
  ("C-c h t" . hydra-tab-bar/body)
  :hydra
  (hydra-tab-bar
   (:color amaranth)
   "Tab Bar Operations"
   ("t"   tab-new             "Create a new tab" :column "Creation")
   ("d"   dired-other-tab     "Open Dired in another tab")
   ("f"   find-file-other-tab "Find file in another tab")
   ("0"   tab-close           "Close current tab")

   ("m"   tab-move   "Move current tab" :column "Management")
   ("r"   tab-rename "Rename Tab")

   ("RET" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
   ("l"   tab-next                   "Next Tab")
   ("j"   tab-previous               "Previous Tab")

   ("q" nil "Exit" :exit t)
   )
  )

(use-package git-gutter :delight
	     :config
	     (set-face-foreground 'git-gutter:separator "#49483E")
	     (set-face-foreground 'git-gutter:modified  "#66D9EF")
	     (set-face-foreground 'git-gutter:added     "#A6E22E")
	     (set-face-foreground 'git-gutter:deleted   "#F92672")
	     (set-face-background 'git-gutter:separator nil)
	     (set-face-background 'git-gutter:modified  nil)
	     (set-face-background 'git-gutter:added     nil)
	     (set-face-background 'git-gutter:deleted   nil)
	     ;; :custom
	     ;; (git-gutter:window-width 2)
	     ;; (git-gutter:modified-sign "☁")
	     ;; (git-gutter:added-sign "☀")
	     ;; (git-gutter:deleted-sign "☂")
	     ;; :bind ("C-x C-g" . git-gutter-mode)
	     ;; :hook
	     ;; (after-init . global-git-gutter-mode)
	     )

(use-package emacs
  :after hydra
  :bind
  ("M-g g"   . hydra-goto-line/body)
  ("C-c h g" . hydra-goto-line/body)
  :hydra
  (hydra-goto-line
   (goto-map ""
	     :pre  (display-line-numbers-mode 1)
	     :post (display-line-numbers-mode -1)
	     )
   "goto-line"
   ("g" goto-line "go")
   ("RET" nil nil)
   ("m" set-mark-command "mark" :bind nil)
   ("q" nil "quit"))
  )

(use-package emacs
  :disabled
  :after hydra
  :bind
  ("M-y"     . hydra-yank-pop/yank-pop)
  ("C-y"     . hydra-yank-pop/yank)
  ("C-c C-y" . yank)
  :hydra
  (hydra-yank-pop
   ()
   "yank"
   ("C-y" yank             nil)
   ("M-y" yank-pop         nil)
   ("y"   (yank-pop 1)     "next")
   ("Y"   (yank-pop -1)    "prev")
   ("l"   browse-kill-ring "list"    :color blue)
   ("c"   counsel-yank-pop "counsel" :color blue)
   )
  )


(use-package emacs
  :after hydra
  :bind
  ("C-n"     . hydra-move/next-line)
  ("C-p"     . hydra-move/previous-line)
  ("C-c h m" . hydra-move/body)
  :hydra
  (hydra-move
   (:body-pre (next-line))
   "move"
   ("n" next-line)
   ("p" previous-line)
   ("f" forward-char)
   ("b" backward-char)
   ("a" beginning-of-line)
   ("e" move-end-of-line)
   ("v" scroll-up-command)
   ;; Converting M-v to V here by analogy.
   ("V" scroll-down-command)
   ("l" recenter-top-bottom)
   )
  )

(use-package emacs
  :after hydra
  :bind
  ("<f4>"    . hydra-lsp/body)
  ("C-c h l" . hydra-lsp/body)
  :hydra
  (hydra-lsp
   (:exit t :hint nil)
   "
  Buffer^^               Server^^                   Symbol
 -------------------------------------------------------------------------------------
  [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
  [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
  [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature"
   ("d" lsp-find-declaration)
   ("D" lsp-ui-peek-find-definitions)
   ("R" lsp-ui-peek-find-references)
   ("i" lsp-ui-peek-find-implementation)
   ("t" lsp-find-type-definition)
   ("s" lsp-signature-help)
   ("o" lsp-describe-thing-at-point)
   ("r" lsp-rename)

   ("f" lsp-format-buffer)
   ("m" lsp-ui-imenu)
   ("x" lsp-execute-code-action)

   ("M-s" lsp-describe-session)
   ("M-r" lsp-restart-workspace)
   ("S"   lsp-shutdown-workspace))
  )

(use-package use-package-chords
  :config
  (key-chord-mode 1)
  )

(use-package ace-jump-mode
  :chords
  ("jj" . ace-jump-char-mode)
  ("jk" . ace-jump-word-mode)
  ("jl" . ace-jump-line-mode)
  ("gg" . hydra-goto-line/goto-line)
  )

(use-package beacon :delight
	     :pin melpa
	     :config
	     (beacon-mode 1)
	     :custom
	     (beacon-blink-when-window-scrolls           nil)
	     (beacon-blink-when-point-moves-horizontally 10)
	     (beacon-blink-when-point-moves-vertically   40)
	     )

(use-package org-roam :delight " Roam"
	:hook
	(after-init . org-roam-mode)
	(org-mode . (lambda ()
			          (set (make-local-variable 'time-stamp-pattern)
				          "8/^#\\+LAST_MODIFIED:[ \t]+\\\\?[\"<]+%%\\\\?[\">]")
			          ))
	(before-save . (lambda ()
			             (when (eq major-mode 'org-mode)
				             (time-stamp)
				             )
			             )
		)
	:custom
	(org-roam-directory "~/.emacs.files/roam/")
	:custom ; Custom vars for time-stamp org-mode.
	(time-stamp-active t)
	(time-stamp-format "%Y-%m-%d %3a %02I:%02M%P %Z")

	:custom ; Custom varsfor org-roam-dailies.
	(org-roam-dailies-directory "daily/")
	(org-roam-dailies-capture-templates
	  '(
		   ("d" "default" entry
		     #'org-roam-capture--get-point
		     "* [%<%02I:%02M%P>] %?\n%i\n%a"
		     :file-name "daily/%<%Y-%m-%d>"
		     :empty-lines 1 ; Add a new-line before and after the entry.
		     :head "#+TITLE: %<%Y-%m-%d>\n#+CREATED: [%<%Y-%m-%d %3a %I:%M%p %Z>]\n#+LAST_MODIFIED: <>\n#+ROAM_ALIAS: \n#+ROAM_TAGS: "
		     :unnarrowed t ; When nil, hide the rest of the file, showing only new content.
		     )
		   ("l" "lab" entry
		     #'org-roam-capture--get-point
		     "* %?"
		     :file-name "daily/%<%Y-%m-%d>"
		     :head "#+TITLE: %<%Y-%m-%d>\n#+CREATED: [%<%Y-%m-%d %3a %I:%M%p %Z>]\n#+LAST_MODIFIED: <>\n#+ROAM_ALIAS: \n#+ROAM_TAGS: "
		     :clock-in t
         :clock-keep t
         :clock-resume t
		     :unnarrowed t ; When nil, hide the rest of the file, showing only new content.
		     :olp ("Lab notes with clock")
		     )
		   ("j" "journal" entry
		     #'org-roam-capture--get-point
		     "* %?"
		     :file-name "daily/%<%Y-%m-%d>"
		     :head "#+TITLE: %<%Y-%m-%d>\n#+CREATED: [%<%Y-%m-%d %3a %I:%M%p %Z>]\n#+LAST_MODIFIED: <>\n#+ROAM_ALIAS: \n#+ROAM_TAGS: "
		     :time-prompt t
		     :unnarrowed t ; When nil, hide the rest of the file, showing only new content.
		     :olp ("Journal")
		     )
		   ))
	:bind
	(:map org-roam-mode-map
		("C-c n l" . org-roam)
		("C-c n r" . org-roam-buffer-toggle-display)
		("C-c n b" . org-roam-switch-to-buffer)
		("C-c n d" . org-roam-find-directory)
		("C-c n f" . org-roam-find-file)
		("C-c n g" . org-roam-graph)
		("C-c n c" . org-roam-capture)
		("C-c n t" . org-roam-dailies-capture-today)
		("C-c n T" . org-roam-dailies-find-today)
		)
	:bind
	(:map org-mode-map
		("C-c n i" . org-roam-insert)
		("C-c n I" . org-roam-insert-immediate)
		)
	)


 ;;;;;;;;;;


(use-package eglot
  ;; :config
  ;; (add-to-list 'eglot-server-programs '(terraform-mode "terraform-lsp"))
  :config
  ;; NOTE: Doesn't work as a :custom setting. Needs to be (setq-default), in :config.
  (setq-default eglot-workspace-configuration
		'((:gopls .
			  (
			    ;; (staticcheck . t)
			    (completeUnimported          . t)
			    (usePlaceholders             . t)
			    (expandWorkspaceToModule     . t)
			    (experimentalWorkspaceModule . t)
			    ))))
  )

(use-package flymake
  :delight ;; Hide Flymake from the modeline.
  )

(use-package flycheck-golangci-lint
  :after (flycheck)
  :hook
  (flycheck-mode  . flycheck-golangci-lint-setup)
  (lsp-after-open . (lambda() (flycheck-add-next-checker 'lsp 'golangci-lint 'append)))
  )


(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 4)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "03:00")
  )

(use-package flycheck-yamllint
  :after (flycheck)
  :ensure-system-package
  (yamllint . "pip3 install yamllint")
  :config
  (flycheck-yamllint-setup)
  :hook
  (yaml-mode     . flycheck-mode)
  )

(use-package gitlab-ci-mode
  :after (flycheck)
  :config
  (flycheck-add-mode 'yaml-yamllint 'gitlab-ci-mode)
  )
