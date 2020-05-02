(use-package fira-code-mode
  :custom
  (fira-code-mode-disabled-ligatures '(":" "[]" "#{" "#(" "#_" "#_(" "x")) ;; List of ligatures to turn off
  :preface
  (defun creack/set-fonts ()
    (set-face-attribute 'default nil :font "Fira Code-12")
    (set-face-attribute 'mode-line nil :font "DejaVu Sans Mono-8")
    )
  (add-hook 'after-make-frame-functions
    (lambda (frame)
      (select-frame frame)
      (when (display-graphic-p frame) (creack/set-fonts))))
  :init
  ; (set-frame-font "Fira Code-11")                         ;; Set Fira Code font.
  (add-to-list 'default-frame-alist '(cursor-type . hbar)) ;; Use a bar as a cusor.
  (unbind-key "C-z")                                      ;; Disable suspend.
  :hook
  (prog-mode . fira-code-mode)
  )

;; Install graphical icons.
(use-package all-the-icons
  :config
  (when window-system
    (when (not (member "all-the-icons" (font-family-list)))
      (all-the-icons-install-fonts t)
      ))
  )
(use-package all-the-icons-dired)

;; Install presentation mode to quickly zoom in/out.
(use-package presentation)

;; Install posframe to display various mods in a frame.
(use-package posframe
  :custom
  (posframe-mouse-banish t) ;; Posframe doesn't support mouse. Disable to avoid conflict.
  )


;; ;; Show ivy frame using posframe.
;; (use-package ivy-posframe
;;   :delight

;;   :custom
;;   (ivy-posframe-parameters
;;     '((left-fringe . 5)
;;        (right-fringe . 5)))
;;   :hook
;;   (ivy-mode . ivy-posframe-enable)
;;   )

;; Show company using posframe.
(use-package company-posframe
  :delight
  :hook (company-mode . company-posframe-mode)
  )

;; Show pretty icons for company.
(use-package company-box
  :delight
  :hook (company-mode . company-box-mode)
  :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :config
  (setq company-box-backends-colors nil)
  (setq company-box-show-single-candidate t)
  (setq company-box-max-candidates 50)

  (defun company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
          ((featurep sym) 'Module)
          ((facep sym) 'Color)
          ((boundp sym) 'Variable)
          ((symbolp sym) 'Text)
          (t . nil)))))

  (with-eval-after-load 'all-the-icons
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-fileicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
      `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
         (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
         (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
         (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
         (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
         (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
         (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
         (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
         (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
         (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
         (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
         (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
         (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
         (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
         (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
         (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
         (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
         (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
         (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
         (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
         (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
         (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
         (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
         (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
         (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
         (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
         (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))

(use-package ace-window
  :functions hydra-frame-window/body
  :bind
  ("C-M-o" . hydra-frame-window/body)
  ("C-M-m" . ladicle/toggle-window-maximize)
  :custom
  (aw-keys '(?j ?k ?l ?i ?o ?h ?y ?u ?p))
  :custom-face
  (aw-leading-char-face ((t (:height 4.0 :foreground "#f1fa8c"))))
  :preface
  (defvar is-window-maximized nil)
  (defun ladicle/toggle-window-maximize ()
    (interactive)
    (progn
      (if is-window-maximized
        (balance-windows)
        (maximize-window))
      (setq is-window-maximized
        (not is-window-maximized))))
  (defun hydra-title(title) (propertize title 'face `(:inherit font-lock-warning-face :weight bold)))
  (defun command-name(title) (propertize title 'face `(:foreground "#f8f8f2")))
  (defun spacer() (propertize "." 'face `(:foreground "#282a36")))
  :config
  (use-package rotate
    :bind
    ("M-o SPC" . rotate-layout))
  (with-eval-after-load 'hydra
    (defhydra hydra-frame-window (:color blue :hint nil)
      (format
        (format "%s" (propertize "                                                                       ╔════════╗
    ((%s))^^^^^^^^   ((%s))^^^^  ((%s))^^  ((%s))^^  ((%s))^^^^^^  ((%s))^   ║ Window ║
^^^^^^ ──────────────────────────────────────────────────────────────────────╨────────╜
        ^_k_^        %s_+_         _-_       %s     _,_ ← %s → _._^  %s
        ^^↑^^          ^↑^         ^↑^       %s
    _h_ ←   → _l_   ^^%s%s^^^^^    ^%s    ^^^%s^^^^     %s
        ^^↓^^          ^↓^         ^↓^       %s^^       %s
        ^_j_^        %s_=_         _/_       %s
^^^^^^ ┌──────────────────────────────────────────────────────────────────────────────┘
                           [_q_]: %s, [_<SPC>_]: %s" 'face `(:inherit font-lock-doc-face)))
        (hydra-title "Size")
        (hydra-title "Zoom")
        (hydra-title "Split")
        (hydra-title "Window")
        (hydra-title "Buffer")
        (hydra-title "Misc")
        (all-the-icons-material "zoom_in" :height .85 :face 'font-lock-doc-face)
        (command-name "_o_ther")
        (command-name "page")
        (command-name "_r_centf")
        (command-name "_s_wap")
        (all-the-icons-faicon "slideshare" :height .85 :face 'font-lock-doc-face)
        (command-name "_p_mode")
        (command-name "w_i_ndow")
        (command-name "_m_aximize")
        (command-name "_s_witch")
        (command-name "_d_elete")
        (command-name "_D_elete")
        (all-the-icons-material "zoom_out" :height .85 :face 'font-lock-doc-face)
        (command-name "del_O_thers")
        (command-name "quit")
        (command-name "rotate")
        )

      ("K" kill-current-buffer :exit t)
      ("D" kill-buffer-and-window :exit t)
      ("O" delete-other-windows  :exit t)
      ("F" toggle-frame-fullscreen)
      ("i" ace-window)
      ("S" ace-swap-window :exit t)
      ("d" ace-delete-window)
      ("m" ladicle/toggle-window-maximize :exit t)
      ("=" text-scale-decrease)
      ("+" text-scale-increase)
      ("-" split-window-vertically)
      ("/" split-window-horizontally)
      ("h" shrink-window-horizontally)
      ("k" shrink-window)
      ("j" enlarge-window)
      ("l" enlarge-window-horizontally)
      ("," previous-buffer)
      ("." next-buffer)
      ("o" other-window)
      ("p" presentation-mode)
      ("r" counsel-recentf :exit t)
      ("s" counsel-switch-buffer :exit t)
      ("D" kill-buffer-and-window)
      ("<SPC>" rotate-layout)
      ("q" nil)))
  )

(use-package which-key
  :delight
  :hook (after-init . which-key-mode)
  :config
  (use-package which-key-posframe
    :config (which-key-posframe-mode))
  )

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  :config
  (use-package dired-git-info
    :bind
    (:map dired-mode-map
      (")" . dired-git-info-mode))
    :hook
    (dired-after-readin . dired-git-info-auto-enable)
    )
  )
