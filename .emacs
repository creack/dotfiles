;; -*- lexical-binding: t; -*-

;; (toggle-debug-on-error)
(package-initialize)
(org-babel-load-file "~/.emacs.files/init.org")

(setq
  lsp-log-io nil ; if set to true can cause a performance hit

  lsp-print-performance nil
  )

(with-eval-after-load 'treemacs

  (defun treemacs-ignore-example (_ absolute-path)
      (string-match-p ".*/node_modules/.*" absolute-path)
    )

  (add-to-list 'treemacs-ignored-file-predicates #'treemacs-ignore-example))

(use-package npm-mode)


(bind-key [mouse-2] #'origami-recursively-toggle-node)
;; (profiler-start 'cpu)
;; (save-buffer) ;; Code to profile.
;; (profiler-end)
;; (profiler-find-profile-other-window)

(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
  (message "Native complation is *not* available"))

(setq
 lsp-eslint-server-command '("node" "/home/creack/Downloads/vscode-eslint/server/out/eslintServer.js" "--stdio")
 )
;; (tide-setup)
;; (nvm-use)

(use-package treemacs
  :bind
  ("<f5>" . treemacs)
  :config
  (use-package lsp-treemacs
    :disabled
    :custom
    (lsp-treemacs-error-list-severity 4)
    :config
    (lsp-treemacs-sync-mode 1)
    )
  (use-package treemacs-projectile
    :after projectile
    )
  :custom
  (treemacs-is-never-other-window t)
  (treemacs-show-hidden-files nil)
  (treemacs-width 20)
  )

(use-package yasnippet
  :after hydra
  :bind
  (:map yas-minor-mode-map
    ("<f2>" . hydra-yas/body)
    ("C-c h y" . hydra-yas/body)
    )
  :hydra (hydra-yas (:color blue :hint nil)
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
  ("C-c f"   . origami-recursively-toggle-node)
  ("C-c a"   . origami-toggle-all-nodes)
  :hydra (hydra-origami (:color red :hint nil)
    "
              ^Origami^
--------------------------------------------
_t_: toggle    _r_: redo    _p_: prev        _c_: close all
_u_: undo      _n_: next    _o_: open all    _q_: quit
"
    ("t" origami-recursively-toggle-node)
    ("u" origami-undo)
    ("r" origami-redo)
    ("p" origami-previous-fold)
    ("n" origami-next-fold)
    ("o" origami-open-all-nodes)
    ("c" origami-close-all-nodes)
    ("q" nil "Quit" :color blue))
  ;:config
  ;(global-origami-mode)
 )

(use-package lsp-origami
  :after origami
  )
;(add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(use-package ediff
  :bind ("C-c h d" . hydra-ediff/body)
  :hydra (hydra-ediff (:color blue :hint nil)
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
           ("w" ediff-regions-wordwise))
  )

(use-package dotenv-mode
  )

;;(use-package doom-themes
;;  :disabled
;;  :config (load-theme 'doom-gruvbox t)
;;  )
;; (tide-format-before-save)
(use-package manage-minor-mode
  :defer)

(use-package ivy-hydra
  :after ivy)

(use-package ace-window
  :custom
  (aw-scope 'frame)
  (aw-dispatch-always t)
  ;;:bind ("C-x o" . ace-window)
  :bind ("M-o" . ace-window)
  )

(use-package tab-bar
  :after hydra
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice t)
  :config
  ;(tab-bar-mode)
  (tab-bar-history-mode)
  :bind
  ("C-x t" . hydra-tab-bar/body)
  ("C-c h t" . hydra-tab-bar/body)
  :hydra ( hydra-tab-bar (:color amaranth)
           "Tab Bar Operations"
           ("t" tab-new "Create a new tab" :column "Creation")
           ("d" dired-other-tab "Open Dired in another tab")
           ("f" find-file-other-tab "Find file in another tab")
           ("0" tab-close "Close current tab")
           ("m" tab-move "Move current tab" :column "Management")
           ("r" tab-rename "Rename Tab")
           ("RET" tab-bar-select-tab-by-name "Select tab by name" :column "Navigation")
           ("l" tab-next "Next Tab")
           ("j" tab-previous "Previous Tab")
           ("q" nil "Exit" :exit t))
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
  ("M-g"     . hydra-goto-line/body)
  ("C-c h g" . hydra-goto-line/body)
  :hydra (hydra-goto-line (goto-map ""
                            :pre (display-line-numbers-mode 1)
                            :post (display-line-numbers-mode -1))
           "goto-line"
           ("g" goto-line "go")
           ("RET" nil nil)
           ("m" set-mark-command "mark" :bind nil)
           ("q" nil "quit"))
  )

(use-package emacs
  :after hydra
  :bind
  ("M-y" . hydra-yank-pop/yank-pop)
  ("C-y" . hydra-yank-pop/yank)
  ("C-c C-y" . yank)
  :hydra (hydra-yank-pop ()
           "yank"
           ("C-y" yank nil)
           ("M-y" yank-pop nil)
           ("y" (yank-pop 1) "next")
           ("Y" (yank-pop -1) "prev")
           ("l" browse-kill-ring "list" :color blue)
           ("c" counsel-yank-pop "counsel" :color blue)
           )
  )


(use-package emacs
  :after hydra
  :bind
  ("C-n". hydra-move/body)
  ("C-c h m". hydra-move/body)
  :hydra (hydra-move (:body-pre (next-line))
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
           ("l" recenter-top-bottom))
  )

(use-package emacs
  :after (hydra lsp)
  :bind
  (:map lsp-mode-map
    ("<f4>" . hydra-lsp/body)
    ("C-c h l" . hydra-lsp/body)
    )
  :hydra (hydra-lsp (:exit t :hint nil)
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
    ("S" lsp-shutdown-workspace))
  )

;; (global-set-key (kbd "TAB") (lambda() (interactive) (scroll-down 5)))
;; (global-set-key (kbd "<mouse-5>") (lambda() (interactive) (scroll-up 5))))
;; (company-complete-common-or-cycle)

(use-package emacs
  :after hydra
  :bind
  ("C-c h s" . hydra-flyspell/body)
  :preface
  (defun my/flyspell-error-p (&optional position)
    "Return non-nil if at a flyspell misspelling, and nil otherwise."
    ;; The check technique comes from 'flyspell-goto-next-error'.
    (let* ((pos (or position (point)))
           (ovs (overlays-at pos))
           r)
      (while (and (not r) (consp ovs))
        (if (flyspell-overlay-p (car ovs))
            (setq r t)
          (setq ovs (cdr ovs))))
      ))

  (defvar my/hydra-flyspell-direction 'forward)
  (defun my/flyspell-correct-at-point-maybe-next ()
    (interactive)
    (cond ((my/flyspell-error-p)
           (save-excursion
             (flyspell-correct-at-point)))
          ((equal my/hydra-flyspell-direction 'forward)
           (my/flyspell-goto-next-error)
           ;; recheck, for 'my/flyspell-goto-next-error' can legitimately stop
           ;; at the end of buffer
           (when (my/flyspell-error-p)
             (save-excursion
               (flyspell-correct-at-point))))
          ((equal my/hydra-flyspell-direction 'backward)
           (my/flyspell-goto-previous-error)
           ;; recheck, for 'my/flyspell-goto-previous-error' can legitimately
           ;; stop at the beginning of buffer
           (when (my/flyspell-error-p)
             (save-excursion
               (flyspell-correct-at-point))))))

  ;; Just an adapted version of 'flyspell-goto-next-error'.
  (defun my/flyspell-goto-previous-error ()
    "Go to the previous previously detected error.
In general FLYSPELL-GOTO-PREVIOUS-ERROR must be used after
FLYSPELL-BUFFER."
    (interactive)
    (setq my/hydra-flyspell-direction 'backward)
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto end of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the previous error
      (while (and (> pos min)
                  (not (my/flyspell-error-p pos)))
        (setq pos (1- pos)))
      (goto-char pos)
      (when (eq (char-syntax (preceding-char)) ?w)
        (backward-word 1))
      ;; save the current location for next invocation
      (setq flyspell-old-pos-error (point))
      (setq flyspell-old-buffer-error (current-buffer))
      (if (= pos min)
          (message "No more miss-spelled word!")))
    ;; After moving, check again if we are at a misspelling (accepting a word
    ;; might have changed this, since the last check).  If not, go to the next
    ;; error again, unless we are at point-min (otherwise we might enter into
    ;; infinite loop, if there are no remaining errors).
    (flyspell-word)
    (unless (or (= (point) (point-min))
                (my/flyspell-error-p))
      (my/flyspell-goto-previous-error))
    (when (my/flyspell-error-p)
      (swiper--ensure-visible)))

  (defun my/flyspell-goto-next-error ()
    "Go to the next previously detected error.
In general FLYSPELL-GOTO-NEXT-ERROR must be used after
FLYSPELL-BUFFER."
    ;; Just a recursive wrapper on the original flyspell function, which takes
    ;; into account possible changes of accepted words since the last check.
    (interactive)
    (setq my/hydra-flyspell-direction 'forward)
    (flyspell-goto-next-error)
    ;; After moving, check again if we are at a misspelling.  If not, go to
    ;; the next error again.
    (flyspell-word)
    (unless (or (= (point) (point-max))
                (my/flyspell-error-p))
      (my/flyspell-goto-next-error))
    (when (my/flyspell-error-p)
      (swiper--ensure-visible)))

  (defun my/flyspell-accept-word (&optional local-dict lowercase)
    "Accept word at point for this session.
If LOCAL-DICT is non-nil, also add it to the buffer-local
dictionary. And if LOWERCASE is non-nil, do so with the word
lower-cased."
    (interactive)
    (let ((word (flyspell-get-word)))
      (if (not (and (consp word)
                    ;; Check if we are actually at a flyspell error.
                    (my/flyspell-error-p (car (cdr word)))))
          (message "Point is not at a misspelling.")
        (let ((start (car (cdr word)))
              (word (car word)))
          (when (and local-dict lowercase)
            (setq word (downcase word)))
          ;; This is just taken (slightly adjusted) from
          ;; 'flyspell-do-correct', which is essentially what
          ;; 'ispell-command-loop' also does.
          (ispell-send-string (concat "@" word "\n"))
          (add-to-list 'ispell-buffer-session-localwords word)
          (or ispell-buffer-local-name ; session localwords might conflict
              (setq ispell-buffer-local-name (buffer-name)))
          (flyspell-unhighlight-at start)
          (if (null ispell-pdict-modified-p)
              (setq ispell-pdict-modified-p
                    (list ispell-pdict-modified-p)))
          (if local-dict
              (ispell-add-per-file-word-list word))
          (ispell-pdict-save t t)

          ))))
  (defun my/flyspell-accept-word-buffer ()
    "See `my/flyspell-accept-word'."
    (interactive)
    (my/flyspell-accept-word 'local-dict))
  (defun my/flyspell-accept-lowercased-buffer ()
    "See `my/flyspell-accept-word'."
    (interactive)
    (my/flyspell-accept-word 'local-dict 'lowercase))

  :hydra (hydra-flyspell (:color amaranth
                                 :body-pre
                                 (progn
                                   (when mark-active
                                     (deactivate-mark))
                                   (when (or (not (mark t))
                                             (/= (mark t) (point)))
                                     (push-mark (point) t)))
                                 :hint nil)
                         "
 ^Flyspell^         ^Errors^            ^Word^
---------------------------------------------------------
 _b_ check buffer   _c_ correct         _s_ save (buffer)
 _d_ change dict    _n_ goto next       _l_ lowercase (buffer)
 _u_ undo           _p_ goto previous   _a_ accept (session)
 _q_ quit
"
                         ("b" flyspell-buffer)
                         ("d" ispell-change-dictionary)
                         ("u" undo-tree-undo)
                         ("q" nil :color blue)
                         ("C-/" undo-tree-undo)

                         ("c" my/flyspell-correct-at-point-maybe-next)
                         ("n" my/flyspell-goto-next-error)
                         ("p" my/flyspell-goto-previous-error)
                         ("." my/flyspell-correct-at-point-maybe-next)
                         ("SPC" my/flyspell-goto-next-error)
                         ("DEL" my/flyspell-goto-previous-error)

                         ("s" my/flyspell-accept-word-buffer)
                         ("l" my/flyspell-accept-lowercased-buffer)
                         ("a" my/flyspell-accept-word)

                         ("M->" end-of-buffer)
                         ("M-<" beginning-of-buffer)
                         ("C-v" scroll-up-command)
                         ("M-v" scroll-down-command))
  )

(use-package use-package-chords
  :config (key-chord-mode 1)
  )

(use-package ace-jump-mode
  :chords (("jj" . ace-jump-char-mode)
           ("jk" . ace-jump-word-mode)
           ("jl" . ace-jump-line-mode)
           ("gg" . hydra-goto-line/goto-line))
  )

;; (with-eval-after-load 'org
;;   (defvar-local rasmus/org-at-src-begin -1
;;     "Variable that holds whether last position was a ")
;;
;;   (defvar rasmus/ob-header-symbol ?☰
;;     "Symbol used for babel headers")
;;
;;   (defun rasmus/org-prettify-src--update ()
;;     (let ((case-fold-search t)
;;           (re "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*")
;;           found)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (while (re-search-forward re nil t)
;;           (goto-char (match-end 0))
;;           (let ((args (org-trim
;;                        (buffer-substring-no-properties (point)
;;                                                        (line-end-position)))))
;;             (when (org-string-nw-p args)
;;               (let ((new-cell (cons args rasmus/ob-header-symbol)))
;;                 (cl-pushnew new-cell prettify-symbols-alist :test #'equal)
;;                 (cl-pushnew new-cell found :test #'equal)))))
;;         (setq prettify-symbols-alist
;;               (cl-set-difference prettify-symbols-alist
;;                                  (cl-set-difference
;;                                   (cl-remove-if-not
;;                                    (lambda (elm)
;;                                      (eq (cdr elm) rasmus/ob-header-symbol))
;;                                    prettify-symbols-alist)
;;                                   found :test #'equal)))
;;         ;; Clean up old font-lock-keywords.
;;         (font-lock-remove-keywords nil prettify-symbols--keywords)
;;         (setq prettify-symbols--keywords (prettify-symbols--make-keywords))
;;         (font-lock-add-keywords nil prettify-symbols--keywords)
;;         (while (re-search-forward re nil t)
;;           (font-lock-flush (line-beginning-position) (line-end-position))))))
;;
;;   (defun rasmus/org-prettify-src ()
;;     "Hide src options via `prettify-symbols-mode'.
;;
;;   `prettify-symbols-mode' is used because it has uncollpasing. It's
;;   may not be efficient."
;;     (let* ((case-fold-search t)
;;            (at-src-block (save-excursion
;;                            (beginning-of-line)
;;                            (looking-at "^[ \t]*#\\+begin_src[ \t]+[^ \f\t\n\r\v]+[ \t]*"))))
;;       ;; Test if we moved out of a block.
;;       (when (or (and rasmus/org-at-src-begin
;;                      (not at-src-block))
;;                 ;; File was just opened.
;;                 (eq rasmus/org-at-src-begin -1))
;;         (rasmus/org-prettify-src--update))
;;       ;; Remove composition if at line; doesn't work properly.
;;       (when at-src-block
;;         (with-silent-modifications
;;           (remove-text-properties (match-end 0)
;;                                   (1+ (line-end-position))
;;                                   '(composition))))
;;       (setq rasmus/org-at-src-begin at-src-block)))
;;
;;   (defun rasmus/org-prettify-symbols ()
;;     (mapc (apply-partially 'add-to-list 'prettify-symbols-alist)
;;           (cl-reduce 'append
;;                      (mapcar (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
;;                              `(("#+begin_src" . ?⇲) ;; ➤ 🖝 ➟ ➤ ✎
;;                                ("#+end_src"   . ?⇱) ;;
;;                                ("#+header:" . ,rasmus/ob-header-symbol)
;;                                ("#+begin_quote" . ?»)
;;                                ("#+end_quote" . ?«)))))
;;     (turn-on-prettify-symbols-mode)
;;     (add-hook 'post-command-hook 'rasmus/org-prettify-src t t))
;;   (add-hook 'org-mode-hook #'rasmus/org-prettify-symbols))

(use-package beacon
  :pin melpa
  :config
  (beacon-mode 1)
  :custom
  (beacon-blink-when-window-scrolls nil)
  (beacon-blink-when-point-moves-horizontally 10)
  (beacon-blink-when-point-moves-vertically 40)
  )

(use-package org-roam
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
         :head "#+title: %<%Y-%m-%d>\n"
         :clock-in t
         :olp ("Lab notes")
         )
       ("j" "journal" entry
         #'org-roam-capture--get-point
         "* %?"
         :file-name "daily/%<%Y-%m-%d>"
         :head "#+title: %<%Y-%m-%d>\n"
         :time-prompt t
         :olp ("Journal")
         )
       ))
  :bind (:map org-roam-mode-map
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
  :bind (:map org-mode-map
          ("C-c n i" . org-roam-insert)
          ("C-c n I" . org-roam-insert-immediate)
          )
  )
