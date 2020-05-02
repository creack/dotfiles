;; Better undo.
(use-package undo-tree
  :delight
  :demand t
  :bind
  (:map undo-tree-map ("C-x u" . hydra-undo-tree/body))
  :init
  (defhydra hydra-undo-tree (:hint nil)
    "
  _p_: undo  _n_: redo _s_: save _l_: load   "
    ("p"   undo-tree-undo)
    ("n"   undo-tree-redo)
    ("s"   undo-tree-save-history)
    ("l"   undo-tree-load-history)
    ("u"   undo-tree-visualize "visualize" :color blue)
    ("q"   nil "quit" :color blue))
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `((".*" . , temporary-file-directory)))
  :config
  (global-undo-tree-mode)
  )
