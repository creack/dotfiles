
;; Briefly highlight cursor position when moving around.
(use-package beacon
  :unless window-system

  :init
  (setq
    beacon-blink-when-point-moves-vertically 1
    beacon-blink-when-focused 1
    )
  :hook (after-init . beacon-mode)
  )


;; (add-hook 'after-make-frame-functions
;;   (lambda (frame)
;;     (if (display-graphic-p frame)
;;       (with-selected-frame frame
;;         (message "hello graphic %s" frame)
;;         )
;;       (with-selected-frame frame
;;         (lambda ()
;;           (message "hello terminal %s" frame)
;;           (xterm-mouse-mode 1)
;;           )
;;         )
;;       )
;;     ))
