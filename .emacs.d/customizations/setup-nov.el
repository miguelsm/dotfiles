(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.25))

(setq nov-text-width 80)

(add-hook 'nov-mode-hook 'my-nov-font-setup)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
