(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; https://emacs.stackexchange.com/a/12844/19224
(defun org-insert-source-block (name language switches header)
  "Asks name, language, switches, header.
Inserts org-mode source code snippet"
  (interactive "sname?
slanguage?
sswitches?
sheader? ")
  (insert
   (if (string= name "")
       ""
     (concat "#+NAME: " name))
   (format "
#+BEGIN_SRC %s %s %s

#+END_SRC" language switches header))
  (forward-line -1)
  (goto-char (line-end-position)))
