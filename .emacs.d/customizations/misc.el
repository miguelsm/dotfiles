;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(put 'dired-find-alternate-file 'disabled nil)

(global-undo-tree-mode)

(defun eshell/reset ()
  "Clear the eshell buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)))

;; google-this
(google-this-mode 1)

;; Eshell prompt
(setq eshell-prompt-function
      (lambda ()
        (concat (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) " # " " λ "))))

(setq eshell-prompt-regexp "^[^#λ\n]* [#λ] ")

;; pdf-tools HiDPI
(setq pdf-view-use-scaling 1)
(pdf-loader-install)
