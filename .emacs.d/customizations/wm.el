;;
;; exwm
;;

(require 'exwm)
(require 'exwm-config)
(exwm-config-default)
(server-start)

;; jump to buffers with s-[hjkl]
(exwm-input-set-key (kbd "s-h") #'windmove-left)
(exwm-input-set-key (kbd "s-j") #'windmove-down)
(exwm-input-set-key (kbd "s-k") #'windmove-up)
(exwm-input-set-key (kbd "s-l") #'windmove-right)
;; swap buffers with C-s-[hjkl]
(exwm-input-set-key (kbd "C-s-h") 'buf-move-left)
(exwm-input-set-key (kbd "C-s-j") 'buf-move-down)
(exwm-input-set-key (kbd "C-s-k") 'buf-move-up)
(exwm-input-set-key (kbd "C-s-l") 'buf-move-right)
;; resize buffers
(exwm-input-set-key (kbd "s-[") 'shrink-window-horizontally)
(exwm-input-set-key (kbd "s-{") 'shrink-window)
(exwm-input-set-key (kbd "s-]") 'enlarge-window-horizontally)
(exwm-input-set-key (kbd "s-}") 'enlarge-window)

;; https://github.com/ch11ng/exwm/wiki#an-issue-with-ediff-mode-in-magit
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-sensibly)

;; (defun pnh-trim-non-ff ()
;;   (delete-if-not (apply-partially 'string-match "- Mozilla Firefox$")
;;                  ido-temp-list))

(add-hook 'exwm-manage-finish-hook
          (defun pnh-exwm-manage-hook ()
            (when (string-match "Firefox" exwm-class-name)
              (exwm-workspace-move-window 7)
              ;; (setq ido-make-buffer-list-hook 'pnh-trim-non-ff)
              )))

(add-hook 'exwm-update-title-hook
          (defun pnh-exwm-title-hook ()
            (when (string-match "Firefox" exwm-class-name)
              (exwm-workspace-rename-buffer exwm-title))))

(setq browse-url-firefox-arguments '("-new-window"))

(desktop-environment-mode)

(global-set-key (kbd "s-&") 'helm-xstarter)
