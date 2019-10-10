;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Make more room
(when (fboundp 'fringe-mode)
  (fringe-mode -1))

;; Show line numbers
(setq nlinum-format "%3d\u2502 ")
(add-hook 'prog-mode-hook 'nlinum-mode)

;; Highlight numbers
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Highlight current symbol
(add-hook 'prog-mode-hook 'auto-highlight-symbol-mode)

;; Highlight column 80
(add-hook 'prog-mode-hook 'fci-mode)
(setq fci-rule-column 80)
(setq fci-rule-width 1)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Color Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(load-theme 'acme t)
(setq fci-rule-color "#dee0c7")

;; (load-theme 'alabaster t)
;; (setq fci-rule-color "#f0f0f0")

;; (load-theme 'plastic t)
;; (setq fci-rule-color "#323232")

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 x-select-enable-clipboard t

 ;; I'm actually not sure what this does but it's recommended?
 x-select-enable-primary t

 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; Reverse colors for the border to have nicer line
(set-face-inverse-video-p 'vertical-border nil)
(set-face-background 'vertical-border (face-background 'default))

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border 9474)

;; line highlighting
(global-hl-line-mode 1)

;; disable bold text
(mapc
 (lambda (face)
   (set-face-attribute face nil :weight 'normal :underline nil))
 (face-list))

;; display time, load average and battery
(display-time)
(display-time-next-load-average)
(display-battery-mode)
