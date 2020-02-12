;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Cascadia Code" :size 10))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
;; (setq doom-theme 'doom-one)
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'acme t)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/org/")

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;
;; Customizations
;;

;;
;; Editing
;;

(setq doom-leader-key "C-c"
      doom-leader-alt-key "C-c l")

(global-set-key (kbd "C-h") 'delete-backward-char)

;; Search at point https://www.emacswiki.org/emacs/SearchAtPoint#toc3
(global-set-key (kbd "C-S-s") 'isearch-forward-symbol-at-point)

;;
;; eshell
;;

(defun eshell/reset ()
  "Clear the eshell buffer"
  (let ((inhibit-read-only t))
    (erase-buffer)))

(setq eshell-prompt-function
      (lambda ()
        (concat (abbreviate-file-name (eshell/pwd))
                (if (= (user-uid) 0) " # " " λ "))))

(setq eshell-prompt-regexp "^[^#λ\n]* [#λ] ")

;;
;; google-this
;;

(google-this-mode 1)

;;
;; multiple-cursors
;;

(global-set-key (kbd "C-c ;") 'mc/mark-more-like-this-extended)

;;
;; n3-mode
;;

(autoload 'n3-mode "n3-mode" "Major mode for OWL or N3 files" t)

;; Turn on font lock when in n3 mode
(add-hook 'n3-mode-hook
          'turn-on-font-lock)

(setq auto-mode-alist
      (append
       (list
        '("\\.n3" . n3-mode)
        '("\\.owl" . n3-mode))
       auto-mode-alist))

;;
;; pdf-tools
;;

;; enable hidpi
(setq pdf-view-use-scaling 1)
(setq pdf-view-resize-factor 1.05)
(pdf-loader-install)

;;
;; projectile
;;

(projectile-global-mode)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
