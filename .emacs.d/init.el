;; Packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Load and activate emacs packages
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ace-jump-mode
    ace-window
    ack-menu
    ag
    aggressive-indent
    auto-highlight-symbol
    base16-theme
    buffer-move
    cider
    clj-refactor
    clojure-mode
    clojure-mode-extra-font-locking
    desktop-environment
    eterm-256color
    exwm
    google-this
    highlight-numbers
    hydra
    ido-ubiquitous
    magit
    multiple-cursors
    neotree
    nix-mode
    nlinum
    nov
    paredit
    projectile
    ranger
    smex
    tagedit
    undo-tree
    web-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Place downloaded elisp files in ~/.emacs.d/vendor
(add-to-list 'load-path "~/.emacs.d/vendor")
(require 'fill-column-indicator)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; Window manager
(load "wm.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")
(load "setup-css.el")
(load "setup-java.el")

;; nov.el
(load "setup-nov.el")
