;;; acme-theme.el --- Theme

;; Version: 0.1
;; Package-Requires: ((emacs "24"))

;;; Code:

(deftheme acme)

(let* ((class '((class color) (min-colors 89)))
       (black          "#424242")
       (blue           "#2a8dc5")
       (cyan           "#b0eced")
       (green          "#57864e")
       (magenta        "#7572c0")
       (red            "#b85c57")
       (yellow         "#8f7634")
       (white          "#b7b19c")
       (bright-black   "#999957")
       (bright-blue    "#a6dcf8")
       (bright-cyan    "#e2ffff")
       (bright-green   "#98ce8f")
       (bright-magenta "#d0d0f7")
       (bright-red     "#f2acaa")
       (bright-yellow  "#e8ef82")
       (bright-white   "#eefeff")
       ;; ---
       (fg1 black)
       (fg2 white)
       (fg3 white)
       (fg4 white)
       (bg1 "#feffe2")
       (bg2 bright-cyan)
       (bg3 "#dee0c7")
       (bg4 bright-yellow)
       (bg5 "#ffd787")
       (builtin yellow)
       (keyword magenta)
       (const   magenta)
       (comment red)
       (func    blue)
       (str     green)
       (type    blue)
       (var     blue)
       (warning bright-red))

  (custom-theme-set-faces
   'acme
   `(default ((,class (:background ,bg1 :foreground ,fg1))))
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func ))))
   `(font-lock-keyword-face ((,class (,class :foreground ,keyword))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(region ((,class (:background ,bg4 :foreground ,fg1))))
   `(highlight ((,class (:foreground ,fg3 :background ,bg3))))
   `(highlight-numbers-number ((,class (:foreground ,builtin))))
   `(hl-line ((,class (:background ,bright-white))))
   `(fringe ((,class (:background ,bg3 :foreground ,fg4))))
   `(cursor ((,class (:background ,magenta))))
   `(show-paren-match ((,class (:foreground ,fg1 :background ,bg5))))
   `(isearch ((,class (:foreground ,bg5 :background ,fg1))))
   `(mode-line ((,class (:box (:line-width 1 :color nil) :foreground ,fg1 :background ,bg2))))
   `(mode-line-inactive ((,class (:box (:line-width 1 :color nil) :foreground ,var :background ,bg1 :weight normal))))
   `(mode-line-buffer-id ((,class (:foreground ,func :background nil))))
   `(mode-line-highlight ((,class (:foreground ,keyword :box nil))))
   `(mode-line-emphasis ((,class (:foreground ,fg1))))
   `(vertical-border ((,class (:foreground ,fg3))))
   `(minibuffer-prompt ((,class (:foreground ,keyword))))
   `(default-italic ((,class (:italic t))))
   `(link ((,class (:foreground ,const :underline t))))
   `(linum ((,class (:foreground ,fg2 :background ,bg1))))
   `(eshell-prompt ((,class (:foreground ,red))))
   `(ahs-face ((,class (:background ,bg3 :foreground nil))))
   `(ahs-definition-face ((,class (:background ,bright-yellow :foreground nil))))
   `(ahs-plugin-defalt-face ((,class (:background ,bg5 :foreground nil))))
   `(org-code ((,class (:foreground ,fg2))))
   `(org-hide ((,class (:foreground ,fg4))))
   `(org-level-1 ((,class (:foreground ,fg2 :height 1.1))))
   `(org-level-2 ((,class (:foreground ,fg3))))
   `(org-level-3 ((,class (:foreground ,fg4))))
   `(org-level-4 ((,class (:foreground ,bg4))))
   `(org-date ((,class (:underline t :foreground ,var) )))
   `(org-footnote  ((,class (:underline t :foreground ,fg4))))
   `(org-link ((,class (:underline t :foreground ,type ))))
   `(org-special-keyword ((,class (:foreground ,func))))
   `(org-block ((,class (:foreground ,fg3))))
   `(org-quote ((,class (:inherit org-block :slant italic))))
   `(org-verse ((,class (:inherit org-block :slant italic))))
   `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword))))
   `(org-done ((,class (:box (:line-width 1 :color ,bg3) :foreground ,bg4))))
   `(org-warning ((,class (:underline t :foreground ,warning))))
   `(org-agenda-structure ((,class (:foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
   `(org-agenda-date ((,class (:foreground ,var :height 1.1 ))))
   `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
   `(org-agenda-date-today ((,class (:foreground ,keyword :height 1.4))))
   `(org-agenda-done ((,class (:foreground ,bg4))))
   `(org-scheduled ((,class (:foreground ,type))))
   `(org-scheduled-today ((,class (:foreground ,func :height 1.2))))
   `(org-ellipsis ((,class (:foreground ,builtin))))
   `(org-verbatim ((,class (:foreground ,fg4))))
   `(org-document-info-keyword ((,class (:foreground ,func))))
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,var :italic t))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(ido-only-match ((,class (:foreground ,warning))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(ido-first-match ((,class (:foreground ,red))))
   `(ffap ((,class (:foreground ,fg4))))
   `(warning ((,class (:foreground ,warning)))) 
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(icompletep-determined ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(magit-branch ((,class (:foreground ,const))))
   `(magit-diff-context-highlight ((,class (:background ,bright-white))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,cyan))))
   `(magit-diff-hunk-heading ((,class (:background ,bg3))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,cyan))))
   `(magit-diffstat-added ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-hunk-heading ((,class (:background ,cyan))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-process-ng ((,class (:foreground ,warning))))
   `(magit-process-ok ((,class (:foreground ,func))))
   `(magit-section-heading ((,class (:foreground ,yellow))))
   `(magit-section-highlight ((,class (:background ,bg3))))
   `(lazy-highlight ((,class (:background ,bright-yellow))))
   `(term ((,class (:foreground ,fg1 :background ,bg1))))
   `(term-color-black ((,class (:foreground ,black :background ,black))))
   `(term-color-blue ((,class (:foreground ,blue :background ,blue))))
   `(term-color-red ((,class (:foreground ,red :background ,red))))
   `(term-color-green ((,class (:foreground ,green :background ,green))))
   `(term-color-yellow ((,class (:foreground ,yellow :background ,yellow))))
   `(term-color-magenta ((,class (:foreground ,magenta :background ,magenta))))
   `(term-color-cyan ((,class (:foreground ,cyan :background ,cyan))))
   `(term-color-white ((,class (:foreground ,white :background ,white))))
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,func))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
   `(jde-java-font-lock-package-face ((t (:foreground ,var))))
   `(jde-java-font-lock-public-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-private-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-constant-face ((t (:foreground ,const))))
   `(jde-java-font-lock-modifier-face ((t (:foreground ,fg2))))
   `(jde-jave-font-lock-protected-face ((t (:foreground ,keyword))))
   `(jde-java-font-lock-number-face ((t (:foreground ,var))))
   `(clojure-keyword-face ((,class (:foreground ,keyword))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'acme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; acme-theme.el ends here