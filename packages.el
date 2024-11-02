;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;; (package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;; (package! another-package
;;   :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;; (package! this-package
;;   :recipe (:host github :repo "username/repo"
;;            :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;; (package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;; (package! builtin-package :recipe (:nonrecursive t))
;; (package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;; (package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;; (package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;; (unpin! pinned-package)
;; ...or multiple packages
;; (unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;; (unpin! t)

;(unpin! lsp-mode lsp-ui)

(package! groovy-mode)
(package! puppet-mode)
(package! solaire-mode :disable t)

(package! ef-themes :recipe (:host github :repo "protesilaos/ef-themes"))
(package! modus-themes :recipe (:host github :repo "protesilaos/modus-themes"))

;;(package! catppuccin :recipe (:host github :repo "catppuccin/emacs"))

;; org-gtd
(package! org-gtd)
(package! org-edna)
(package! org-super-agenda)

;;(package! magit :built-in 'prefer)
;;(package! sqlite :built-in 'prefer)
;;(package! emacsql-sqlite :built-in 'prefer)
;;(package! evil :built-in 'prefer)
;;(package! evil-args :built-in 'prefer)
;;(package! evil-easymotion :built-in 'prefer)
;;(package! evil-embrace :built-in 'prefer)
;;(package! evil-escape :built-in 'prefer)
;;(package! evil-exchange :built-in 'prefer)
;;(package! evil-lion :built-in 'prefer)
;;(package! evil-numbers :built-in 'prefer)
;;(package! evil-snipe :built-in 'prefer)
;;(package! evil-surround :built-in 'prefer)
;;(package! evil-traces :built-in 'prefer)
;;(package! evil-visualstar :built-in 'prefer)
;;(package! evil-collection :built-in 'prefer)
;;(package! evil-anzu :built-in 'prefer)
;;(package! treemacs :built-in 'prefer)
;;(package! lsp-treemacs :built-in 'prefer)
;;(package! doom-modeline :built-in 'prefer)
;;(package! posframe :built-in 'prefer)
;;(package! lsp-mode :built-in 'prefer)
;;(package! lsp-ui :built-in 'prefer)
;;;;(package! dap-mode :built-in 'prefer)
;;(package! auctex :built-in 'prefer)
;;(package! auctex-latexmk :built-in 'prefer)
;;(package! writeroom-mode :built-in 'prefer)
;;;;(package! nerd-icons-completion :built-in 'prefer)
;;(package! compat :built-in 'prefer)
;;(package! seq :built-in 'prefer)
;;(package! org-gtd :built-in 'prefer)
;;(package! org-edna :built-in 'prefer)
;;(package! org-super-agenda :built-in 'prefer)
;;(package! yaml-mode :built-in 'prefer)
;;;;(package! ts-fold :built-in 'prefer)
;;(package! org :built-in 'prefer)
;;(package! ox-asciidoc :built-in 'prefer)

;;(package! treesit-grammars :built-in 'prefer)
;; (package! tree-sitter-langs :built-in t)
;; (package! tree-sitter :built-in t)
;; (package! tree-sitter-indent :built-in t)
(package! treesit-auto :built-in 'prefer) ; Grammars installed by nix - for non-nix treesit can handle it for us
;(package! tree-sitter-langs :built-in t) ; Let nix handle this due to binary deps on grammars

;; (package! eglot :built-in t) ; Get latest via nix (or fallback to emacs 29.x version)
(package! vterm :built-in t) ; Get latest via nix (or fallback to emacs 29.x version)

; 2024-06-18 Custom eshell module (+eshell.el):
;; (package! eshell :built-in 'prefer)
;; (package! eshell-prompt-extras :built-in 'prefer)
;; (package! fish-completion :built-in 'prefer)
;; (package! bash-completion)
;; (package! esh-help :built-in 'prefer)
;; (package! eshell-syntax-highlighting :built-in 'prefer)
;; (package! eat :built-in 'prefer)
;; (package! pinentry :built-in 'prefer)
;; (package! capf-autosuggest)
(package! eat :built-in t)
(package! esh-autosuggest :built-in t)

(package! denote :built-in t)
(package! consult-notes :built-in t)

(package! hyperbole :built-in t)
(package! standard-themes :built-in t)
(package! evil :built-in t)

(package! 2048-game)




; git-commit vs magit pinning causing issues
; (package! git-commit :pin "55656a31cc4fe6c8996c621f4cf14aa4a1bfe47d")
