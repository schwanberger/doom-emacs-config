;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Thomas Schwanberger"
      user-mail-address "thomas@schwanberger.dk")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!
;;(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font Mono" :size 18 :weight 'semi-light))
;;(setq doom-font (font-spec :family "Iosevka Nerd Font" :size 13.0 :weight 'light))
;;(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 14 :weight 'semi-light))
;; (setq doom-font (font-spec :family "VictorMono Nerd Font" :size 14.0 :weight 'semibold))
;;(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 14.0))
;;(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 19 :weight 'light))
;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 11.0 :weight 'regular))
;;(setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 18 :weight 'regular))
;;(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font" :size 15 :weight 'semi-light))
;; (setq doom-font (font-spec :family "JetBrainsMonoNL NF" :size 11.0 :weight 'semi-light))
;; (setq doom-font (font-spec :family "VictorMono Nerd Font" :size 15 :weight 'semi-bold))
;;(setq doom-font (font-spec :family "MesloLGMDZ Nerd Font Mono" :size 11.0 :weight 'regular))
;;(setq doom-font (font-spec :family "RobotoMono Nerd Font Mono" :size 11.0 :weight 'regular))
;;(setq doom-font (font-spec :family "FiraMono Nerd Font Mono" :size 11.0 :weight 'regular))
;; (setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 11.0 :weight 'light))
 ;(setq doom-font (font-spec :family "JetBrainsMonoNL Nerd Font Mono" :size 11.0))

;(setq doom-font (font-spec :family "Iosevka Comfy Motion Fixed" :size 14.0))
;(setq doom-font (font-spec :family "Iosevka Comfy Fixed" :size 14.0))

;(setq doom-font (font-spec :family "Iosevka Comfy Motion Fixed"))
;(setq doom-font (font-spec :family "Iosevka Comfy Fixed"))

(set-face-attribute 'default nil :height 135)

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;(setq doom-theme 'catppuccin
(setq doom-theme 'doom-vibrant
;(setq doom-theme 'ef-theme-melissa-dark
      doom-themes-treemacs-theme 'doom-colors)

;; (use-package! doom-themes
;;   :config
;;   (ef-themes-select 'ef-melissa-light))

;; (after! doom-themes
;;   (ef-themes-select 'ef-melissa-light))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'visual)
(setq display-line-numbers-width-start t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; (require 'lsp-mode) ; 2024-03-25: Weird bug with lsp-mode, gotta be activated before entering golang buffer

(setq-default buffer-file-coding-system 'utf-8-unix)

(setq read-process-output-max (* 1024 1024)) ;; 1mb - default is too low

(load! "+org")
(load! "+tramp")
(load! "+shell")
(load! "+autosave")
(load! "+defaults")
(load! "+lsp")
(load! "+eshell")
; (load! "+eshellt")
; (user/eshell-layer)
(load! "+denote")

; Comment
