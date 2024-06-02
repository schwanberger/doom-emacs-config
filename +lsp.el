;;; +lsp.el -*- lexical-binding: t; -*-

(when (or (modulep! :checkers syntax +flymake)
          (not (modulep! :checkers syntax)))
  (setq lsp-diagnostics-provider :flymake))
(after! lsp-mode
  (setq
   lsp-log-io nil
   lsp-auto-guess-root t
   lsp-progress-via-spinner t
   lsp-enable-file-watchers nil
   lsp-idle-delay 0.01
   lsp-completion-enable-additional-text-edit t

   lsp-signature-render-documentation t
   lsp-signature-auto-activate '(:on-trigger-char :on-server-request :after-completion)
   lsp-signature-doc-lines 10

   lsp-eldoc-enable-hover t
   lsp-headerline-breadcrumb-enable t
   lsp-modeline-code-actions-segments '(count icon name)

   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-enable-symbol-highlighting nil
   lsp-enable-links nil

   lsp-lens-enable t))

(after! lsp-ui
  (setq
   ;; Sideline
   lsp-ui-sideline-enable t
   lsp-ui-sideline-show-code-actions t
   lsp-ui-sideline-show-symbol nil
   lsp-ui-sideline-show-hover nil
   lsp-ui-sideline-show-diagnostics nil
   ;; Peek
   lsp-ui-peek-enable t
   ;; Doc
   lsp-ui-doc-enable t
   lsp-ui-doc-position 'top
   lsp-ui-doc-delay 0.51
   lsp-ui-doc-max-width 50
   lsp-ui-doc-max-height 30
   lsp-ui-doc-include-signature t
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-doc-header t))

(use-package! treesit-auto
  ;:defer-incrementally t
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
