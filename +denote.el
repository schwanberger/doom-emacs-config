;;; +denote.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Thomas Schwanberger
;;
;; Author: Thomas Schwanberger <thomas@schwanberger.dk>
;; Maintainer: Thomas Schwanberger <thomas@schwanberger.dk>
;; Created: September 07, 2024
;; Modified: September 07, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/schwanberger/+denote
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! denote
  :config (setq denote-known-keywords '()
                denote-infer-keywords t
                denote-prompts '(title keywords)
                denote-directory (expand-file-name "~/org/denote")
                denote-backlinks-show-context t)
           (require 'denote-journal-extras)
  )

(use-package! consult-notes
  :after (denote)
  :config
  (consult-notes-denote-mode 1)
  ;; search only for text files in denote dir
  (setq consult-notes-denote-files-function (function denote-directory-text-only-files))
  )

(after! denote
  (with-eval-after-load 'org-capture
    (setq denote-org-capture-specifiers "%l\n%i\n%?")
    (add-to-list 'org-capture-templates
                 '("n" "New note (with denote.el)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  )


(provide '+denote)
;;; +denote.el ends here
