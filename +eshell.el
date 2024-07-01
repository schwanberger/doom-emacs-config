;;; +eshell.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Thomas Schwanberger
;;
;; Author: Thomas Schwanberger <thomas@schwanberger.dk>
;; Maintainer: Thomas Schwanberger <thomas@schwanberger.dk>
;; Created: June 18, 2024
;; Modified: June 18, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/schwanberger/+eshell
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

; From:
; https://gist.githubusercontent.com/alexispurslane/93c35dcfc910088016e0603aec9b24e0/raw/0475e947860d4d001934f8af2608e80197242765/eshell-layer.el
; Customized a bit..
;;     "Make eshell feel like a hyper-modern, advanced, featureful
;;  shell, like Fish or Nushell. Also make eshell output read-only
;;  to avoid confusion.
;;
;; Loads:
;; - `eshell-prompt-extras': three beautiful, useful modern prompt
;;   themes, and better tools for building your own.
;; - `esh-autosuggest': automatically suggest (in grayed out text)
;;   commands from your history based on your current command, which
;;   you can accept with a right arrow, like Fish.
;; - `fish-completion': use Fish as your pcomplete backend, to give
;;   LSP-like completions for your terminal (since corfu is enabled
;;   in terminals by default in Quake Emacs already).
;; - `esh-help': documentation popups for eshell and shell commands
;;   for a more IDE-like experience.
;; - `eshell-syntax-highlighting': Fish/Nushell-like syntax
;;   highlighting for eshell, including recognizing whether a
;;   command is found or not.
;; - `eat': a pure-elisp, no-external-dependencies fast,
;;   full-featured vterm-class terminal emulator for Emacs that
;;   embeds into Eshell to make Eshell able to deal with commands
;;   that aren't dumb-terminal-compatible. (This removes the main
;;   eshell pain-point)."
;;;; Configure base eshell
;; (after! em-alias
;;   (eshell/alias "vim" "find-file $1")
;;   (eshell/alias "less" "view-file $1")
;;   )
(after! eshell
  ;; Make eshell/cat capable of cat'ing images and buffers as well as files.
  (advice-add 'eshell/cat
              :around (lambda (oldfun &rest args)
                        (mapconcat (lambda (arg)
                                     (cond
                                      ((bufferp arg) (concat
                                                      (with-current-buffer arg (buffer-string))
                                                      "\n"))
                                      ((string-match-p (image-file-name-regexp) arg)
                                       (concat
                                        (propertize "image" 'display (create-image arg))
                                        "\n"))
                                      (t (apply oldfun args))))
                                   args)))
  (setq eshell-banner-message
        (concat
         (propertize "
                            λλλ
                         λλλλλλλλ
                        λλλλλλλλλλλ
                       λλλ     λλλλ
                      λλ        λλλλ
                      λ          λλλλ
                      λ           λλλ
                                   λλλ
                                   λλλ
                                    λλ
                                    λλλ
                                   λλλλ
                                  λλλλλλ
                                 λλλλλλλ
                                λλλλλλλλλ
                               λλλλλλλλλλ
                              λλλλλλ    λλ
                             λλλλλλ     λλλ
                            λλλλλλ       λλ
                           λλλλλλ        λλλ
                          λλλλλλ         λλλ
                         λλλλλλ           λλλ
                        λλλλλλ            λλλ
                       λλλλλλ              λλλ          λ
                      λλλλλλ               λλλλ        λλ
                     λλλλλλ                 λλλλλ     λλλ
                    λλλλλλ                   λλλλλλλλλλλ
                   λλλλλλλ                    λλλλλλλλλ
                                                 λλλ"
                     'face `(:foreground "#FEDD00"))
         "\n\nWelcome to the Emacs shell\n\n"))
                                        ;:config
;;;;; shell mode configuration
;;   (defun eshell-interactive-output-readonly ()
;;     "Make output of interactive commands in eshell read-only.
;; This should be the last entry in eshell-output-filter-functions!"
;;     (let ((end eshell-last-output-end))
;;       (save-excursion
;;         (goto-char end)
;;         (end-of-line 0)
;;         (setq end (point)))
;;       (when (< eshell-last-output-block-begin end)
;;         (put-text-property eshell-last-output-block-begin end 'read-only "Read-only interactive eshell output"))))

;;   (add-hook 'eshell-mode-hook (lambda ()
;;                                 (add-hook 'eshell-output-filter-functions 'eshell-interactive-output-readonly 'append)))

;;;;; extra commands
  (defun eshell/clear ()
    "Clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

;;;; extra packages
(use-package! eshell-prompt-extras
  :after (eshell)
  :config
  (setq eshell-highlight-prompt t
        eshell-prompt-regexp "^[^λ]+ λ "
        eshell-prompt-function 'epe-theme-dakrone))

(use-package! esh-autosuggest
  :after (eshell)
  :config
  (add-hook! eshell-mode esh-autosuggest-mode)
  )

(use-package! fish-completion
  :after (eshell)
  :config
  (when (executable-find "fish")
    (global-fish-completion-mode)))

(use-package! esh-help
  :after (eshell fish-completion)
  :config
  (setup-esh-help-eldoc))

(use-package! eshell-syntax-highlighting
  :after (eshell)
  :custom
  (eshell-syntax-highlighting-highlight-in-remote-dirs nil)
  :config
  (eshell-syntax-highlighting-global-mode))

(use-package! eat
  :defer t
  :config
  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode))

(add-hook! (eshell-mode) (persp-add-buffer (current-buffer))) ;Make eshell buffers real buffers

(after! eshell
  ; eshell lisp functions are cross platform.
  (setq eshell-prefer-lisp-functions t))

(after! eshell
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (remove-hook! '(eshell-mode-hook) #'hide-mode-line-mode)
  ;; (map! :map eshell-command-mode-map
  ;;       "C-s"   #'consult-history
  ;;       "C-c C-p" #'+thsc/paste-from-minibuffer
  ;;       (:localleader
  ;;        "b" #'eshell-insert-buffer-name
  ;;        "e" #'eshell-insert-envvar
  ;;        "s" #'consult-history))
  )

(after! esh-mode
  (map! :map eshell-mode-map
        :ni "C-s"     #'consult-history
        :ni "C-c C-p" #'+thsc/paste-from-minibuffer
        (:localleader
         "b" #'eshell-insert-buffer-name
         "e" #'eshell-insert-envvar
         "p" #'+thsc/paste-from-minibuffer
         "s" #'consult-history)))

(defun +thsc/eshell ()
  "Setup eshell buffer and open it in default directory."
  (interactive)
  (let* ((remote-hostname (file-remote-p default-directory 'host))
         (eshell-buffer-hostname (if remote-hostname remote-hostname (system-name)))
         (eshell-buffer-name-local (concat "eshell_" eshell-buffer-hostname "__" (sha1 (format "%s" (current-time))))))
    (with-temp-buffer
      (setq-local eshell-buffer-name eshell-buffer-name-local)
      (eshell)
      (auto-save-mode)
      ;; (do-auto-save)
      )))

(defun +thsc/eshell-remote-open (&optional arg)
  "Prompt for a remote host to connect to (ARG), and open a shell there.
With prefix argument, get a sudo shell."
  (interactive "p")
  (require 'tramp)
  (let*
      ((hosts
        (cl-reduce 'append
                   (mapcar
                    (lambda (x)
                      (cl-remove nil (mapcar 'cadr (apply (car x) (cdr x)))))
                    (tramp-get-completion-function "ssh"))))
       (remote-host (completing-read "Remote host: " hosts))
       (eshell-buffer-name-local (concat "eshell_" remote-host "___" (sha1 (format "%s" (current-time)))))
       )
    (with-temp-buffer
      (cd (concat "/" (or tramp-default-method "ssh") ":" remote-host ":"))
      (setq-local eshell-buffer-name eshell-buffer-name-local)
      (eshell remote-host)
      (auto-save-mode))))

(provide '+eshell)
;;; +eshell.el ends here
