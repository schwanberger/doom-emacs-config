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

(after! eshell
  (add-to-list 'eshell-modules-list 'eshell-elecslash)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
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
        :ni "C-a"     #'eshell-bol
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

(defun +thsc/ensure-ssh-key ()
  "Add current SSH key defined in env var SSH_KEY to ssh-agent."
  (let* ((command (format "ssh-add - <%s" (getenv "SSH_KEY")))
         (exit-code (shell-command command)))
    (when (/= exit-code 0)
      (signal 'error (list (format "Failed to add SSH key at to agent with command: %s" command))))))

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
    (+thsc/ensure-ssh-key)
    (with-temp-buffer
      (cd (concat "/" (or tramp-default-method "ssh") ":" remote-host ":"))
      (setq-local eshell-buffer-name eshell-buffer-name-local)
      (eshell remote-host)
      (auto-save-mode))))

(use-package! esh-autosuggest
  :hook ((eshell-mode . esh-autosuggest-mode)))

(after! eshell
  (setq eshell-syntax-highlighting-highlight-in-remote-dirs nil))

(use-package! eat
  :hook ((eshell-load . eat-eshell-mode)))

(use-package! fish-completion
  :after (eshell)
  :config
  (setq fish-completion-prefer-bash-completion t)
  (add-hook 'eshell-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions
                      'bash-completion-capf-nonexclusive nil t))))

(provide '+eshell)
;;; +eshell.el ends here
