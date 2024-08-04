;;; +tramp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Thomas Schwanberger
;;
;; Author: Thomas Schwanberger <thomas@schwanberger.dk>
;; Maintainer: Thomas Schwanberger <thomas@schwanberger.dk>
;; Created: June 18, 2024
;; Modified: June 18, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/schwanberger/+tramp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! tramp
  :defer-incrementally t
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path) ; Add paths of remote at login time to tramp - why is not default?
  (setq tramp-histfile-override "/dev/null") ; stop creating trash history files
  ;; make sure vc stuff is not making tramp slower
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  (setq tramp-default-method "ssh"
        remote-file-name-inhibit-locks 't
        tramp-auto-save-directory (eval thsc/autosave-dir)
        tramp-backup-directory-alist backup-directory-alist
        tramp-use-ssh-controlmaster-options nil
        ;; cache file-name forever
        remote-file-name-inhibit-cache nil
        projectile-mode-line "Projectile"
        tramp-password-prompt-regexp   ; Add verification code support.
        (concat
         "^.*"
         (regexp-opt
          '("passphrase" "Passphrase"
            "password" "Password"
            "Verification code"
            "RADIUS challenge")
          t)
         ".*:\0? *"))
  ;; (add-to-list 'tramp-methods
  ;;              '("isudo"
  ;;                (tramp-login-program "env")
  ;;                (tramp-login-args
  ;;                 (
  ;;                  ("sudo")
  ;;                  ("-u" "%u")
  ;;                  ("--login")))
  ;;                (tramp-remote-shell "/bin/bash")
  ;;                (tramp-remote-shell-login
  ;;                 ("-l"))
  ;;                (tramp-remote-shell-args
  ;;                 ("-c"))
  ;;                (tramp-connection-timeout 10)
  ;;                (tramp-session-timeout 300)
  ;;                (tramp-password-previous-hop t))
  ;;              )
  (add-to-list 'tramp-completion-function-alist
               '("ssh"
                 (tramp-parse-sconfig "~/.config/sops-nix/secrets/ssh_config_work")
                 (tramp-parse-sconfig "~/.ssh/adhoc_config")))
  )

(defun tramp-remote-dired (&optional arg)
  "Prompt for a remote host to connect to, and open an eshell
there."
  (require 'tramp)
  (interactive "p")
  (let*
      ((hosts
        (cl-reduce 'append
                   (mapcar
                    (lambda (x)
                      (cl-remove nil (mapcar 'cadr (apply (car x) (cdr x)))))
                    '((tramp-parse-sconfig "~/.ssh/config")))))
       (remote-host (completing-read "Remote host: " hosts)))
    (with-temp-buffer
      (cd (concat "/" (or tramp-default-method "ssh") ":" remote-host ":/home"))
      (dired default-directory))))

(defun +thsc/remote-dired (&optional arg)
  "Prompt for a remote host to connect to and open dired."
  (interactive "p")
  (require 'tramp)
  (let*
      ((hosts
        (cl-reduce 'append
                   (mapcar
                    (lambda (x)
                      (cl-remove nil (mapcar 'cadr (apply (car x) (cdr x)))))
                    '((tramp-parse-sconfig "~/.ssh/config")))))
       (remote-host (completing-read "Remote host: " hosts)))
    (with-temp-buffer
      (cd (concat "/" (or tramp-default-method "ssh") ":" remote-host ":"))
      (dired default-directory))))

;; Not particularly useful. Using +thsc/oracle-remote-login-shell () instead.
;; (defun +thsc/remote-dired-oracle ()
;;   "Use remote connection to connect as oracle and open dired."
;;   (interactive)
;;   (unless (file-remote-p default-directory) (error "This function only works on remote hosts"))
;;   (let
;;       ((default-directory (concat
;;                            "/ssh:"
;;                            (file-remote-p default-directory 'host)
;;                            "|sudo:oracle@:"
;;                            )))
;;     (dired default-directory)))


(defun +thsc/oracle-remote-login-shell ()
  "Open login shell as user oracle on remote host belonging to `default-directory`."
  (interactive)
  (require 'tramp)
  (unless (file-remote-p default-directory) (error "This function only works on remote hosts"))
  (let
      ((default-directory (concat
                           "/ssh:"
                           (file-remote-p default-directory 'host)
                           "|isudo:oracle@:"
                           )))
    (shell (generate-new-buffer-name (format "shell %s"
                                             (concat
                                              default-directory
                                              "___"
                                              (sha1 (format "%s" (current-time))))
                                             ))))
  (auto-save-mode)
  ;;(comint-simple-send (current-buffer) "export PS1='[\\u@\\h \\W] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  (comint-simple-send (current-buffer) "export PS1='[\\u@\\h:\\w] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  )

(defun +thsc/oracle-remote-shell ()
  "Open login shell as user oracle on remote host belonging to `default-directory`."
  (interactive)
  (require 'tramp)
  (unless (file-remote-p default-directory) (error "This function only works on remote hosts"))
  (let
      ((default-directory (concat
                           "/ssh:"
                           (file-remote-p default-directory 'host)
                           "|sudo:oracle@:"
                           )))
    (shell (generate-new-buffer-name (format "shell %s"
                                             (concat
                                              default-directory
                                              "___"
                                              (sha1 (format "%s" (current-time))))
                                             ))))
  (auto-save-mode)
  ;;(comint-simple-send (current-buffer) "export PS1='[\\u@\\h \\W] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  (comint-simple-send (current-buffer) "export PS1='[\\u@\\h:\\w] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  )

(defun +thsc/grid-remote-login-shell ()
  "Open login shell as user oracle on remote host belonging to `default-directory`."
  (interactive)
  (require 'tramp)
  (unless (file-remote-p default-directory) (error "This function only works on remote hosts"))
  (let
      ((default-directory (concat
                           "/ssh:"
                           (file-remote-p default-directory 'host)
                           "|isudo:grid@:"
                           )))
    (shell (generate-new-buffer-name (format "shell %s"
                                             (concat
                                              default-directory
                                              "___"
                                              (sha1 (format "%s" (current-time))))
                                             ))))
  (auto-save-mode)
  ;;(comint-simple-send (current-buffer) "export PS1='[\\u@\\h \\W] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  (comint-simple-send (current-buffer) "export PS1='[\\u@\\h:\\w] \\D{%F %T}\n(\$ORACLE_SID) $ '")
  )

(provide '+tramp)
;;; +tramp.el ends here
