;;; +autosave.el -*- lexical-binding: t; -*-

;; Backup and autosave revamp
(defvar thsc/backup-location (expand-file-name "~/.emacs-saves")
  "Base directory for backup files.")
(defvar thsc/eshell-dir (expand-file-name "~/.emacs-saves/eshell")
  "Directory for eshell related backup/autosave/history.")
(defvar thsc/backup-dir (expand-file-name "~/.emacs-saves/backup"))
(defvar thsc/autosave-dir (expand-file-name "~/.emacs-saves/autosave"))
(defvar thsc/autosave-dir-nonfile (expand-file-name "~/.emacs-saves/autosave_nonfile"))

(make-directory thsc/backup-location t)
(make-directory thsc/eshell-dir t)
(make-directory thsc/backup-dir t)
(make-directory thsc/autosave-dir t)
(make-directory thsc/autosave-dir-nonfile t)

(defun auto-save-file-name-p (filename) (string-match "^#.*#$" (file-name-nondirectory filename)))
(defun make-auto-save-file-name ()
  (if buffer-file-name (concat thsc/autosave-dir "/" "#" (file-name-nondirectory buffer-file-name) "#")
    ;;(expand-file-name (concat thsc/autosave-dir-nonfile "/" (format-time-string "%y_%m_%d") (format "%s" (shell-command "uuidgen" t)) "#%"
    (expand-file-name (concat thsc/autosave-dir-nonfile "/" (format-time-string "%y_%m_%d") "#%"
                              (replace-regexp-in-string "[*]\\|/" "" (buffer-name)) "_" (sha1 (format "%s" (current-time))) "#"))))
;;                              (replace-regexp-in-string "[*]\\|/" "" (buffer-name)) (sha1 buffer-name) "#"))))


(defadvice! thsc/make-backup-file-name (fn FILE)
  :around #'make-backup-file-name-1
  (let ((dirname (concat thsc/backup-dir
                         (format-time-string "/%y/%m/%d/"))))
    (if (not (file-exists-p dirname))
        (make-directory dirname t))
    (concat dirname (file-name-nondirectory FILE))))

(advice-remove 'make-backup-file-name-1 'doom-make-hashed-backup-file-name-a)
(advice-remove 'make-auto-save-file-name 'doom-make-hashed-auto-save-file-name-a)

(setq
 make-backup-files t
 backup-by-copying t     ; don't clobber symlinks
 kept-new-versions 10    ; keep 10 latest versions
 kept-old-versions 0     ; don't bother with old versions
 delete-old-versions t   ; don't ask about deleting old versions
 version-control t       ; number backups
 vc-make-backup-files t  ; backup version controlled files
 savehist-file (concat thsc/backup-location "/" "savehist")
 history-delete-duplicates t
 history-length t
 eshell-history-file-name (concat thsc/eshell-dir "/" "history")
 backup-directory-alist
 `((".*" . ,thsc/backup-dir))
 auto-save-file-name-transforms
 `((".*" ,thsc/autosave-dir t)))
;;
