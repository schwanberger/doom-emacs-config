;;; +defaults.el -*- lexical-binding: t; -*-

;; spell-check will be activated when needed, not before
;;(remove-hook! (org-mode markdown-mode TeX-mode rst-mode mu4e-compose-mode message-mode) #'flyspell-mode)
(remove-hook 'text-mode-hook #'spell-fu-mode) ; Only spell check when I want it

(after! corfu
  (setq corfu-auto nil ; You shell help when prompted (by TAB)
        ;; +corfu-want-minibuffer-completion nil ; terminal minibuffer corfu completion leads to bad UX
        ))

                                        ; (fset 'epg-wait-for-status 'ignore) ; Disabled now that I'm not emacs 29.3 or newer version of gpg; Help emacs 29.1 with gpg save timeout with newer gnupg versions
(setq auth-sources '("~/.emacs_authinfo.gpg"))

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      ;;select-enable-clipboard t                  ; OS clipboard integration
      select-enable-clipboard nil                  ; evil-mode shall not trash my clipboard
      password-cache-expiry nil
      kill-ring-max 50000
      kill-do-not-save-duplicates t
      display-time-24hr-format t
      ;;battery-mode-line-format "[%b%p\%] "
      ;;battery-mode-line-format "[%b] "
      ;;company-idle-delay nil
      )

(display-battery-mode 1)                          ; Show battery, not useful if not laptop
(display-time-mode 1)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; (after! yaml-mode
;;   (def-project-mode! +ansible-yaml-mode
;;     :modes '(yaml-mode)
;;     :add-hooks '(ansible ansible-auto-decrypt-encrypt ansible-doc-mode)
;;     :files (or "roles/" "tasks/main.yml" "tasks/main.yaml" "playbooks/"))
;;   )
;; 
;; (after! evil
;;   (setq evil-vsplit-window-right t
;;         evil-split-window-below t)
;;   (defadvice! prompt-for-buffer (&rest _)
;;     :after '(evil-window-split evil-window-vsplit)
;;     (consult-buffer)))

(after! magit
  (add-hook! 'magit-process-find-password-functions
             'magit-process-password-auth-source)
  )

(global-set-key [remap dabbrev-expand] #'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-list
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-all-abbrevs
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev-from-kill
        try-expand-whole-kill
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(when (getenv "WSLENV")
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark))

  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
      (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
      (insert clipboard)))

  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))

  (after! org
    ;; Make http(s) links in org mode open in Windows on RET
    (dolist (scheme '("ftp" "http" "https" "mailto" "news"))
      (org-link-set-parameters scheme
                               :follow
                               (lambda (url arg)
                                 (browse-url-generic (concat scheme ":" url) arg)))))

  (defun paste-remove-carriage-return (func &rest args)
    (let ((result (apply func args)))
      (if (stringp result)
          (replace-regexp-in-string "\r" "" result)
        result)))
  ;;(advice-add 'yank :around 'paste-remove-carriage-return)
  (advice-add 'current-kill :around 'paste-remove-carriage-return)
  )

(defun +thsc/paste-from-minibuffer()
  "An easy way to paste from Windows to anywhere in emacs e.g.
'Ditto' when 'select-enable-clipboard' is nil"
  (interactive)
  (let ((select-enable-clipboard t)
        ;; (kill-ring nil) ; In case we do not want insertions into the kill-ring
        )
    (kill-new (format "%s" (read-from-minibuffer "Paste: ")))
    (yank)))
(map!
 "C-c C-p" #'+thsc/paste-from-minibuffer)

;; BEGIN --- From https://github.com/elken/doom
(global-subword-mode 1)
;;(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(after! evil
  (setq evil-kill-on-visual-paste nil)
  (setq evil-disable-insert-state-bindings t)
  )

;; (use-package! eshell-p10k
;;   :after eshell
;;   :config
;;   (setq eshell-prefer-lisp-functions t)
;;   (setq eshell-prompt-function #'eshell-p10k-prompt-function
;;         eshell-prompt-regexp eshell-p10k-prompt-string))

(after! eshell
  ; eshell lisp functions are cross platform.
  (setq eshell-prefer-lisp-functions t))

(after! vterm
  (define-key vterm-mode-map (kbd "<C-backspace>") (lambda () (interactive) (vterm-send-key (kbd "C-w")))))

(map! :after evil
      :gnvi "C-s" #'consult-line
      :n "H" #'evil-window-left
      :n "J" #'evil-window-down
      :n "K" #'evil-window-up
      :n "L" #'evil-window-right
      )


(map! :map dired-mode-map
      :n "h" #'dired-up-directory
      :n "l" #'dired-find-alternate-file)

;; (after! marginalia
;;   (setq marginalia-censor-variables nil)
;; 
;;   (defadvice! +marginalia--anotate-local-file-colorful (cand)
;;     "Just a more colourful version of `marginalia--anotate-local-file'."
;;     :override #'marginalia--annotate-local-file
;;     (when-let (attrs (file-attributes (substitute-in-file-name
;;                                        (marginalia--full-candidate cand))
;;                                       'integer))
;;       (marginalia--fields
;;        ((marginalia--file-owner attrs)
;;         :width 12 :face 'marginalia-file-owner)
;;        ((marginalia--file-modes attrs))
;;        ((+marginalia-file-size-colorful (file-attribute-size attrs))
;;         :width 7)
;;        ((+marginalia--time-colorful (file-attribute-modification-time attrs))
;;         :width 12))))
;; 
;;   (defun +marginalia--time-colorful (time)
;;     (let* ((seconds (float-time (time-subtract (current-time) time)))
;;            (color (doom-blend
;;                    (face-attribute 'marginalia-date :foreground nil t)
;;                    (face-attribute 'marginalia-documentation :foreground nil t)
;;                    (/ 1.0 (log (+ 3 (/ (+ 1 seconds) 345600.0)))))))
;;       ;; 1 - log(3 + 1/(days + 1)) % grey
;;       (propertize (marginalia--time time) 'face (list :foreground color))))
;; 
;;   (defun +marginalia-file-size-colorful (size)
;;     (let* ((size-index (/ (log10 (+ 1 size)) 7.0))
;;            (color (if (< size-index 10000000) ; 10m
;;                       (doom-blend 'orange 'green size-index)
;;                     (doom-blend 'red 'orange (- size-index 1)))))
;;       (propertize (file-size-human-readable size) 'face (list :foreground color)))))

;; (use-package! info-colors
;;   :after info
;;   :commands (info-colors-fontify-node)
;;   :hook (Info-selection . info-colors-fontify-node))

(after! doom-modeline (setq doom-modeline-major-mode-icon t
                            doom-modeline-time-analogue-clock nil))

;; (after! doom-modeline
;;   (setq auto-revert-check-vc-info t
;;         doom-modeline-major-mode-icon t
;;         doom-modeline-buffer-file-name-style 'relative-to-project
;;         doom-modeline-github nil
;;         doom-modeline-height 30
;;         doom-modeline-vcs-max-length 60)
;;   (remove-hook 'doom-modeline-mode-hook #'size-indication-mode)
;;   (doom-modeline-def-modeline 'main
;;     '(matches bar modals workspace-name window-number persp-name selection-info buffer-info remote-host debug vcs matches)
;;     '(github mu4e grip gnus check misc-info repl lsp time " ")))

(after! org
  (setq org-use-property-inheritance t)
  (setq org-ellipsis " ▾")
  )

(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROG(i)" "PROJ(p)" "STORY(s)" "WAIT(w@/!)" "|" "DONE(d@/!)" "KILL(k@/!)")
          (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)"))
        ;; The triggers break down to the following rules:

        ;; - Moving a task to =KILLED= adds a =killed= tag
        ;; - Moving a task to =WAIT= adds a =waiting= tag
        ;; - Moving a task to a done state removes =WAIT= and =HOLD= tags
        ;; - Moving a task to =TODO= removes all tags
        ;; - Moving a task to =NEXT= removes all tags
        ;; - Moving a task to =DONE= removes all tags
        org-todo-state-tags-triggers
        '(("KILL" ("killed" . t))
          ("HOLD" ("hold" . t))
          ("WAIT" ("waiting" . t))
          (done ("waiting") ("hold"))
          ("TODO" ("waiting") ("cancelled") ("hold"))
          ("NEXT" ("waiting") ("cancelled") ("hold"))
          ("DONE" ("waiting") ("cancelled") ("hold")))

        ;; This settings allows to fixup the state of a todo item without
        ;; triggering notes or log.
        org-treat-S-cursor-todo-selection-as-state-change nil)
  org-cycle-emulate-tab nil
  org-startup-folded 'content
  org-agenda-start-with-log-mode t)

(use-package! org-tempo
  :after org
  :init
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("els" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))
;; END --- From https://github.com/elken/doom

(after! tramp
                                        ; Oracle sqlplus tab width fix
  (setq tab-width 8))

(after! evil
  (defun +thsc/yank-to-clipboard (beg end &optional region)
    ;; Pass mark first, then point, because the order matters when
    ;; calling `kill-append'.
    (interactive (list (mark) (point) 'region))
    (let ((select-enable-clipboard t))
      (copy-region-as-kill beg end region))
    ;; This use of called-interactively-p is correct because the code it
    ;; controls just gives the user visual feedback.
    (if (called-interactively-p 'interactive)
        (indicate-copied-region)))

  (map! :leader
        (:prefix-map ("y" . "Yank helpers")
         :desc "Yank it to system clipboard, please" "y" #'+thsc/yank-to-clipboard))
  )

(after! spell-fu
 (setq ispell-library-directory "/home/thsc/doomemacs-config/aspell/ispell"
       spell-fu-directory "/home/thsc/doomemacs-config/aspell/ispell"
       ispell-personal-dictionary "/home/thsc/doomemacs-config/aspell/ispell/.pws"
       ))
