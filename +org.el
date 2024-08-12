;;; +org.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Thomas Schwanberger
;;
;; Author: Thomas Schwanberger <thomas@schwanberger.dk>
;; Maintainer: Thomas Schwanberger <thomas@schwanberger.dk>
;; Created: March 22, 2024
;; Modified: March 22, 2024
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/thsc/+org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package! org-gtd
  :after org
  :init
  (setq org-gtd-update-ack "3.0.0")
  :config
  (setq org-gtd-directory "~/org/gtd/"
        org-gtd-default-file-name "actionable"
        org-edna-use-inheritance t)
  (org-edna-mode)
  (map! :leader
        (:prefix ("d" . "org-gtd")
         :desc "Capture"        "c"  #'org-gtd-capture
         :desc "Engage"         "e"  #'org-gtd-engage
         :desc "Process inbox"  "p"  #'org-gtd-process-inbox
         :desc "Show all next"  "n"  #'org-gtd-show-all-next
         :desc "Stuck projects" "s"  #'org-gtd-review-stuck-projects))
  (map! :map org-gtd-clarify-map
        :desc "Organize this item" "C-c c" #'org-gtd-organize)
  (map! :map org-mode-map
        "C-c C-p" #'+thsc/paste-from-minibuffer))

(after! org-agenda
  (setq org-agenda-start-on-weekday nil
        org-agenda-dim-blocked-tasks nil
        org-agenda-start-day "0d"
        ;; Need to see deadline long in advance in some views
        org-deadline-warning-days 365
        org-duration-format (quote h:mm))
        ;; org-agenda-clockreport-parameter-plist '(:link nil :maxlevel 4) ; 2024-03-25: Seems to be unncessary now. ; 2023-02-27: Fix links everywhere, seen on issue on doom emacs
  (add-to-list 'org-agenda-custom-commands '("g" "Scheduled today and all NEXT items" ((agenda "" ((org-agenda-span 1))) (todo "NEXT"))))
  ;; (add-to-list 'org-agenda-custom-commands '("d" "Scheduled today and all NEXT items" (
  ;;                                                                                      (agenda "" ((org-agenda-span 1)
  ;;                                                                                                  (org-deadline-warning-days 0)
  ;;                                                                                                  (org-scheduled-past-days 10000)
  ;;                                                                                                  (TODO "NEXT")))))))
  (add-to-list 'org-agenda-custom-commands '("d" "Items scheduled up to today" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/gtd/actionable.org" "~/org/todo/new_todo.org" "~/org/private/practical.org" "~/org/gtd/inbox.org" "~/org/private/nerd.org"))
                                                                                                   (org-scheduled-past-days 10000)
                                                                                                   (TODO "NEXT"))))))

  (add-to-list 'org-agenda-custom-commands '("D" "Items scheduled today only" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/gtd/actionable.org" "~/org/todo/new_todo.org" "~/org/private/practical.org" "~/org/gtd/inbox.org" "~/org/private/nerd.org"))
                                                                                                   (org-scheduled-past-days 0)
                                                                                                   (TODO "NEXT"))))))

  (add-to-list 'org-agenda-custom-commands '("p" "Items scheduled up to today for personal" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/private/practical.org" "~/org/private/nerd.org"))
                                                                                                   (org-scheduled-past-days 10000)
                                                                                                   (TODO "NEXT"))))))

  (add-to-list 'org-agenda-custom-commands '("P" "Items scheduled up to today for personal" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/private/practical.org" "~/org/private/nerd.org"))
                                                                                                   (org-scheduled-past-days 0)
                                                                                                   (TODO "NEXT"))))))

  (add-to-list 'org-agenda-custom-commands '("w" "Items scheduled up to today for NC work" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/gtd/actionable.org" "~/org/todo/new_todo.org" "~/org/gtd/inbox.org"))
                                                                                                   (org-scheduled-past-days 10000)
                                                                                                   (TODO "NEXT"))))))

  (add-to-list 'org-agenda-custom-commands '("W" "Items scheduled today only for NC work" (
                                                                                       (agenda "" ((org-agenda-span 1)
                                                                                                   (org-deadline-warning-days 0)
                                                                                                   (org-agenda-files '("~/org/gtd/actionable.org" "~/org/todo/new_todo.org" "~/org/gtd/inbox.org"))
                                                                                                   (org-scheduled-past-days 0)
                                                                                                   (TODO "NEXT")))))))

(after! org
  ;(doom-themes-org-config)
  (setq org-tags-exclude-from-inheritance '("crypt" "project")) ;; "project" tag is special for my GTD setup and should not be inherited, i.e. only "top-level"
  (setq org-log-done 'time
        ;; Require braces to make org consider_{this} or^{this} as subscript or superscript
        org-use-sub-superscripts t
        org-export-with-sub-superscripts '{}
        org-agenda-tags-column 'auto
        org-log-into-drawer t)
           ;;; Clocking
  ;; Resume clocking task when emacs is restarted
  (org-clock-persistence-insinuate)
  (setq
   ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
   org-clock-history-length 23
   ;; Resume clocking task on clock-in if the clock is open
   org-clock-in-resume t
   ;; Separate drawers for clocking and logs
   org-drawers (quote ("PROPERTIES" "LOGBOOK"))
   ;; Save clock data and state changes and notes in the LOGBOOK drawer
   org-clock-into-drawer t
   ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
   org-clock-out-remove-zero-time-clocks t
   ;; Cl!ock out when moving task to a done state
   org-clock-out-when-done t
   ;; Save the running clock and all clock history when exiting Emacs, load it on startup
   org-clock-persist t
   ;; Enable auto clock resolution for finding open clocks
   org-clock-auto-clock-resolution 'when-no-clock-is-running
   ;; Include current clocking task in clock reports
   org-clock-report-include-clocking-task t
   org-hide-emphasis-markers t
   )
  (defun +thsc/org-inactive-timestamp-with-time ()
    "Insert an inactive timestamp with date and time."
    (interactive)
    (let ((current-prefix-arg 4)) ;; emulate C-u
      (call-interactively 'org-time-stamp-inactive)))
  (setq org-pandoc-options-for-html5 `(
                                       (number-sections . t)
                                       (toc . t)
                                       (self-contained . t)
                                       (toc-depth . 5)
                                       ;;(template . "C:/Projects/todo/easy_template.html")
                                       (template . ,(expand-file-name "~/org/todo/pandoc/html5/github/GitHub.html5"))
                                       ;;(template . "C:/Projects/todo/pandoc/html5/kjhealy/html.template")
                                       ))
  ;; Org id's
  (require 'org-id)
  (setq org-id-link-to-org-use-id 'create-if-interactive)
  (setq org-id-extra-files '(
                             (org-agenda-text-search-extra-files)
                             (directory-files-recursively (expand-file-name "~/org") ".org$" t)))
  )

(defun set-progress-property-h (n-done n-not-done)
  (org-set-property "PROGRESS" (concat "[" (number-to-string n-done) "/" (number-to-string (+ n-done n-not-done)) "]")))

(add-hook 'org-after-todo-statistics-hook 'set-progress-property-h)

(provide '+org)
;;; +org.el ends here
