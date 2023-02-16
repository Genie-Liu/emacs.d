;;; init-local.el --- Configure of personal settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; pretty format for elisp-mode
;; (require-package 'elisp-format)

;; theme settings
(setq custom-enabled-themes '(sanityinc-tomorrow-bright))
(reapply-themes)

;; set org agenda files

(setq org-return-follows-link t
      org-deadline-warning-days 30)

(when (file-exists-p "~/org/")
  ;; (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-agenda-files '("~/org/inbox.org"
                           "~/org/repeats.org"
                           "~/org/projects.org"
                           "~/org/work.org"
                           "~/org/improve.org")))

(setq org-capture-templates
      `(("t" "todo" entry (file "~/org/inbox.org")  ; "" => `org-default-notes-file'
         "* TODO %?\n%U\n" :clock-resume t)
        ("n" "note" entry (file "")
         "* %? :NOTE:\n%U\n%a\n" :clock-resume t)
        ))

;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file "~/org/inbox.org") "* TODO %?\n %i\n %a")))

(setq org-agenda-custom-commands
      '(
        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-span 'week)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled work tasks")
                 (org-agenda-files '("~/org/work.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled tasks")
                 (org-agenda-files '("~/org/inbox.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled projects tasks")
                 (org-agenda-files '("~/org/projects.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
        ;; default setting of agenda
        ("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))))

;; (maybe-require-package 'json-reformat)
;; (setq json-reformat:indent-width 4)
(setq json-encoding-default-indentation "    ")

(provide 'init-local)
;;; init-locales.el ends here
