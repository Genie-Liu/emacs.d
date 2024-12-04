;;; init-local.el --- Configure of personal settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; setting for python enviroment

;; (require-package 'conda)
;; if you want interactive shell support, include:
;; (conda-env-initialize-interactive-shells)
;; if you want eshell support, include:
;; (conda-env-initialize-eshell)
;; if you want auto-activation (see below for details), include:
;; (conda-env-autoactivate-mode t)
;; if you want to automatically activate a conda environment on the opening of a file:
;; (add-to-hook 'find-file-hook (lambda () (when (bound-and-true-p conda-project-env-path)
;;                                           (conda-env-activate-for-buffer))))

;; (setq conda-env-home-directory (expand-file-name "~/miniconda3/"))


(require-package 'elisp-format)


;; pretty format for elisp-mode
;; (require-package 'elisp-format)

;; theme settings

;; (require-package 'molokai-theme)
;; (load-theme 'molokai)

;; (setq custom-enabled-themes '(sanityinc-tomorrow-day))
;; (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
;; (reapply-themes)

;; set org agenda files

(setq org-return-follows-link t org-deadline-warning-days 30)

(when (file-exists-p "~/OneDrive/documents/sync_doc/org/")
  ;; (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
  (setq org-agenda-files '("~/OneDrive/documents/sync_doc/org/inbox.org"
                           "~/OneDrive/documents/sync_doc/org/repeats.org"
                           "~/OneDrive/documents/sync_doc/org/projects.org"
                           "~/OneDrive/documents/sync_doc/org/work.org"
                           "~/OneDrive/documents/sync_doc/org/improve.org"
                           "~/OneDrive/documents/sync_doc/org/personal.org")))

(setq org-capture-templates `(("t" "todo" entry (file "~/OneDrive/documents/sync_doc/org/inbox.org")
                                        ; "" => `org-default-notes-file'
                               "* TODO %?\n%U\n"
                               :clock-resume t)
                              ("n" "note" entry (file "") "* %? :NOTE:\n%U\n%a\n"
                               :clock-resume t)))

;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file "~/org/inbox.org") "* TODO %?\n %i\n %a")))

(setq org-agenda-custom-commands
      '(
        (" " "Agenda"
         ((agenda ""
                  ((org-agenda-span 'week)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled personal tasks")
                 (org-agenda-files '("~/OneDrive/documents/sync_doc/org/personal.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled work tasks")
                 (org-agenda-files '("~/OneDrive/documents/sync_doc/org/work.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled tasks")
                 (org-agenda-files '("~/OneDrive/documents/sync_doc/org/inbox.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled projects tasks")
                 (org-agenda-files '("~/OneDrive/documents/sync_doc/org/projects.org"))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
        ;; default setting of agenda
        ("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))))

;; (maybe-require-package 'json-reformat)
;; (setq json-reformat:indent-width 4)
(setq json-encoding-default-indentation "    ")

;; set pandoc path
(custom-set-variables
 '(markdown-command "/opt/homebrew/bin/pandoc"))

;; setting for eglot
;; (add-hook 'c-mode-hook 'eglot-ensure)
;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; (add-hook 'python-mode-hook 'eglot-ensure)



(defun my-get-file-name ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name)))
        (x-select-enable-clipboard t))
    (kill-new filename)))

(defun my-get-name ()
  "Put the current name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-name)))
        (x-select-enable-clipboard t))
    (kill-new filename)))


;; scala setting
(require-package 'scala-mode)

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; (face-attribute 'default :font)
;; (message "%s" (font-family-list))
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono for Powerline-16")
;; (set-face-attribute 'default nil :font "Source Code Pro-16")
(set-face-attribute 'default nil :font "Menlo-14")
;; (set-face-attribute 'default nil :font "Monaco-18")

(provide 'init-local)
;;; init-locales.el ends here
