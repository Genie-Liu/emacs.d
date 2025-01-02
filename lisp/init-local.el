;;; init-local.el --- Configure of personal settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Many configure are borrow from
;; Pros's blog https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration


;; Install use-package if not installed yet.
;; I prefer using use-package since it make the configuration more compact

(when (< emacs-major-version 29)
  (unless (package-installed-p 'use-package)
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)

;; Enable defer and ensure by default for use-package
(setq use-package-always-defer t
      use-package-always-ensure t)

;; select the region, when typing then delete them

(use-package delsel
  :ensure nil                ; no need to install it as it is built-in
  :hook
  (after-init . delete-selection-mode))

;; theme setting
;; Use Protesilaos Stavrou’s lovely modus-operandi https://gitlab.com/protesilaos/modus-themes
;;; For the built-in themes which cannot use `require'.

(use-package modus-themes
  :ensure t
  :pin gnu
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;; Font setting
;; Before adding the font, need to make sure the font is installed.
;; mono spaced font
;; - DejaVu Sans Mono for Powerline
;; - Fira Mono for Powerline
;; - Menlo
;; - Iosevka Comfy Motion Fixed
;; - Monaspace Xeon/Argon

(let ((mono-spaced-font "Iosevka Comfy Motion Fixed")
      (proportionately-spaced-font "Helvetica"))
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil :family mono-spaced-font :height 1.0)
  (set-face-attribute 'variable-pitch nil :family proportionately-spaced-font :height 1.0))

;; better C-g
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Use icon fonts
(use-package nerd-icons
  :ensure t)

(use-package nerd-icons-completion
  :ensure t
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; denote setting

(use-package denote
  :ensure t
  :bind
  ("C-c n n" . #'denote)
  ("C-c n s" . #'denote-subdirectory)
  ("C-c n i" . #'denote-link)
  ("C-c n b" . #'denote-backlinks)
  ("C-c n r" . #'denote-rename-file)
  :config
  (setq denote-directory (expand-file-name "~/OneDrive/documents/sync_doc/notes/"))
  (setq denote-known-keywords '("emacs" "life" "work" "learn"))
  (setq denote-file-type 'markdown-yaml))

(use-package consult-denote
  :ensure t
  :bind
  ("C-c n f" . #'consult-denote-find)
  ("C-c n g" . #'consult-denote-grep)
  :config
  (consult-denote-mode 1))

;; setting for pulsar: https://protesilaos.com/emacs/pulsar#h:96289426-8480-4ea6-9053-280348adc0ed
(use-package pulsar
  :ensure t
  :config
  (setq pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  (setq pulsar-region-face 'pulsar-green) ; unchanged regions are green
  (setq pulsar-region-change-face 'pulsar-red) ; changed regions are red

  (add-to-list 'pulsar-pulse-functions 'switch-window)
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)

  (pulsar-global-mode 1)
  :bind
  (:map global-map
        ("C-x l" . #'pulsar-pulse-line)
        ("C-x L" . #'pulsar-highlight-line))
  )

;; setting for spacious-padding https://protesilaos.com/emacs/spacious-padding
(use-package spacious-padding
  :ensure t
  :defer 1
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8
           :fringe-width 8))
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))
  (spacious-padding-mode 1)
  :bind
  (:map global-map
        ("<f8>" . #'spacious-padding-mode)))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  ;; :config
  ;; (setq dired-subtree-use-backgrounds nil)
  )

(use-package trashed
  :ensure t
  :commands (trashed)
  :config
  (setq trashed-action-confirmer 'y-or-n-p)
  (setq trashed-use-header-line t)
  (setq trashed-sort-key '("Date deleted" . t))
  (setq trashed-date-format "%Y-%m-%d %H:%M:%S"))


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
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; emacs-rime setting: https://github.com/DogLooksGood/emacs-rime/blob/master/INSTALLATION.org
;; 1. extract pre-compiled librime to rime-librime-root. Ref: https://github.com/rime/librime/releases/
;; 2. add the header-root which contains the <emacs-module.h> header file. Depends on where you install Emacs
;; Some configuration for rime: https://github.com/DogLooksGood/emacs-rime/tree/master?tab=readme-ov-file#%E6%89%93%E5%BC%80-rime-%E7%9A%84%E9%85%8D%E7%BD%AE%E6%96%87%E4%BB%B6
(use-package rime
  :custom
  (default-input-method "rime")
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include")
  )

(provide 'init-local)
;;; init-locales.el ends here
