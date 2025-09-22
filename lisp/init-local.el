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

(require-package 'use-package)

;; Enable defer and ensure by default for use-package
(setq use-package-always-defer t
      use-package-always-ensure t)

;; select the region, when typing then delete them

(use-package delsel
  :ensure nil                ; no need to install it as it is built-in
  :hook
  (after-init . delete-selection-mode))

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
  (spacious-padding-mode -1)
  :bind
  (:map global-map
        ("<f8>" . #'spacious-padding-mode)))

;; theme setting
;; Use Protesilaos Stavrou’s lovely modus-operandi https://gitlab.com/protesilaos/modus-themes
;; For the built-in themes which cannot use `require'.


(when (maybe-require-package 'modus-themes)
  (require 'modus-themes)
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice:
  (load-theme 'modus-operandi :no-confirm)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

;; Font setting
;; Before adding the font, need to make sure the font is installed.
;; mono spaced font
;; - DejaVu Sans Mono for Powerline
;; - Fira Mono for Powerline
;; - Menlo
;; - Aporetic Project by protesilaos
;; https://github.com/protesilaos/aporetic/tree/main
;; - Monaspace Xenon/Argon/Radon

;; serif proportion: Times New Roman, Georgia
;; sans proportionate: Helvetica

;; Chinese font:
;; serif: SimSong 宋体, STHeiti

(let ((mono-spaced-font "Aporetic Sans Mono")
      (proportionately-spaced-font "Helvetica")
      (mono-spaced-serif-font "Courier New"))
  ;; enable for reading blog
  ;; (set-face-attribute 'default nil
  ;;                     :family proportionately-spaced-font
  ;;                     :height 140)
  ;; enable for coding
  (set-face-attribute 'default nil :family mono-spaced-font :height 140)
  (set-face-attribute 'fixed-pitch nil
                      :family mono-spaced-font
                      :height 1.0)
  (set-face-attribute 'variable-pitch nil
                      :family proportionately-spaced-font
                      :height 1.0))

;; toggle fonts
(defvar my-font-list '("Aporetic Sans Mono-14" "Menlo-14" "Helvetica-14")
  "A list of fonts to toggle through.")

(defvar my-font-index 0
  "Current index in `my-font-list`.")

(defun toggle-font ()
  (interactive)
  (setq my-font-index (% (+ my-font-index 1) (length my-font-list)))
  (set-frame-font (nth my-font-index my-font-list) nil t)
  (message "Current font: %s" (nth my-font-index my-font-list)))

;; set line spacing
(setq-default line-spacing 1)
;; (setq line-spacing 1)

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

;; window movement setting
;; windmove: S-<left>, S-<right>, S-<up>, S-<down>.
(global-set-key (kbd "M-o") 'other-window)
;; (windmove-default-keybindings) ;; has conflicts with S-<cursor> https://www.emacswiki.org/emacs/OrgMode#h5o-8

;; Use icon fonts
(use-package nerd-icons
  :ensure t)

(use-package
  nerd-icons-completion
  :ensure t
  :after marginalia
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

(use-package
  nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

;; denote setting

(use-package
  denote
  :ensure t
  :bind
  ("C-c n n" . #'denote)
  ("C-c n s" . #'denote-subdirectory)
  ("C-c n i" . #'denote-link)
  ("C-c n b" . #'denote-backlinks)
  ("C-c n r" . #'denote-rename-file)
  :config
  (setq denote-directory (expand-file-name "~/Documents/OneDrive/documents/sync_doc/notes/"))
  (setq denote-known-keywords '("emacs" "life" "work" "learn"))
  ;; (setq denote-file-type 'markdown-yaml)
  )

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
        ("C-x L" . #'pulsar-highlight-line)))



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

;; ;; set org agenda files

;; (setq org-return-follows-link t org-deadline-warning-days 30)

;; (when (file-exists-p "~/Documents/OneDrive/documents/sync_doc/org/")
;;   ;; (setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
;;   (setq org-agenda-files '("~/Documents/OneDrive/documents/sync_doc/org/inbox.org"
;;                            "~/Documents/OneDrive/documents/sync_doc/org/repeats.org"
;;                            "~/Documents/OneDrive/documents/sync_doc/org/projects.org"
;;                            "~/Documents/OneDrive/documents/sync_doc/org/work.org"
;;                            "~/Documents/OneDrive/documents/sync_doc/org/improve.org"
;;                            "~/Documents/OneDrive/documents/sync_doc/org/personal.org")))

;; (setq org-capture-templates `(("t" "todo" entry (file "~/Documents/OneDrive/documents/sync_doc/org/inbox.org")
;;                                         ; "" => `org-default-notes-file'
;;                                "* TODO %?\n%U\n"
;;                                :clock-resume t)
;;                               ("n" "note" entry (file "") "* %? :NOTE:\n%U\n%a\n"
;;                                :clock-resume t)))

;; ;; (setq org-capture-templates
;; ;;       '(("t" "Todo" entry (file "~/org/inbox.org") "* TODO %?\n %i\n %a")))

;; (setq org-agenda-custom-commands
;;       '(
;;         (" " "Agenda"
;;          ((agenda ""
;;                   ((org-agenda-span 'week)))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header "Unscheduled personal tasks")
;;                  (org-agenda-files '("~/Documents/OneDrive/documents/sync_doc/org/personal.org"))
;;                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header "Unscheduled work tasks")
;;                  (org-agenda-files '("~/Documents/OneDrive/documents/sync_doc/org/work.org"))
;;                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header "Unscheduled tasks")
;;                  (org-agenda-files '("~/Documents/OneDrive/documents/sync_doc/org/inbox.org"))
;;                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
;;           (todo "TODO"
;;                 ((org-agenda-overriding-header "Unscheduled projects tasks")
;;                  (org-agenda-files '("~/Documents/OneDrive/documents/sync_doc/org/projects.org"))
;;                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
;;         ;; default setting of agenda
;;         ("n" "Agenda and all TODOs"
;;          ((agenda "")
;;           (alltodo "")))))


;;; Org-mode Configuration

;; Define the home directory for org files
(defvar org-home "~/Documents/OneDrive/documents/sync_doc/org/"
  "The root directory for all org files.")

;; Basic org settings
(setq org-return-follows-link t       ; Follow links with RET
      org-deadline-warning-days 30)   ; Show deadline warnings 30 days in advance

;; Set org agenda files if org directory exists
(when (file-exists-p org-home)
  ;; Option 1: Recursively find all org files (commented out)
  ;; (setq org-agenda-files (directory-files-recursively org-home "\\.org$"))

  ;; Option 2: Explicitly list org files
  (setq org-agenda-files (mapcar (lambda (file) (expand-file-name file org-home))
                                 '("inbox.org"
                                   "repeats.org"
                                   "projects.org"
                                   "work.org"
                                   "improve.org"
                                   "personal.org"))))

;; Org capture templates
(setq org-capture-templates
      `(("t" "todo" entry
         (file ,(expand-file-name "inbox.org" org-home))
         "* TODO %?\n%U\n"
         :clock-resume t)
        ("n" "note" entry
         (file "")  ; Uses `org-default-notes-file'
         "* %? :NOTE:\n%U\n%a\n"
         :clock-resume t)))

;; Org agenda custom commands
(setq org-agenda-custom-commands
      '((" " "Agenda"
         ((agenda ""
                  ((org-agenda-span 'week)))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled personal tasks")
                 (org-agenda-files (list (expand-file-name "personal.org" org-home)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled work tasks")
                 (org-agenda-files (list (expand-file-name "work.org" org-home)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled tasks")
                 (org-agenda-files (list (expand-file-name "inbox.org" org-home)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))
          (todo "TODO"
                ((org-agenda-overriding-header "Unscheduled projects tasks")
                 (org-agenda-files (list (expand-file-name "projects.org" org-home)))
                 (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))))))
        ;; Default agenda view
        ("n" "Agenda and all TODOs"
         ((agenda "")
          (alltodo "")))))

;; (maybe-require-package 'json-reformat)
;; (setq json-reformat:indent-width 4)
(setq json-encoding-default-indentation "    ")

;; set pandoc path
(custom-set-variables
 '(markdown-command "/opt/homebrew/bin/pandoc"))

(use-package ox
  :ensure nil
  :after org
  :config
  (add-to-list 'org-export-backends 'pandoc)
  :custom
  (org-export-with-toc t)
  (org-export-with-tags 'not-in-toc)
  (org-export-with-email t)
  (org-export-with-author t)
  (org-export-with-drawers nil)
  (org-export-with-priority t)
  (org-export-with-footnotes t)
  (org-export-with-smart-quotes t)
  (org-export-with-section-numbers nil)
  (org-export-with-sub-superscripts '{})
  ;; Use :eval never-export header argument to avoid evaluating.
  (org-export-use-babel t)
  (org-export-headline-levels 5)
  (org-export-coding-system 'utf-8)
  (org-export-with-broken-links 'mark)
  ;; (org-export-backends '(ascii html md icalendar man))) ; original value
  )

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

(global-display-fill-column-indicator-mode 1)

;; scala setting
(use-package scala-mode
  :ensure t
  :interpreter
  ("scala" . scala-mode))

;; scheme setting using racket mode and racket-langserver
;; racket-langserver: https://github.com/jeapostrophe/racket-langserver
;; racket-mode: https://www.racket-mode.com/
(use-package
  racket-mode
  :ensure t
  :bind
  (:map racket-mode-map
        ("C-c C-c" . racket-run))
  :hook
  (racket-mode . eglot-ensure)
  :mode
  ("\\.ss\\'" . racket-mode) ;; using config doesn't work, need mode binding
  )

;; setting for fill column, enable it for text mode
;; If you want to autofill the written paragraph, you can use (fill-region) or (fill-paragraph)
;; (setq-default fill-column 80)
;; (add-hook 'text-mode-hook 'auto-fill-mode)
;; (add-hook 'org-mode-hook 'auto-fill-mode)
;; display-fill-column-indicator-mode
;; combine visual-line-mode and visual-fill-column-mode

;; 保存文件时自动删除末尾空白字符
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; nov.el a epub reading major mode
;; https://depp.brause.cc/nov.el/

(require 'nov)
(setq nov-unzip-program (executable-find "bsdtar")
      nov-unzip-args '("-xC" directory "-f" filename))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Define font for nov
(defun my-nov-font-setup ()
  (face-remap-add-relative 'variable-pitch :family "Heiti SC"
                           :height 1.0))
(add-hook 'nov-mode-hook 'my-nov-font-setup)

;; elfeed for RSS
;; https://github.com/skeeto/elfeed?tab=readme-ov-file
(global-set-key (kbd "C-x w") 'elfeed)

;; Add RSS source
;; (setq elfeed-feeds '(("https://planet.emacslife.com/atom.xml" emacs)
;;                      "https://www.solidot.org/index.rss"
;;                      ("https://simonwillison.net/atom/everything/" simon ai)))

;; (defvar elfeed-feeds-alist '(("https://planet.emacslife.com/atom.xml" emacs)
;;                              "https://www.solidot.org/index.rss"
;;                              ("https://simonwillison.net/atom/everything/" simon ai)))


;; Config RSS feeds with org file
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))

;; (image-type-available-p 'svg)
(add-to-list 'image-types 'svg)


;; highlight codetag
;; https://www.jamescherti.com/emacs-highlight-keywords-like-todo-fixme-note/

(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\|XXX\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))

  ;; Fontify the current buffer
  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'prog-mode-hook #'highlight-codetags-local-mode)




(provide 'init-local)
;;; init-locales.el ends here
