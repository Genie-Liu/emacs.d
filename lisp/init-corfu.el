;;; init-corfu.el --- Interactive completion in buffers -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; WAITING: haskell-mode sets tags-table-list globally, breaks tags-completion-at-point-function
;; TODO Default sort order should place [a-z] before punctuation

(setq tab-always-indent 'complete)
(when (maybe-require-package 'orderless)
  (with-eval-after-load 'vertico
    (require 'orderless)
    (setq completion-styles '(orderless basic))))
(setq completion-category-defaults nil
      completion-category-overrides nil)
(setq completion-cycle-threshold 4)

(when (and (version< "28.1" emacs-version) (maybe-require-package 'corfu))
  (setq-default corfu-auto t)
  (with-eval-after-load 'eshell
    (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-auto nil))))
  (setq-default corfu-quit-no-match 'separator)
  (add-hook 'after-init-hook 'global-corfu-mode)

  (define-key corfu-mode-map (kbd "<tab>") 'corfu-complete)

  (with-eval-after-load 'corfu
    (corfu-popupinfo-mode))


  (setq tab-always-indent 'complete)
  (setq corfu-preview-current nil)
  (setq corfu-min-width 20)
  (setq corfu-popupinfo-delay '(1.25 . 0.5))


  ;; Make Corfu also work in terminals, without disturbing usual behaviour in GUI
  (when (maybe-require-package 'corfu-terminal)
    (with-eval-after-load 'corfu
      (corfu-terminal-mode)))

  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; TODO: https://github.com/jdtsmith/kind-icon
  )

;; Add extensions

(when  (maybe-require-package 'cape)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

;;(add-to-list 'completion-at-point-functions #'cape-history)
;;(add-to-list 'completion-at-point-functions #'cape-keyword)
;;(add-to-list 'completion-at-point-functions #'cape-tex)
;;(add-to-list 'completion-at-point-functions #'cape-sgml)
;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
;;(add-to-list 'completion-at-point-functions #'cape-ispell)
;;(add-to-list 'completion-at-point-functions #'cape-dict)
;;(add-to-list 'completion-at-point-functions #'cape-symbol)
;;(add-to-list 'completion-at-point-functions #'cape-line)

(provide 'init-corfu)
;;; init-corfu.el ends here
