;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot)

  ;; 1. Global Performance Tweaks
  ;; Increase the amount of data Emacs reads from processes to avoid stuttering
  (setq read-process-output-max (* 1024 1024)) ; 1mb

  ;; 2. Server Configurations
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(racket-mode . ("racket" "-l" "racket-langserver")))
    (add-to-list 'eglot-server-programs '((c++-mode c-mode c++-ts-mode) . ("clangd" "--header-insertion=never")))
    (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))

    ;; Optional: Shutdown server when last buffer is closed to save RAM
    (setq eglot-autoshutdown t)

    ;; Optimization: Disable heavy flymake UI if you prefer a cleaner look
    ;; (setq eglot-ignored-server-capabilities '(:hoverProvider))
    )

  ;; 3. Automatic Activation
  ;; Instead of manual M-x eglot, start it automatically for these modes
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'c-mode-hook 'eglot-ensure)
  (add-hook 'c++-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'racket-mode-hook 'eglot-ensure)

  ;; 4. Keybindings
  (defun my/eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-format-buffer-on-save))

;; (require 'init-metal)
;; (add-to-list 'eglot-server-programs '((scala-mode) . "metals-emacs"))
;; (add-hook 'scala-mode-hook 'eglot-ensure)

(provide 'init-eglot)
;;; init-eglot.el ends here
