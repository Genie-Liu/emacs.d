;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

;; For Emacs >= 27
(setq read-process-output-max (* 1024 1024))


(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
(add-to-list 'eglot-server-programs '((python-mode) "pylsp"))

(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'python-mode-hook 'eglot-ensure)

(provide 'init-eglot)
;;; init-eglot.el ends here
