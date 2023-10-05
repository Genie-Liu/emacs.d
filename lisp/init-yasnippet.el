(require 'yasnippet)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"                 ;; personal snippets
        "~/.emacs.d/elpa-28.1/yasnippet-snippets-20230815.820/snippets/"        ))

(yas-global-mode 1)

(provide 'init-yasnippet)
