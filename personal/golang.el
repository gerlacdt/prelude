(require 'prelude-lsp)
(require 'prelude-programming)
(require 'go-projectile)

(prelude-require-packages '(go-mode
                            tree-sitter
                            tree-sitter-langs))

(require 'tree-sitter)
(require 'tree-sitter-langs)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(with-eval-after-load 'go-mode

  (add-hook 'go-mode-hook #'tree-sitter-mode)
  (add-hook 'go-mode-hook #'tree-sitter-hl-mode)
  (defun prelude-go-mode-defaults ()
    ;; lsp config
    (setq lsp-enable-snippet t
          lsp-enable-symbol-highlighting t
          lsp-ui-flycheck-enable t
          lsp-ui-doc-enable t
          lsp-ui-doc-show-with-mouse nil
          lsp-ui-doc-show-with-cursor t)
    (lsp-register-custom-settings
     '(("gopls.completeUnimported" t t)
       ("gopls.staticcheck" t t)))
    (lsp)

    ;; my defaults
    (setq tab-width 4)
    (setq indent-tabs-mode 1)
    (let ((map go-mode-map))
      (define-key map (kbd "M-.") 'lsp-ui-peek-find-definitions)
      (define-key map (kbd "M-?") 'lsp-ui-peek-find-references))

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))

    ;; enable snippets
    (yas-minor-mode)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

  (setq prelude-go-mode-hook 'prelude-go-mode-defaults)

  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'prelude-go-mode-hook))))
