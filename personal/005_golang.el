;; lsp-mode, disable snippets because i do not use yasnippets
(setq lsp-enable-snippet nil)

(require 'prelude-programming)

(prelude-require-packages '(go-mode
                            go-eldoc
                            go-projectile
                            gotest))

(require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(define-key 'help-command (kbd "G") 'godoc)

(with-eval-after-load 'go-mode
  (defun prelude-go-mode-defaults ()
    ;; lsp config
    (lsp-deferred)
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t)

    ;; my defaults
    (setq tab-width 4)
    (setq indent-tabs-mode 1)
    (let ((map go-mode-map))
      (define-key map (kbd "M-.") 'godef-jump))

    ;; Add to default go-mode key bindings
    (let ((map go-mode-map))
      (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
      (define-key map (kbd "C-c m") 'go-test-current-file)
      (define-key map (kbd "C-c .") 'go-test-current-test)
      (define-key map (kbd "C-c b") 'go-run)
      (define-key map (kbd "C-h f") 'godoc-at-point))

    ;; Prefer goimports to gofmt if installed
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    ;; stop whitespace being highlighted
    (whitespace-toggle-options '(tabs))


    ;; El-doc for Go
    (go-eldoc-setup)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-go-mode-hook 'prelude-go-mode-defaults)

  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'prelude-go-mode-hook))))

(provide 'prelude-go)
;;; prelude-go.el ends here
