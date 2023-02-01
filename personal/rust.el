(require 'prelude-programming)

;; You may need to install the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)
;; * rust-analyser as lsp server needs to be in global path, see:
;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary

;; fix lsp flycheck startup error in .emacs.d/elpa/flycheck/flycheck.el
;; see also: https://github.com/flycheck/flycheck/pull/1917


(prelude-require-packages '(rust-mode
                            cargo
                            flycheck-rust
                            tree-sitter
                            tree-sitter-langs
                            ron-mode))

(require 'tree-sitter)
(require 'tree-sitter-langs)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'lsp) ; enable lsp for rust
  (add-hook 'rust-mode-hook #'tree-sitter-mode)
  (add-hook 'rust-mode-hook #'tree-sitter-hl-mode)

  (defun prelude-rust-mode-defaults ()
    ;; format on save
    (setq rust-format-on-save t)

    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)

    ;; lsp settings
    (setq lsp-lens-enable nil
          lsp-headerline-breadcrumb-enable t
          lsp-eldoc-enable-hover t
          lsp-signature-auto-activate t
          lsp-signature-render-documentation nil
          lsp-ui-flycheck-enable t
          lsp-ui-sideline-enable t
          lsp-modeline-code-actions-enable nil
          lsp-modeline-diagnostics-enable nil
          lsp-ui-doc-enable t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-show-with-cursor t
          lsp-ui-doc-show-with-mouse nil
          ;; lsp-rust-analyzer-server-display-inlay-hints t
          ;; lsp-rust-analyzer-display-parameter-hints t
          ;; lsp-rust-analyzer-display-chaining-hints t
          lsp-rust-analyzer-proc-macro-enable t
          lsp-rust-analyzer-experimental-proc-attr-macros t
          lsp-rust-analyzer-display-lifetime-elision-hints-enable "always"
          lsp-rust-analyzer-cargo-run-build-scripts t)


    ;; Prevent #! from chmodding rust files to be executable
    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

    ;; enable snippets
    (yas-minor-mode)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-rust-mode-hook 'prelude-rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda ()
                              (run-hooks 'prelude-rust-mode-hook))))

(provide 'prelude-rust)
;;; prelude-rust.el ends here
