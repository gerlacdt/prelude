(require 'prelude-programming)

;; You may need to install the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)
;; * rust-analyser as lsp server needs to be in global path, see:
;; https://rust-analyzer.github.io/manual.html#rust-analyzer-language-server-binary


(prelude-require-packages '(rust-mode
                            cargo
                            flycheck-rust
                            ron-mode))

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
  (add-hook 'rust-mode-hook 'lsp) ; enable lsp for rust

  (defun prelude-rust-mode-defaults ()
    ;; format on save
    (setq rust-format-on-save t)

    ;; (add-hook 'before-save-hook #'lsp-format-buffer t t)

    ;; lsp settings
    (setq lsp-enable-snippet nil
          lsp-enable-symbol-highlighting t
          lsp-lens-enable nil
          lsp-headerline-breadcrumb-enable nil
          lsp-eldoc-enable-hover t
          lsp-ui-flycheck-enable t
          lsp-ui-doc-enable t
          lsp-ui-doc-show-with-cursor t
          lsp-ui-doc-show-with-mouse t
          lsp-ui-doc-enable t)

    ;; Prevent #! from chmodding rust files to be executable
    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

    ;; CamelCase aware editing operations
    (subword-mode +1))

  (setq prelude-rust-mode-hook 'prelude-rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda ()
                              (run-hooks 'prelude-rust-mode-hook))))

(provide 'prelude-rust)
;;; prelude-rust.el ends here
