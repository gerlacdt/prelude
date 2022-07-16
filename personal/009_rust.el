(require 'prelude-programming)

;; You may need to install the following packages on your system:
;; * rustc (Rust Compiler)
;; * cargo (Rust Package Manager)
;; * racer (Rust Completion Tool)
;; * rustfmt (Rust Tool for formatting code)
;; * rls (Rust Language Server, if the prelude-lsp feature is enabled)

(prelude-require-packages '(rust-mode
                            cargo
                            flycheck-rust
                            ron-mode))

(unless (featurep 'prelude-lsp)
  (prelude-require-packages '(racer)))

(setq rust-format-on-save t)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  (if (featurep 'prelude-lsp)
      (add-hook 'rust-mode-hook 'lsp)
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'racer-mode-hook 'eldoc-mode))

  (defun prelude-rust-mode-defaults ()
    (unless (featurep 'prelude-lsp)
      (local-set-key (kbd "C-c C-d") 'racer-describe)
      (local-set-key (kbd "C-c .") 'racer-find-definition)
      (local-set-key (kbd "C-c ,") 'pop-tag-mark))

    (setq lsp-enable-snippet nil
          lsp-ui-flycheck-enable t
          lsp-ui-doc-enable t
          lsp-ui-doc-show-with-cursor t
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
