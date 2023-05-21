(require 'prelude-programming)

(prelude-require-packages '(terraform-mode))

(with-eval-after-load 'terraform-mode
  (setq lsp-disabled-clients '(tfls)) ;; use hashicorp lsp server
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  (add-hook 'terraform-mode-hook 'lsp) ; enable lsp for terraform


  (defun prelude-terraform-mode-defaults ()

    ;; lsp settings
    (setq lsp-ui-doc-enable t
          lsp-ui-doc-include-signature t
          lsp-ui-doc-show-with-cursor t
          lsp-ui-doc-show-with-mouse nil
          lsp-enable-links t
          lsp-terraform-ls-enable-show-reference t))

  (setq prelude-terraform-mode-hook 'prelude-terraform-mode-defaults)

  (add-hook 'terraform-mode-hook (lambda ()
                              (run-hooks 'prelude-terraform-mode-hook))))

(provide 'prelude-terraform)
;;; terraform.el ends here
