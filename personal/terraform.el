(require 'prelude-programming)

(prelude-require-packages '(terraform-mode))

(with-eval-after-load 'terraform-mode

  ;; (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)

  (defun prelude-terraform-mode-defaults ()
    )

  (setq prelude-terraform-mode-hook 'prelude-terraform-mode-defaults)

  (add-hook 'terraform-mode-hook (lambda ()
                                   (run-hooks 'prelude-terraform-mode-hook)))

  (add-hook 'terraform-mode-hook 'eglot-ensure))

(provide 'prelude-terraform)
;;; terraform.el ends here
