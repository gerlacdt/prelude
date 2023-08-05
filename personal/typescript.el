(prelude-require-packages '(js2-mode
                            json-mode
                            prettier-js
                            tree-sitter
                            tree-sitter-langs
                            tide
                            typescript-mode))


(require 'prelude-programming)
(require 'typescript-mode)
(require 'tree-sitter)
(require 'tree-sitter-langs)


(with-eval-after-load 'typescript-mode
  (defun prelude-ts-mode-defaults ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    ;; tslint is outdated, so use eslint for typescript
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    (subword-mode +1)
    (company-mode +1))

  (setq prelude-ts-mode-hook 'prelude-ts-mode-defaults)

  (set-default 'typescript-indent-level 2)
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")

  ;; prettier must be installed globally
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook
            (lambda () (local-set-key (kbd "M-?") #'tide-references)))


  (add-hook 'typescript-mode-hook #'tree-sitter-mode)
  (add-hook 'typescript-mode-hook #'tree-sitter-hl-mode)
  (add-hook 'typescript-mode-hook (lambda () (run-hooks 'prelude-ts-mode-hook))))

;; web-mode for reactjs with typescript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-web-html))
            (company-mode t)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (prelude-ts-mode-defaults))))

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


;; javascript
(set-default 'js-indent-level 2)
(set-default 'js2-bounce-indent-p t)
(set-default 'js2-strict-trailing-comma-warning nil)
(add-hook 'js2-mode-hook 'prettier-js-mode)

;; json
(set-default 'json-reformat:indent-width 2)
