;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
;; Also setup the slime-fancy contrib
(add-to-list 'slime-contribs 'slime-fancy)
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (let ((map slime-repl-mode-map))
                                    (define-key map (kbd "M-s") 'sp-splice-sexp))))

(add-hook 'slime-mode-hook (lambda ()
                             (let ((map slime-mode-map))
                               (define-key map (kbd "M-_") 'undo-tree-redo))))


;; scheme
(add-hook 'scheme-mode-hook 'geiser-mode)
(setq geiser-active-implementations '(racket))
(setq scheme-program-name "racket -il xrepl")
(add-hook 'geiser-repl-mode-hook (lambda () (smartparens-mode)))

;; latex and flyspell
(setq-default ispell-program-name "aspell")
(add-hook 'LaTeX-mode-hook 'tex-pdf-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(setq ispell-dictionary "en")
(setq ispell-default-dictionary "en")
(setq flyspell-default-dictionary "en")
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; (setq flyspell-issue-welcome-flag nil) ;; fix for Ubuntu 10.10 problem
(add-hook 'flyspell-mode-hook 'flyspell-buffer)
(add-hook 'org-mode-hook 'flyspell-mode)

;; lsp-mode
(setq lsp-enable-snippet nil)

;; golang
(add-hook 'go-mode-hook
          (lambda ()
            (lsp-deferred)
            (setq tab-width 4)
            (setq indent-tabs-mode 1)
            (let ((map go-mode-map))
              (define-key map (kbd "M-.") 'godef-jump))
            (add-hook 'before-save-hook #'lsp-format-buffer t t)
            (add-hook 'before-save-hook #'lsp-organize-imports t t)))


;; typescript
(set-default 'typescript-indent-level 2)
(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
;; prettier must be installed globally
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook
          (lambda () (local-set-key (kbd "C-c C-l") #'tide-references)))
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (subword-mode +1)
  (company-mode +1))
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(setq prettier-js-args '(
                         "--trailing-comma" "all"
                         ))
(setq prettier-js-command "prettier")

;; web-mode for reactjs with typescript
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends) '(company-web-html))
            (company-mode t)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


;; javascript
(set-default 'js-indent-level 2)
(set-default 'js2-bounce-indent-p t)
(set-default 'js2-strict-trailing-comma-warning nil)
(add-hook 'js2-mode-hook 'prettier-js-mode)

;; json
(set-default 'json-reformat:indent-width 2)


;; python
(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")
(add-hook 'inferior-python-mode-hook
          '(lambda ()
             (smartparens-mode)))

(add-hook 'python-mode-hook 'blacken-mode)

(setq flycheck-python-pycompile-executable "python3")

;; was needed for macos, with messed up python installation
;; (setq flycheck-python-flake8-executable "/usr/local/bin/flake8")
;; (setq flycheck-python-pylint-executable "/usr/local/bin/pylint")
