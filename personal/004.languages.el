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
