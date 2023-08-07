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


;; racket
(add-hook 'racket-repl-mode-hook (lambda () (smartparens-mode)))

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

;; active spell checking for org-mode and markdown-mode
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'markdown-mode-hook 'flyspell-mode)


;; web-mode
(setq web-mode-enable-auto-closing t)
