(require 'prelude-common-lisp)
(require 'prelude-racket)

;; common lisp
(setq inferior-lisp-program "/usr/local/bin/sbcl --noinform")
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (let ((map slime-repl-mode-map))
                                    (define-key map (kbd "M-s") 'sp-splice-sexp))))

(add-hook 'slime-mode-hook (lambda ()
                             (let ((map slime-mode-map))
                               (define-key map (kbd "M-_") 'undo-tree-redo))))


;; racket
(add-hook 'racket-repl-mode-hook (lambda ()
                                   (smartparens-strict-mode +1)
                                   (whitespace-mode -1)))
