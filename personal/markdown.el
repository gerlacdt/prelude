(prelude-require-packages '(prettier-js))


(add-hook 'markdown-mode-hook 'prettier-js-mode)
