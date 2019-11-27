(require 'prelude-xml)

;;split emacs in 3 windows with same height and width
(defun gerlacdt/split3 ()
  (interactive)
  (split-window-horizontally)
  (split-window-horizontally) (balance-windows))


(defun gerlacdt/json-compress ($string &optional $from $to)
  "Remove all spaces and newlines in the given string. When
called interactively, work on current paragraph or text
selection. When called in lisp code, if $string is non-nil,
returns a changed string.  If $string nil, change the text in the
region between positions $from $to."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)))
       (list nil (car bds) (cdr bds)))))

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if $string t nil))
    (setq inputStr (if workOnStringP $string (buffer-substring-no-properties $from $to)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "\s-*\\|\n" "" inputStr)))
    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region $from $to)
        (goto-char $from)
        (insert outputStr)))))


(defun gerlacdt/unfill-region (from to)
  "Remove all newlines in the marked region FROM and TO.
Replace them with a single space."
  (interactive "r")
  (let (inputStr outputStr)
    (setq inputStr (buffer-substring-no-properties from to))
    (setq outputStr (let ((case-fold-search t))
                      (replace-regexp-in-string "\n" " " inputStr)))
    (save-excursion
      (delete-region from to)
      (goto-char from)
      (insert outputStr))))


;; xml
(defun gerlacdt/xml-pretty-print (point-min point-max)
  "Formats the marked region in pretty-printed xml.
POINT-MIN and POINT-MAX mark the region."
  (interactive "r")
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))


(defun gerlacdt/recompile-all-packages ()
  "Recompile all installed ELPA packages. Needed if some package
is deprecated and needs re-compilation."
  (interactive)
  (byte-recompile-directory package-user-dir 0 t))


(defun gerlacdt/find-init-file ()
  "Opens the current custom.el."
  (interactive)
  (find-file (expand-file-name "~/.emacs.d/personal/custom.el")))


(defun gerlacdt/clear-packages ()
  "Installs selectd packages and removes all unselected packages."
  (package-install-selected-packages)
  (package-autoremove))
