
(require 'avy)
;;;###autoload
(defun my/avy-isearch (&optional arg)
  "Goto isearch candidate in this window with hints."
  (interactive "P")
  (let ((avy-all-windows)
        (current-prefix-arg (if arg 4)))
    (call-interactively 'avy-isearch)))

;;;###autoload
(defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))

;;;###autoload
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    (helpful-at-point))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)


;;;###autoload
(defun avy-action-embark (pt)
  ;; (require 'embark nil :no-error)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
;;;###autoload
(defun avy-action-kill-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-line))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)
;;;###autoload
(defun avy-action-copy-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (cl-destructuring-bind (start . end)
        (bounds-of-thing-at-point 'line)
      (copy-region-as-kill start end)))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)
;;;###autoload
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)
;;;###autoload
(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)

;;;###autoload
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)

;;;###autoload
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

;;;###autoload
(defun avy-action-push-mark-no-activate (pt)
  (push-mark (if pt
                 pt
               (point)) t nil))

(provide 'avy-utils)
