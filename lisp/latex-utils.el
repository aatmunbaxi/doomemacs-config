;;;###autoload
(defun forward-latex-math ()
  "Move forward across the next LaTeX equation.
It is meant work like `forward-sexp' but for LaTeX math delimiters."
  (interactive)
  ;; (require 'latex)
  (let ((count 1))
    ;; Search for either of the following \( \) \[ \]
    (re-search-forward-ignore-TeX-comments "\\\\(\\|\\\\)\\|\\\\\\[\\|\\\\]")
    (cond
     ;; If the search nhits \(
     ((looking-back "\\\\(" (- (point) 2))
      (while (< 0 count)
        ;; Search for delimiters inside the equation
        (re-search-forward-ignore-TeX-comments "\\\\(\\|\\\\)")
        (if (looking-back "\\\\(" (- (point) 2))
            (setq count (1+ count))     ; If start of a nested level
          (setq count (1- count))))     ; If end of a nested level
      ;; Find the matching \)
      (re-search-forward "\\\\)" (eobp) t count))
     ;; If the search hits \[
     ((looking-back "\\\\\\[" (- (point) 2))
      (while (< 0 count)
        ;; Search for delimiters inside the equation
        (re-search-forward-ignore-TeX-comments "\\\\\\[\\|\\\\]")
        (if (looking-back "\\\\\\[" (- (point) 2))
            (setq count (1+ count))     ; If start of a nested level
          (setq count (1- count))))     ; If end of a nested level
      ;; Find the matching \]
      (re-search-forward "\\\\]" (eobp) t count)))))

;;;###autoload
(defun backward-latex-math ()
  "Move forward across the next LaTeX equation.
It is meant work like `forward-sexp' but for LaTeX math delimiters."
  (interactive)
  ;; (require 'latex)
  (let ((count 1))
    ;; Search for either of the following \( \) \[ \]
    (re-search-backward-ignore-TeX-comments "\\\\(\\|\\\\)\\|\\\\\\[\\|\\\\]")
    (cond
     ;; If the search hits \)
     ((looking-at "\\\\)")
      (while (< 0 count)
        ;; Search for delimiters inside the equation
        (re-search-backward-ignore-TeX-comments "\\\\(\\|\\\\)")
        (if (looking-at "\\\\)")
            (setq count (1+ count))     ; If start of a nested level
          (setq count (1- count))))     ; If end of a nested level
      ;; Find the matching \(
      (re-search-forward "\\\\(" (eobp) t count))
     ;; If the search hits \[
     ((looking-at "\\\\\\]")
      (while (< 0 count)
        ;; Search for delimiters inside the equation
        (re-search-backward-ignore-TeX-comments "\\\\\\[\\|\\\\]")
        (if (looking-at "\\\\\\]")
            (setq count (1+ count))     ; If start of a nested level
          (setq count (1- count))))     ; If end of a nested level
      ;; Find the matching \]
      (re-search-forward "\\\\[" (eobp) t count)))))

(defun re-search-forward-ignore-TeX-comments (regexp)
  "Search for REGEXP and ignore TeX comments. Used by `forward-latex-math'."
  (re-search-forward regexp (eobp) t)
  ;; If in comment search to after it
  (while (TeX-in-comment)
    (forward-line)
    (re-search-forward regexp (eobp) t)))

(defun re-search-backward-ignore-TeX-comments (regexp)
  "Search for REGEXP and ignore TeX comments. Used by `forward-latex-math'."
  (re-search-backward regexp (bobp) t)
  ;; If in comment search to after it
  (while (TeX-in-comment)
    (forward-line -1)
    (re-search-forward regexp (eobp) t)))
(provide 'latex-utils)
