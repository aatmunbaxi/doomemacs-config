;;; * Setup load path
(if (equal (system-name) "pop-os")
    (add-to-list 'load-path "~/.config/doom/")
  (add-to-list 'load-path "~/.doom.d/"))

;;; * Some functions
(defmacro find-in-dir (dir name)
  `(defun ,(intern (concat "find-in-" name)) ()
     (interactive)
     (ido-find-file-in-dir ,dir)))

(find-in-dir "~/Documents/org/" "org")
(find-in-dir "~/Documents/books/" "books")
(find-in-dir "~/Documents/bib/pdfs/" "articles")

(defun my/close-other-window nil
  (interactive)
  (delete-window (ace-select-window)))

(defun my/switch-buffer-other-window ()
  (interactive)
  (save-excursion
    (consult-buffer-other-window)))

;;; * Basic emacs stuff
(when (equal (system-name) "EVA-02")
  (display-battery-mode))

(display-time-mode)
(when (require 'activities nil t)
  (activities-mode))

;;; * Buffer related config
;;; ** Popper
(setq! popper-reference-buffers
       '("\\*Messages\\*"
         "Output\\*$"
         "\\*compilation\\*"
         "\\*Async Shell Command\\*"
         "\\*Bufler\\*"
         "\\*sly-description\\*"
         helpful-mode
         compilation-mode
         "\\*Outline .*?\\*"))

(popper-mode +1)
(popper-echo-mode)

;;; ** Bufler
(after! bufler
  (bufler-defgroups
   (group
    ;; Subgroup collecting all named workspaces.
    (auto-workspace))
   (group
    ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
    (group-or "*Help/Info*"
              (mode-match "*Help*" (rx bos "help-"))
              (mode-match "*Info*" (rx bos "info-"))))
   (group
    ;; Subgroup collecting all special buffers (i.e. ones that are not
    ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
    ;; through to other groups, so they end up grouped with their project buffers).
    (group-and "*Special*"
               (lambda (buffer)
                 (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
                                      buffer)
                             (funcall (mode-match "Dired" (rx bos "dired"))
                                      buffer)
                             (funcall (auto-file) buffer))
                   "*Special*")))
    (group
     ;; Subgroup collecting these "special special" buffers
     ;; separately for convenience.
     (name-match "**Special**"
                 (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
    (group
     ;; Subgroup collecting all other Magit buffers, grouped by directory.
     (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
     (auto-directory))
    ;; Subgroup for Helm buffers.
    (mode-match "*Helm*" (rx bos "helm-"))
    ;; Remaining special buffers are grouped automatically by mode.
    (auto-mode))
   ;; All buffers under "~/.emacs.d" (or wherever it is).
   (dir user-emacs-directory)
   (group
    ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
    ;; `org-directory' is not yet defined).
    (dir (if (bound-and-true-p org-directory)
             org-directory
           "~/org"))
    (group
     ;; Subgroup collecting indirect Org buffers, grouping them by file.
     ;; This is very useful when used with `org-tree-to-indirect-buffer'.
     (auto-indirect)
     (auto-file))
    ;; Group remaining buffers by whether they're file backed, then by mode.
    (group-not "*special*" (auto-file))
    (auto-mode))
   (group
    ;; Subgroup collecting buffers in a projectile project.
    (auto-projectile))
   (group
    ;; Subgroup collecting buffers in a version-control project,
    ;; grouping them by directory.
    (auto-project))
   ;; Group remaining buffers by directory, then major mode.
   (auto-directory)
   (auto-mode)))

;;; * Editing
(setq! kill-whole-line t)

(defun my/find-bounds-of-regexps (open close)
  (let ((start (point))
        (parity 0)
        (open-close (concat "\\(?:" open "\\|" close "\\)"))
        end)
    (save-excursion
      (while (and (not (= parity -1))
                  (re-search-backward open-close nil t))
        (if (looking-at open)
            (setq parity (1- parity))
          (setq parity (1+ parity))))
      (setq end (point))
      (goto-char start)
      (while (and (not (= parity 0))
                  (re-search-forward open-close nil t))
        (if (looking-back
             close
             (- (point) (length (match-string-no-properties 0))))
            (setq parity (1+ parity))
          (setq parity (1- parity))))
      (when (= parity 0) (cons end (point))))))

;;; ** `easy-kill' and `expand-region' interop
(after! (:and easy-kill expand-region)
  (defun easy-kill-expand-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/expand-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark)))))

  (defun easy-kill-contract-region ()
    "Expand kill according to expand-region."
    (interactive)
    (let* ((thing (easy-kill-get nil))
           (bounds (easy-kill--bounds)))
      (save-mark-and-excursion
        (set-mark (cdr bounds))
        (goto-char (car bounds))
        (er/contract-region 1)
        (deactivate-mark)
        (easy-kill-adjust-candidate thing (point) (mark))))))

;;; ** Mark utilities
(defun push-mark-no-activate (&optional pt)
  "Push `point' to local mark ring, without activating the region."
  (interactive)
  (push-mark (if pt
                 pt
               (point)) t nil)
  (message "Pushed point to mark ring"))

(defun jump-to-mark ()
  "Jump to local mark, respecting `mark-ring' order."
  (interactive)
  (set-mark-command 1))
;;; ** `easy-kill'
;; (after! easy-kill
;;   (require 'extra-things)
;;   (add-to-list 'easy-kill-alist '(?W  WORD " ") t)
;;   (add-to-list 'easy-kill-alist '(?\' squoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?\" dquoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?\` bquoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?q  quoted-string "") t)
;;   (add-to-list 'easy-kill-alist '(?Q  quoted-string-universal "") t)
;;   (add-to-list 'easy-kill-alist '(?\) parentheses-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\( parentheses-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\] brackets-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?\[ brackets-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?}  curlies-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?{  curlies-pair "\n") t)
;;   (add-to-list 'easy-kill-alist '(?>  angles-pair-content "\n") t)
;;   (add-to-list 'easy-kill-alist '(?<  angles-pair "\n") t))

;;; ** Smartparens
(add-hook! prog-mode #'sp-use-smartparens-bindings)

;;; * Font config
;;; ** Doom font config
(setq! font-of-choice (if (equal (system-name) "pop-os")
                          (font-spec :family "JetBrains Mono" :size 18)
                        (font-spec :family "JetBrains Mono Nerd Font" :size 18))

       variable-font-of-choice  (font-spec :family "Iosevka Aile" :size 18)

       big-font (font-spec :family "Iosevka Aile" :style "Bold"  :size 28))

(setq! doom-font font-of-choice
       doom-variable-pitch-font variable-font-of-choice
       doom-serif-font (font-spec :family "IBM Plex Serif" :size 19)
       doom-big-font big-font)

;;; ** Fontaine
(setq! fontaine-presets
       '((regular
          :variable-pitch-family variable-font-of-choice
          :fixed-pitch-family font-of-choice
          :default-height 100)
         (medium
          :inherit regular
          :default-height 140)
         (large
          :inherit medium
          :default-height 180)

         (t ; our shared fallback properties
          :default-family variable-font-of-choice
          :default-weight normal
          :fixed-pitch-family font-of-choice  ; falls back to :default-family
          :fixed-pitch-height 1.0
          :variable-pitch-family variable-font-of-choice
          :variable-pitch-weight normal
          :variable-pitch-height 1.05
          :bold-family nil ; use whatever the underlying face has
          :bold-weight bold
          :italic-family nil
          :italic-slant italic
          :line-spacing nil)))

(variable-pitch-mode)


;;; * LaTeX
(setq! LaTeX-always-use-Biber t
       bibtex-dialect 'biblatex)
;;; ** Helper functions
(defun my/cdlatex-sub-superscript ()
  "Insert ^{} or _{} unless the number of backslashes before point is odd.
When not in LaTeX math environment, _{} and ^{} will have dollars.
When pressed twice, make the sub/superscript roman."
  (interactive)
  (if (and nil
           (equal this-command last-command))
      (progn
        (insert "\\mathrm{}")
        (backward-char 1))
    (if (cdlatex-number-of-backslashes-is-odd)
        ;; Quoted
        (insert (event-basic-type last-command-event))
      ;; Check if we are in math mode, if not switch to or only add _ or ^
      (if (not (or (laas-mathp)
                   cdlatex-sub-super-scripts-outside-math-mode))
          (insert (event-basic-type last-command-event))
        (cdlatex-ensure-math)
        ;; Insert the normal template.
        (insert (event-basic-type last-command-event))
        (insert "{}")
        (forward-char -1)))))

(defun newline-if-not-already ()
  (unless (= (point) (pos-bol))
    (newline)))

(defun insert-newline-separated-text (beg end)
  (interactive)
  (newline-if-not-already)
  (insert (concat beg "\n"))
  (save-excursion
    (insert (concat "\n" end))))

(defun forward-latex-math ()
  "Move forward across the next LaTeX equation. It is meant work like `forward-sexp' but for LaTeX math delimiters."
  (interactive)
  (require 'latex)
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

(defun backward-latex-math ()
  "Move forward across the next LaTeX equation. It is meant work like `forward-sexp' but for LaTeX math delimiters."
  (interactive)
  (require 'latex)
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


(defvar select-newlines-with-envs)
(setq!  select-newlines-with-envs nil)

(defun evil-tex--select-env ()
  (let (outer-beg outer-end inner-beg inner-end)
    (save-excursion
      (cond
       ;; `LaTeX-find-matching-begin' doesn't like being exactly on the \\begin
       ((looking-at (regexp-quote "\\begin{"
                                  t)))
       ;; `LaTeX-find-matching-begin' doesn't like being near the } of \\end{}
       ((or (= (char-before) ?))))
      (= (char-after) ?
         (backward-char 2)
         (LaTeX-find-matching-begin
          (t))
         (LaTeX-find-matching-begin
          ;; We are at backslash of \\begin
          (setq outer-beg (point))
          (forward-sexp)
          (while (or))
          (= (char-after) ?{})
          (= (char-after) ?\[))
         (forward-sexp
          (when (and select-newlines-with-envs))
          (looking-at "\n[ \t]*"))
         (goto-char (match-end 0)
                    (setq inner-beg (point))
                    (goto-char (1+ outer-beg))
                    (LaTeX-find-matching-end)        ; we are at closing brace
                    (setq outer-end (point))
                    (search-backward "\\end")        ; goto backslash
                    (when (and select-newlines-with-envs))
                    (looking-back "\n[ \t]*" (- (point) 10)))
         (goto-char (match-beginning 0)
                    (setq inner-end (point))
                    (list outer-beg outer-end inner-beg inner-end))))))

;; Select inner and outer environment pairs
(defun inner-of-latex-env-at-point ()
  (let ((result (evil-tex--select-env)))
    (cons (caddr result) (cadddr result))))

(defun bounds-of-latex-env-at-point ()
  (let ((result (evil-tex--select-env)))
    (cons (car result) (cadr result))))

(setq latex-delim-pairs
      (cl-loop for (l r)
               in '(( "(" ")")
                    ( "\\[" "\\]")
                    ( "\\\\{" "\\\\}")
                    ( "\\\\lvert" "\\\\rvert")
                    ( "\\\\lVert" "\\\\rVert")
                    ( "\\\\langle" "\\\\rangle"))
               nconc
               (cl-loop for (pre-l pre-r)
                        in '(  ;; ("" "")
                             ( "\\\\left"  "\\\\right")
                             ( "\\\\bigl"  "\\\\bigr")  ("\\\\big"  "\\\\big")
                             ( "\\\\biggl" "\\\\biggr") ("\\\\bigg" "\\\\bigg")
                             ( "\\\\Bigl"  "\\\\Bigr")  ("\\\\Big"  "\\\\Big")
                             ( "\\\\Biggl" "\\\\Biggr") ("\\\\Bigg" "\\\\Bigg"))
                        collect (cons (concat pre-l l) (concat pre-r r)))))

(defun append-bounds-distance (pair)
  (if pair
      (cons pair (min  (- (point) (car pair))  (- (cdr pair) (point))))
    nil))

(defun find-min-distance-match (matches-with-distances)
  (let ((nearest-match (cons (point) (point)))
        (min-distance (float 1.0e+INF)))
    (pcase-dolist (`(,delims . ,distance) matches-with-distances)
      ;;      (message (format "%1$s with distance %2$s " delims distance))
      (when distance
        (when  (> min-distance distance)
          (setq nearest-match delims)
          (setq min-distance distance))))
    nearest-match))


(defun meow--thing-parse-pair-search (push-token pop-token back near)
  (let* ((search-fn (if back #'re-search-backward #'re-search-forward))
         (match-fn (if back #'match-end #'match-beginning))
         (cmp-fn (if back #'> #'<))
         (push-next-pos nil)
         (pop-next-pos nil)
         (push-pos (save-mark-and-excursion
                     (when (funcall search-fn push-token nil t)
                       (setq push-next-pos (point))
                       (if near (funcall match-fn 0) (point)))))
         (pop-pos (save-mark-and-excursion
                    (when (funcall search-fn pop-token nil t)
                      (setq pop-next-pos (point))
                      (if near (funcall match-fn 0) (point))))))
    (cond
     ((and (not pop-pos) (not push-pos))
      nil)
     ((not pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     ((not push-pos)
      (goto-char pop-next-pos)
      (cons 'pop pop-pos))
     ((funcall cmp-fn push-pos pop-pos)
      (goto-char push-next-pos)
      (cons 'push push-pos))
     (t
      (goto-char pop-next-pos)
      (cons 'pop pop-pos)))))



(defun meow--thing-pair-function (push-token pop-token near)
  (let* ((found nil)
         (depth  0)
         (beg (save-mark-and-excursion
                (prog1
                    (let ((case-fold-search nil))
                      (while (and (<= depth 0)
                                  (setq found (meow--thing-parse-pair-search push-token pop-token t near)))
                        (let ((push-or-pop (car found)))
                          (if (eq 'push push-or-pop)
                              (cl-incf depth)
                            (cl-decf depth))))
                      (when (> depth 0) (cdr found)))
                  (setq depth 0
                        found nil))))
         (end (save-mark-and-excursion
                (let ((case-fold-search nil))
                  (while (and (>= depth 0)
                              (setq found (meow--thing-parse-pair-search push-token pop-token nil near)))
                    (let ((push-or-pop (car found)))
                      (if (eq 'push push-or-pop)
                          (cl-incf depth)
                        (cl-decf depth))))
                  (when (< depth 0) (cdr found))))))
    (when (and beg end)
      (cons beg end))))

(defun my/closest-delim-search (delims near)
  "Find LaTeX parenthesis bounds.
NEAR denotes if match should be inner or bounds"
  (interactive)
  (let ((found-pairs (list)))
    (pcase-dolist (`(,left . ,right)  delims)
      (push  (meow--thing-pair-function
              left right near) found-pairs))
    (let ((bounds-with-distances (mapcar #'append-bounds-distance found-pairs)))
      (find-min-distance-match bounds-with-distances))))

(defun bounds-of-latex-delim-at-point ()
  (my/closest-delim-search latex-delim-pairs nil))

(defun inner-bounds-of-latex-delim-at-point ()
  (my/closest-delim-search latex-delim-pairs t))

;;; ** Easy kill LaTeX delimiters
(setq! math-delims '(("\\\\(" . "\\\\)")
                     ("\\\\[[]" . "\\\\[]]")))

(defun bounds-of-math-at-point ()
  (my/closest-delim-search math-delims nil))

(defun inner-bounds-of-math-at-point ()
  (my/closest-delim-search math-delims t))

(put 'delim-bounds 'bounds-of-thing-at-point 'bounds-of-latex-delim-at-point)
(put 'delim-inner 'bounds-of-thing-at-point 'inner-bounds-of-latex-delim-at-point)

(add-to-list 'thing-at-point-provider-alist '((delim-bounds . bounds-of-latex-delim-at-point)
                                              (delim-inner . inner-of-latex-delim-at-point)))

(put 'math       'bounds-of-thing-at-point 'bounds-of-math-at-point)
(put 'math-inner 'bounds-of-thing-at-point 'inner-bounds-of-math-at-point)


(add-to-list 'thing-at-point-provider-alist '((math       . bounds-of-math-at-point)
                                              (math-inner . inner-bounds-of-math-at-point)))

(put 'env-bounds 'bounds-of-thing-at-point 'bounds-of-latex-env-at-point)
(put 'env-inner 'bounds-of-thing-at-point 'inner-of-latex-env-at-point)
(add-to-list 'thing-at-point-provider-alist '((env-bounds . bounds-of-latex-delim-at-point)
                                              (env-inner . inner-of-latex-delim-at-point)))

(after! easy-kill
  (add-to-list 'easy-kill-alist '(?p delim-inner))
  (add-to-list 'easy-kill-alist '(?P delim-bounds))
  (add-to-list 'easy-kill-alist '(?m math-inner))
  (add-to-list 'easy-kill-alist '(?M math))
  (add-to-list 'easy-kill-alist '(?o env-inner))
  (add-to-list 'easy-kill-alist '(?O env-bounds)))

;;; ** laas-mode
(add-hook! org-mode #'laas-mode)

(after! (:and laas aas)
  (aas-set-snippets 'laas-mode
    ";m" (lambda ()
           (interactive)
           (insert "\\(  ")
           (save-excursion
             (insert "  \\)")))


    "dm" (lambda ()
           (interactive)
           (if (word-at-point)
               (insert "dm")
             (insert-newline-separated-text "\\[" "\\]\n")))

    :cond #'laas-mathp
    "opr" '(tempel "\\operatorname{" r "}" q)
    "^" #'my/cdlatex-sub-superscript
    "_" #'my/cdlatex-sub-superscript
    "ox" "\\otimes"
    "iso" "\\cong"
    "hom" "\\hom"
    "ker" "\\ker"
    "ZZ" "\\mathbb{Z}"
    "CC" "\\mathbb{C}"
    "RR" "\\mathbb{R}"
    "QQ" "\\mathbb{Q}"))

;; (add-hook 'aas-pre-snippet-expand-hook (setq! smartparens-mode nil)
;;           (setq! global-smartparens-mode nil))
;; (add-hook 'aas-post-snippet-expand-hook (setq! smartparens-mode t)
;;           (setq! global-smartparens-mode t))
(require 'cdlatex)
(defun my/org-try-cdlatex-tab ()
  (interactive)
  "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `laas-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
  (require 'cdlatex)
  (if laas-mode
      (cond
       ;; Before any word on the line: No expansion possible.
       ((save-excursion (skip-chars-backward " \t") (bolp)) (forward-word))
       ;; Just after first word on the line: Expand it.  Make sure it
       ;; cannot happen on headlines, though.
       ((save-excursion)))
    (skip-chars-backward "a-zA-Z0-9*")
    (skip-chars-backward " \t")
    (and (bolp) (not (org-at-heading-p))
         (cdlatex-tab ) t
         ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t)
         (t (forward-word))
         (forward-word))))

;;; ** `expand-region' interop with `easy-kill'
(defun er/mark-LaTeX-inside-math ()
  "Mark text inside LaTeX math delimiters. See `er/mark-LaTeX-math'
for details."
  (when (texmathp)
    (let* ((string (car texmathp-why))
           (pos (cdr texmathp-why))
           (reason (assoc string texmathp-tex-commands1))
           (type (cadr reason)))
      (cond
       ((eq type 'sw-toggle) ;; $ and $$
        (goto-char pos)
        (set-mark (1+ (point)))
        (forward-sexp 1)
        (backward-char 1)
        (exchange-point-and-mark))
       ((or (eq type 'sw-on)
            (equal string "Org mode embedded math")) ;; \( and \[
        (re-search-forward texmathp-onoff-regexp)
        (backward-char 2)
        (set-mark (+ pos 2))
        (exchange-point-and-mark))
       (t (error (format "Unknown reason to be in math mode: %s" type)))))))
(defun er/mark-latex-inside-pairs ()
  (if (texmathp)
      (cl-destructuring-bind (beg . end)
          (my/find-bounds-of-regexps " *[{([|<]"
                                     " *[]})|>]")
        (when-let ((n (length (match-string-no-properties 0))))
          (set-mark (save-excursion
                      (goto-char beg)
                      (forward-char n)
                      (skip-chars-forward er--space-str)
                      (point)))
          (goto-char end)
          (backward-char n)
          (if (looking-back "\\\\right\\\\*\\|\\\\" (- (point) 7))
              (backward-char (length (match-string-no-properties 0)))))
        (skip-chars-backward er--space-str)
        (exchange-point-and-mark))
    (er/mark-inside-pairs)))

(defun er/mark-latex-outside-pairs ()
  (if (texmathp)
      (cl-destructuring-bind (beg . end)
          (my/find-bounds-of-regexps " *[{([|<]"
                                     " *[]})|>]")
        (set-mark (save-excursion
                    (goto-char beg)
                    ;; (forward-char 1)
                    (if (looking-back "\\\\left\\\\*\\|\\\\" (- (point) 6))
                        (backward-char (length (match-string-no-properties 0))))
                    (skip-chars-forward er--space-str)
                    (point)))
        (goto-char end)
        (skip-chars-backward er--space-str)
        ;; (backward-char 1)
        (exchange-point-and-mark))
    (er/mark-outside-pairs)))






;;; * org-mode
;;; ** Variables
(after! org
  (setq! org-agenda-files '( "~/Documents/org/inbox.org"
                             "~/Documents/org/gtd.org"
                             "~/Documents/org/tickler.org"
                             "~/Documents/org/graveyard.org")

         org-agenda-include-deadlines t
         org-agenda-block-separator nil
         org-agenda-compact-blocks nil
         org-agenda-start-day nil ;; i.e. today
         org-agenda-span 4
         org-agenda-skip-scheduled-if-done t
         org-agenda-skip-deadline-if-done t
         org-agenda-start-day "-1d"
         org-agenda-todo-ignore-scheduled t
         org-agenda-format-date (lambda (date) (concat "\n"
                                                       (make-string (window-width) 9472)
                                                       "\n"
                                                       ;; Does this load org?
                                                       (org-agenda-format-date-aligned date)))

         org-refile-use-outline-path t
         org-outline-path-complete-in-steps nil
         org-refile-targets '((("~/Documents/org/gtd.org")   :maxlevel . 1)
                              (("~/Documents/org/inbox.org")   :maxlevel . 2)
                              ("~/Documents/org/tickler.org"  :level . 1)
                              (("~/Documents/org/maybe.org")  :level . 1)
                              (("~/Documents/org/notes.org")   :maxlevel . 3)
                              (("~/Documents/org/research_notes.org")   :maxlevel . 2)
                              (("~/Documents/org/graveyard.org") :level . 1)))
  (setq! org-capture-templates
         '(("t" "Todo" entry (file "~/Documents/org/inbox.org")
            "* TODO %?%i\n%a\ncreated: %t\n")
           ("r" "research" entry (file "~/Documents/org/inbox.org")
            "* RSCH %?\n%i\n%a\ncreated: %t\n")
           ("i" "idea" entry (file "~/Documents/org/readinglist.org")
            "* IDEA %?\n%i\n%a\ncreated: %t\n")
           ("j" "Journal entry" entry (file+datetree "~/Documents/org/journal.org")
            ;; Call with C-u C-u interactive argument to insert inactive stamp
            "* %? \n%(funcall 'org-timestamp '(16) 't)"
            :empty-lines 1)
           ("M" "Email workflow")
           ("mf" "Follow Up" entry (file "~/Documents/org/inbox.org")
            "* TODO Follow up with %:fromname on %a\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i" :immediate-finish t)
           ("mt" "Action Required" entry (file "~/Documents/org/inbox.org")
            "* TODO %? \n:PROPERTIES:\n:REFERENCE: %a\n:END:\n%i")
           ("mr" "Read Later" entry (file"~/Documents/org/readinglist.org")
            "* READ %:subject\nSCHEDULED: %t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t))

         org-archive-location ".%s_archive::"

         org-file-apps (quote
                        ((auto-mode . emacs)
                         ("\\.m\\'" . default)
                         ("\\.?html?\\'" . /usr/bin/firefox)
                         ("\\.pdf\\'" . emacs)))


         org-export-with-drawers '(not "noex")
         org-structure-template-alist '(("a" . "export ascii")
                                        ("c" . "center")
                                        ("C" . "comment")
                                        ("e" . "equation")
                                        ("E" . "export")
                                        ("h" . "export html")
                                        ("l" . "export latex")
                                        ("q" . "quote")
                                        ("s" . "src")
                                        ("v" . "verse"))

         org-startup-with-latex-preview nil
         org-directory "~/Documents/org/"
         org-default-notes-file "~/Documents/org/notes.org"
         org-todo-keywords     '((sequence
                                  "TODO(t)"
                                  "IDEA"
                                  "WAIT(w)"
                                  "MAYBE(m)"
                                  "DRAFT(D)"
                                  "|"
                                  "DONE(d)"
                                  "CANCELLED(c)"))

         org-attach-id-dir "~/Documents/org/.attach/"
         org-latex-pdf-process (list "latexmk -f -pdf -%latex -shell-escape -interaction=nonstopmode -output-directory=%o %f")
         org-latex-default-packages-alist '(("" "amssymb" t)
                                            ("" "amsmath" t ("lualatex" "xetex"))
                                            ("" "fontspec" t ("lualatex" "xetex"))
                                            ("AUTO" "inputenc" t ("pdflatex"))
                                            ("T1" "fontenc" t ("pdflatex")))

         org-highlight-latex-and-related '(native)
         org-startup-folded t
         org-startup-with-inline-images nil
         org-fontify-whole-heading-line t
         org-fontify-done-headline t
         org-fontify-quote-and-verse-blocks nil
         org-ellipsis "  "
         org-image-actual-width 400
         org-hide-emphasis-markers t))

;; (defun my/org-edit-latex-if-appropriate ()
;;   (interactive)
;;   (cond ((texmathp-match-environment nil) (org-edit-latex-environment))
;;         ((texmathp) (org-edit-latex-fragment))
;;         (t nil)))
;; (add-hook! org-ctrl-c-ctrl-c #'my/org-edit-latex-if-appropriate)

;;; ** ox-hugo
(after! org
  (with-eval-after-load 'ox
    (defun my/org-ref-process-buffer--html (backend)
      "Preprocess `org-ref' citations to HTML format. Do this only if the export backend is `html' or a derivative of that."
      ;; `ox-hugo' is derived indirectly from `ox-html'.
      ;; ox-hugo <- ox-blackfriday <- ox-md <- ox-html
      (when (org-export-derived-backend-p backend 'html)
        (org-ref-process-buffer 'html)))))

;;; * bibtex
(after! org
  (setq! bibtex-autokey-year-length 4
         bibtex-autokey-name-year-separator "-"
         bibtex-autokey-year-title-separator "-"
         bibtex-autokey-titleword-separator "-"
         bibtex-autokey-titlewords 2
         bibtex-autokey-titlewords-stretch 1
         bibtex-autokey-titleword-length 5))



;;; ** `org-ql' helper
(require 'cl-lib)
(cl-defun org-ql-search-directories-files
    (&key (directories
           (if (file-exists-p org-directory)
               (list org-directory)
             (user-error org-ql-search-directories-files-error)))
          (recurse org-ql-search-directories-files-recursive)
          (regexp org-ql-search-directories-files-regexp))
  "Return list of matching files in DIRECTORIES.
When RECURSE is non-nil, recurse into subdirectories.  When
REGEXP is non-nil, only return files that match REGEXP."
  (let ((files (->> directories
                    (--map (f-files it nil recurse))
                    -flatten)))
    (if regexp
        (--select (string-match regexp it)
                  files)
      files)))


;;; ** `org-present'
(after! org
  (setq! org-present-hide-stars-in-headings t
         org-present-text-scale 4.5)

  (add-hook! org-present-mode
    (setq-local visual-fill-column-mode 1)
    (setq-local hl-line-mode nil)
    (org-present-hide-cursor)
    (org-display-inline-images)
    (setq-local spell-fu-mode nil)
    (hide-mode-line-mode))

  (add-hook! org-present-mode-quit
    (setq-local hl-line-mode 1)
    (setq-local visual-fill-column-mode nil)
    (org-present-show-cursor)
    (setq-local spell-fu-mode 1)
    (org-remove-inline-images)
    (setq! hide-mode-line-mode nil)))

;;; ** `org-super-agenda'
(after! org
  (setq! org-agenda-custom-commands
         '(("n" "Super view"
            ((agenda "" ((org-super-agenda-groups
                          '((:name "Due Today"
                             :deadline today
                             :order 0)
                            (:name "Todo"
                             :todo ("PROG" "WAIT" "NEXT")
                             :and (:todo "TODO" :scheduled today)
                             :and (:todo "TODO" :deadline today)
                             :habit t
                             :order)))))
             (alltodo ""
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
                          (:name "Maybe / To Read"
                           :todo ("IDEA" "READ" "MAYBE")
                           :order 10)

                          (:name "Unscheduled"
                           :children ("TODO" "DONE")
                           :and (:scheduled nil :deadline nil)
                           :order 6)
                          (:discard (:children nil))))))))))
  (org-super-agenda-mode))

;;; ** `org' specific `expand-region' functionality
(after! org
  (defun my/org-end-of-defun (&optional arg)
    (interactive "p")
    (if (not (texmathp))
        (if (not (org-at-heading-p)))
      (org-forward-element))
    (org-forward-element)
    (forward-char -1
                  (goto-char (min (save-mark-and-excursion
                                    (LaTeX-forward-environment (or arg 1))
                                    (point))
                                  (org-element-end (org-element-context))))))

  (defun er/add-latex-in-org-mode-expansions ()
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Better forward/backward defun in Org
    (setq-local beginning-of-defun-function 'my/org-beginning-of-defun)
    ;; Latex mode expansions
    (with-eval-after-load 'expand-region
      (set (make-local-variable 'er/try-expand-list)
           (append (cl-set-difference er/try-expand-list
                                      '(er/mark-method-call
                                        er/mark-inside-pairs
                                        er/mark-outside-pairs)
                                      '(LaTeX-mark-environment
                                        er/mark-LaTeX-inside-math
                                        er/mark-latex-inside-pairs
                                        er/mark-latex-outside-pairs)
                                      er/mark-LaTeX-math)))))

  (add-hook! org-mode #'er/add-latex-in-org-mode-expansions))

;;; ** `org-mode-hook' main
(add-hook! org-mode
           #'org-appear-mode
           #'variable-pitch-mode
           #'org-latex-preview-auto-mode
           #'turn-off-smartparens-mode
           (setq! display-line-numbers-mode nil
                  org-indent-mode nil
                  tab-width 8
                  smartparens-mode nil))

;;; ** `org' navigation transient
;; (transient-define-prefix org-nav-transient ()
;;   "Main transient for lasgun."
;;   [["Navigation"
;;     ("l" "Forward LaTeX" forward-latex-math :transient t)
;;     ("L" "Backward LaTeX" backward-latex-math :transient t)

;;     ("e" "Up Subtree" org-previous-visible-heading :transient t)
;;     ("n" "Down Subtree" org-next-visible-heading :transient t)

;;     ("f" "Next block" org-next-block :transient t)
;;     ("b" "Previous block" org-previous-block :transient t)
;;     ("i" "Search heading" consult-org-heading :transient nil)
;;     ("TAB" "Cycle" org-cycle :transient t)]

;;    ["Structure Editing"
;;     ("E" "Subtree up" org-move-subtree-up :transient t)
;;     ("N" "Subtree down" org-move-subtree-down :transient t)
;;     ("I" "Demote subtree" org-demote-subtree :transient t)
;;     ("M" "Promote subtree" org-promote-subtree :transient t)
;;     ("w" "Refile" org-refile :transient nil)]
;;    ["Quit"
;;     ("q" "Quit" transient-quit-one)
;;     ("C-g" "Quit" transient-quit-one)]])


(setq! rmh-elfeed-org-files '("~/Documents/org/notes.org"))

;;; ** `org' keybindings
(map! :map org-mode-map

      "M-f"                                           #'my/org-try-cdlatex-tab

      "M-m"                                        #'math-delimiters-insert

      :desc "Search"                 "C-c s q s"                #'org-ql-search
      :desc "Find"                 "C-c s q f"                  #'org-ql-find


      :desc "Insert bibliography link" "C-c ]"        #'org-cite-insert
      :desc "Insert cross reference" "C-c ["          #'org-ref-insert-ref-link

      :desc "Forward LaTeX math"   "C-c L f"          #'forward-latex-math
      :desc "Backward LaTeX math"  "C-c L b"          #'backward-latex-math
      :desc "Add note"             "C-c z"            #'org-add-note

      :desc "Make ink figure" "C-c i i"                                       #'ink-make-figure
      :desc "Make quiver (local)" "C-c i c l"                                     #'open-quiver-local
      :desc "Make quiver (online)" "C-c i c w"                                     #'open-quiver-web

      :desc "Org structure editing" "C-c t o"         #'org-nav-transient
      "M-TAB"                                         #'forward-latex-math
      "M-<iso-lefttab>"                                        #'backward-latex-math


      (:when (texmathp)
        "ESC"                                         #'org-try-cdlatex-tab))



;;; * `tempel'
;;; ** Basic setup
(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook! prog-mode #'tempel-setup-capf)
(add-hook! text-mode #'tempel-setup-capf)
(add-hook! org-mode #'tempel-setup-capf)


(after! tempel
  (setq! tempel-path (directory-files (concat doom-user-dir "templates") t "eld$")
         tempel-auto-reload t))

;;; ** `tempel' keybindings
(map! :map tempel-map
      "C-M-a"                                         #'tempel-beginning
      "C-M-e"                                         #'tempel-end
      "M-n"                                           #'tempel-next
      "M-e"                                           #'tempel-previous)


;;; * `org-roam'
;;; ** `Variables'
(after! org
  (setq! org-roam-directory "~/Documents/org/roam/"
         org-roam-dailies-directory "daily/"
         org-roam-node-display-template
         (concat "${title:*} "
                 (propertize "${tags:40}" 'face 'org-modern-tag))

         org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
         org-roam-capture-templates
         '(("h" "default" plain
            "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t)
           ("d" "definition" plain
            "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+filetags: :definition:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t)
           ("t" "test" plain
            "%?"
            :if-new (file+head "test/%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+filetags[[id:86ae361a-cc04-4e42-8ab7-2dc230f8f4a4][test of thing]]: :definition:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t)
           ("f" "fleeting" plain
            "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                               "#+title: ${title}\n#+filetags: :fleeting:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t)
           ("r" "reference" plain
            "%?"
            :if-new (file+head "${citar-citekey}.org"
                               "#+title: ${citar-author}: ${citar-title}\n#+filetags: :reference:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t)
           ("n" "reference notes" plain
            "%?"
            :if-new (file+head "${citar-citekey}.org" "#+title: ${citar-author}: ${citar-title}\n#+filetags: :reference:\n#+created: %U\n#+setupfile: ~/Documents/org/latex_template.org\n\n* Notes\n:PROPERTIES:\n:NOTER_DOCUMENT: ${citar-file}\n:END:\n") :unnarrowed t))
         org-roam-dailies-capture-templates
         '(("i" "default" entry
            "* %?"
            :target (file+head "%<%Y%m%d>.org"
                               "#+title: %<%Y-%m-%d>\n#+setupfile: ~/Documents/org/latex_template.org\n\n")
            :unnarrowed t))))

;;; ** `org-roam' interop with `org-ql'
(defun my/org-ql-find-roam-dailies ()
  (interactive)
  (funcall #'org-ql-search  (org-ql-search-directories-files :directories  (list (concat org-roam-directory org-roam-dailies-directory))
                                                             :recurse nil
                                                             :regexp ".*?\\.org$")
           (read-from-minibuffer "Query: ")))
(defun my/org-ql-find-roam ()
  (interactive)
  (funcall #'org-ql-search  (org-ql-search-directories-files :directories (list org-roam-directory)
                                                             :recurse nil
                                                             :regexp ".*\\.org")
           (read-from-minibuffer "Query: ")))


;;; ** `org-roam' keybindings
(map! :map global-map
      :leader
      "n r i" #'org-roam-node-insert
      "n r f" #'org-roam-node-find
      "n r s" #'org-roam-db-sync
      "n r w" #'org-roam-refile
      "n r d n" #'org-roam-dailies-capture-today
      "n r d y" #'org-roam-dailies-capture-yesterday
      "n r d t" #'org-roam-dailies-capture-tomorrow
      "n r d f" #'org-roam-dailies-goto-today)

(map! :map org-mode-map
      :desc "Search roam dailies"                 "C-c s q d"   #'my/org-ql-find-roam-dailies
      :desc "Search roam directory"                 "C-c s q r" #'my/org-ql-find-roam)


;;; * `citar'
;;; ** variables
(after! citar
  (setq! citar-bibliography '("~/Documents/bib/zotero_refs.bib")
         citar-org-roam-subdir "~/Documents/org/roam/"
         bibtex-completion-library-path '("~/Documents/books/" "~/Documents/bib/pdfs/")
         citar-org-roam-template-fields '((:citar-title "title")
                                          (:citar-author "author" "editor")
                                          (:citar-date "date" "year" "issued")
                                          (:citar-pages "pages")
                                          (:citar-type "=type=")
                                          (:citar-citekey "citekey")
                                          (:citar-file "file"))

         citar-org-roam-capture-template-key "n"))

;;; ** `citar' keybindings
(map! :map org-mode-map
      "C-c ]" #'citar-insert-citation

      :map citar-embark-map
      "p"     #'citar-org-update-prefix-suffix

      :map citar-embark-citation-map
      "p"     #'citar-org-update-prefix-suffix)

;;; * `pdf-view' mode
(add-hook! pdf-tools-enabled #'pdf-view-themed-minor-mode
           #'pdf-view-auto-slice-minor-mode)
;;; ** `pdf'  keybindings
(map! :map pdf-view-mode-map
      "M-m"                                           #'pdf-view-auto-slice-minor-mode
      "M-f"                                           #'pdf-view-themed-minor-mode
      "n"                                             #'pdf-view-next-line-or-next-page
      "e"                                             #'pdf-view-previous-line-or-previous-page)

;;; * `avy'
;;; * `avy' actions
(defun avy-action-easy-copy (pt)
  (unless (require 'easy-kill nil t)
    (user-error "Easy Kill not found, please install."))
  (goto-char pt)
  (cl-letf (((symbol-function 'easy-kill-activate-keymap)
             (lambda ()
               (let ((map (easy-kill-map)))
                 (set-transient-map
                  map
                  (lambda ()
                    ;; Prevent any error from activating the keymap forever.
                    (condition-case err
                        (or (and (not (easy-kill-exit-p this-command))
                                 (or (eq this-command
                                         (lookup-key map (this-single-command-keys)))
                                     (let ((cmd (key-binding
                                                 (this-single-command-keys) nil t)))
                                       (command-remapping cmd nil (list map)))))
                            (ignore
                             (easy-kill-destroy-candidate)
                             (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                               (easy-kill-save-candidate))))
                      (error (message "%s:%s" this-command (error-message-string err))
                             nil)))
                  (lambda ()
                    (let ((dat (ring-ref avy-ring 0)))
                      (select-frame-set-input-focus
                       (window-frame (cdr dat)))
                      (select-window (cdr dat))
                      (goto-char (car dat)))))))))
    (easy-kill)))

(defun avy-action-easy-kill (pt)
  (unless (require 'easy-kill nil t)
    (user-error "Easy Kill not found, please install."))
  (cl-letf* ((bounds (if (use-region-p)
                         (prog1 (cons (region-beginning) (region-end))
                           (deactivate-mark))
                       (bounds-of-thing-at-point 'sexp)))
             (transpose-map
              (define-keymap
                "M-t" (lambda () (interactive "*")
                        (pcase-let ((`(,beg . ,end) (easy-kill--bounds)))
                          (transpose-regions (car bounds) (cdr bounds) beg end
                                             'leave-markers)))))
             ((symbol-function 'easy-kill-activate-keymap)
              (lambda ()
                (let ((map (easy-kill-map)))
                  (set-transient-map
                   (make-composed-keymap transpose-map map)
                   (lambda ()
                     ;; Prevent any error from activating the keymap forever.
                     (condition-case err
                         (or (and (not (easy-kill-exit-p this-command))
                                  (or (eq this-command
                                          (lookup-key map (this-single-command-keys)))
                                      (let ((cmd (key-binding
                                                  (this-single-command-keys) nil t)))
                                        (command-remapping cmd nil (list map)))))
                             (ignore
                              (easy-kill-destroy-candidate)
                              (unless (or (easy-kill-get mark) (easy-kill-exit-p this-command))
                                (easy-kill-save-candidate))))
                       (error (message "%s:%s" this-command (error-message-string err))
                              nil)))
                   (lambda ()
                     (let ((dat (ring-ref avy-ring 0)))
                       (select-frame-set-input-focus
                        (window-frame (cdr dat)))
                       (select-window (cdr dat))
                       (goto-char (car dat)))))))))
    (goto-char pt)
    (easy-kill)))

(defun avy-action-exchange (pt)
  "Exchange sexp at PT with the one at point."
  (set-mark pt)
  (transpose-sexps 0))
(defun avy-action-helpful (pt)
  (save-excursion
    (goto-char pt)
    ;; (helpful-at-point)
    (my/describe-symbol-at-point))

  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)
(defun avy-action-define (pt)
  (cl-letf (((symbol-function 'keyboard-quit)
             #'abort-recursive-edit))
    (save-excursion
      (goto-char pt)
      (dictionary-search-dwim))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(defun avy-action-tuxi (pt)
  (cl-letf (((symbol-function 'keyboard-quit)
             #'abort-recursive-edit))
    (save-excursion
      (goto-char pt)
      (google-search-at-point))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(defun avy-action-embark (pt)
  (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
    (select-window
     (cdr (ring-ref avy-ring 0))))
  t)
(defun avy-action-kill-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-line))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)
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
(defun avy-action-kill-whole-line (pt)
  (save-excursion
    (goto-char pt)
    (kill-whole-line))
  (select-window
   (cdr
    (ring-ref avy-ring 0)))
  t)
(defun avy-action-yank-whole-line (pt)
  (avy-action-copy-whole-line pt)
  (save-excursion (yank))
  t)
(defun avy-action-teleport-whole-line (pt)
  (avy-action-kill-whole-line pt)
  (save-excursion (yank)) t)
(defun avy-action-mark-to-char (pt)
  (activate-mark)
  (goto-char pt))

(defun avy-action-push-mark-no-activate (pt)
  (push-mark-no-activate pt))

;;; ** `avy' variables
(setq! avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)
       avy-timeout-seconds 0.20
       avy-all-windows t
       avy-all-windows-alt nil

       avy-dispatch-alist '((?m . avy-action-mark)
                            (?. . avy-action-embark)
                            (?x . avy-action-exchange)
                            (?, . avy-action-push-mark-no-activate)

                            (?l . avy-action-kill-line)
                            (?Y . avy-action-yank-line)

                            (?w . avy-action-easy-kill)

                            (?k . avy-action-kill-stay)
                            (?y . avy-action-yank)
                            (?f . avy-action-teleport)

                            (?L . avy-action-copy-whole-line)
                            (?K . avy-action-kill-whole-line)
                            (?Y . avy-action-yank-whole-line)
                            (?T . avy-action-teleport-whole-line)))
;;; * `corfu'
;;; ** `corfu'  variables
(after! corfu
  (setq! corfu-cycle t
         corfu-auto t
         corfu-auto-prefix 2
         corfu-auto-delay 0.5
         corfu-quit-at-boundary 'separator
         corfu-preview-current 'insert))

(defun my/eshell-corfu-settings ()
  (setq-local corfu-quit-at-boundary nil
              corfu-quit-no-match t
              corfu-auto nil)
  (corfu-mode))

;;; ** `corfu' hook
(after! corfu
  ;;  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-tex))
;; (add-to-list 'completion-at-point-functions (cape-capf-super #'tempel-complete)

(add-hook! eshell-mode-hook #'my/eshell-corfu-settings)

;;; ** `corfu'  keybindings
(map! (:when (modulep! :completion corfu)
        :map corfu-map
        "M-SPC"                                       #'corfu-insert-separator
        "RET"                                         #'corfu-insert
        "TAB"                                         #'corfu-next
        "S-TAB"                                       #'corfu-previous))



;;; * `consult'
;;; ** `consult-dir'
(after! consult-dir
  (defun consult-dir-reference-pdfs ()
    '("~/Documents/bib/pdfs/" "~/Documents/books/"))

  (defvar consult-dir--source-references
    '(:name "References"
      :narrow ?z
      :category file
      :face consult-file
      :history file-name-history
      :items consult-dir-reference-pdfs)
    "Reference pdf direcories")
  (add-to-list 'consult-dir-sources 'consult-dir--source-references))


;;; * `lasgun'
(setq! lasgun-also-push-mark-ring t)

;;; ** `lasgun' actions
(defun lasgun-action-teleport-separated-sexps (ARG)
  "Kill words at lasgun selections and place them at point."
  (interactive "p")
  (let ((size (ring-length lasgun-mark-ring))
        (lasgun-list (ring-elements lasgun-mark-ring)))
    (save-excursion
      (dolist (pos lasgun-list)
        (goto-char pos)
        (kill-sexp ARG)))
    ;; killed words now in `kill-ring'
    (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
      (dotimes (i size)
        (insert (substring-no-properties (nth i kill-ring)))
        (unless (eq i (1- size))
          (insert separator)))))
  ;; Positions would be messed up if we didn't clear the ring
  (lasgun-clear-lasgun-mark-ring))

(defun lasgun-action-yank-separated-sexps (ARG)
  "Kill words at lasgun selections and place them at point."
  (interactive "p")
  (let ((size (ring-length lasgun-mark-ring))
        (lasgun-list (ring-elements lasgun-mark-ring)))
    (unwind-protect
        (save-excursion
          (dolist (pos lasgun-list)
            (let ((end nil))
              (goto-char pos)
              (forward-sexp ARG)
              (setq end (point))
              (kill-new (buffer-substring pos end)))))

      ;;  sexps now in `kill-ring'
      (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
        (dotimes (i size)
          (insert (substring-no-properties (nth i kill-ring)))
          (unless (eq i (1- size))
            (insert separator)))))
    (lasgun-clear-lasgun-mark-ring)
    (message "Error yanking sexps")))

(defun lasgun-action-cycleshift-sexps (ARG)
  (interactive)
  (let ((size (ring-length lasgun-mark-ring))
        (lasgun-list (ring-elements lasgun-mark-ring)))
    (unwind-protect
        (save-excursion
          (dolist (pos lasgun-list)
            (let ((end nil))
              (goto-char pos)
              (kill-sexp ARG)))))


    (message "Error yanking sexps")))

(defun lasgun-action-toggle-math-delims ()
  (interactive)
  (save-excursion
    (dolist (pos (ring-elements lasgun-mark-ring))
      (goto-char pos)
      (forward-latex-math)
      (math-delimiters-insert))))

(defun lasgun-action-helpful ()
  (interactive)
  (dolist (pos (ring-elements lasgun-mark-ring))
    (save-excursion
      (goto-char pos)
      (helpful-at-point)
      (popper--bury-all)))
  (lasgun-clear-lasgun-mark-ring))


(define-lasgun-action lasgun-action-upcase-word t upcase-word)
(define-lasgun-action lasgun-action-downcase-word t downcase-word)
(define-lasgun-action lasgun-action-kill-word nil kill-word)
(define-lasgun-action lasgun-action-kill-whole-line nil kill-whole-line)
(define-lasgun-action lasgun-action-comment-line t comment-line)
(define-lasgun-action lasgun-action-jinx-correct t jinx-correct-nearest)

;;; ** `embark' menu for `lasgun'
(after! embark
  (defun my-embark-lasgun-mark ()
    (unless (ring-empty-p lasgun-mark-ring)
      (let ((lgmark (ring-ref lasgun-mark-ring 0)))
        `(lasgun-mark  ,(buffer-substring-no-properties lgmark lgmark)
          ,lgmark . ,lgmark))))

  (add-to-list 'embark-target-finders #'my-embark-lasgun-mark)

  (defvar-keymap embark-lasgun-mark-actions
    :doc "Keymap for embark actions on lasgun targets")
  (add-to-list 'embark-keymap-alist '(lasgun-mark . embark-lasgun-mark-actions))

  (map! :map embark-lasgun-mark-actions
        :desc "Upcase word"   "U" #'lasgun-action-upcase-word
        :desc "Downcase word" "l" #'lasgun-action-downcase-word
        :desc "Make multiple cusors" "SPC" #'lasgun-make-multiple-cursors))


(defun my/avy-lg-mark-char-timer (ARG)
  (interactive "P")
  (if (equal ARG '(4))
      (lasgun-mark-char-timer)
    (avy-goto-char-timer)))


(defun lasgun-prompt-action ()
  (interactive)
  (let ((command (read-from-minibuffer "Command: ")))
    (unwind-protect
        (save-excursion
          (dolist (pos (ring-elements lasgun-mark-ring))
            (goto-char pos)
            (call-interactively (intern command) t)))
      (lasgun-clear-lasgun-mark-ring))))

;;; ** `transient' menu for `lasgun'
(transient-define-prefix lasgun-transient ()
  "Main transient for lasgun."
  [["Marks"
    ("c" "Char timer" lasgun-mark-char-timer :transient t)
    ;; ("w" "Word" lasgun-mark-word-0 :transient t)
    ;; ("l" "Begin of line" lasgun-mark-line :transient t)
    ("s" "Symbol" lasgun-mark-symbol-1 :transient t)
    ("n" "Whitespace end" lasgun-mark-whitespace-end :transient t)
    ("x" "Clear lasgun mark ring" lasgun-clear-lasgun-mark-ring :transient t)
    ("u" "Undo lasgun mark" lasgun-pop-lasgun-mark :transient t)]
   ["Actions"
    ("SPC" "Make cursors" lasgun-make-multiple-cursors)
    ("." "Embark act" lasgun-embark-act-all)
    ;; ("T" "Teleport sexps w/ delimiters" lasgun-action-teleport-separated-sexps :transient t)
    ;; ("h" "Help" lasgun-action-helpful :transient t)
    ;; ("Y" "Yank sexps w/ delimiters" lasgun-action-yank-separated-sexps :transient t)
    ;; ("$" "Jinx correct" lasgun-action-jinx-correct :transient t)
    ;; ("k" "Kill word" lasgun-action-kill-word :transient t)
    ;; ("U" "upcase word" lasgun-action-upcase-word :transient t)
    ("K" "Kill whole line" lasgun-action-kill-whole-line :transient t)
    ;; ("m" "Toggle math delims" lasgun-action-toggle-math-delims :transient t)
    (";" "Comment line" lasgun-action-comment-line :transient t)
    ("?" "Specify action" lasgun-prompt-action :transient t)
    ("q" "Quit" transient-quit-one)]])
(add-hook! transient-exit #'lasgun-clear-lasgun-mark-ring)

;;; * Email configuration: `mu4e'
(require 'setup-email)

;;; * TRAMP
(connection-local-set-profile-variables 'remote-path-with-local-cargo
                                        '((tramp-remote-path . ("~/.cargo/bin" tramp-default-remote-path))))
(connection-local-set-profiles nil 'remote-path-with-local-cargo)

;;; * `embark'
(setq! embark-confirm-act-all nil)


;;; * quiver
(defun open-quiver-local ()
  "Open quiver program locally"
  (interactive)
  (start-process "open-quiver" nil "firefox" "--new-window" "/home/aatmun/working/quiver/src/index.html"))

(defun open-quiver-web ()
  "Open quiver program on the web"
  (interactive)
  (start-process "open-quiver" nil "firefox" "--new-window" "https://q.uiver.app"))

;;; * `repeat-mode'
;;; ** window management repeat map
(define-repeat-map window-manage
  (:continue
   "o" other-window
   "0" delete-window
   "2" split-window-below
   "3" split-window-right
   "=" enlarge-window-horizontally
   "-" shrink-window-horizontally
   "}" enlarge-window
   "n" next-buffer
   "p" previous-buffer
   "{" shrink-window
   "u" consult-buffer
   "9" my/close-other-window)
  (:exit
   "f" find-file
   "r" recentf-open-files
   "B" bookmark-jump
   "T" eat
   "E" +eshell/here
   "d" consult-dir)
  (:enter split-window-right
          split-window-below))


;;; ** `org' heading navigation repeat map
(define-repeat-map org-navigation
  (:continue "n" org-next-visible-heading
             "p" org-previous-visible-heading
             "L" org-demote-subtree
             "H" org-promote-subtree
             "t" org-todo
             "l" org-set-property
             "d" org-deadline
             "s" org-schedule
             "e" org-edit-heading
             "v" consult-org-heading
             "N" org-narrow-to-subtree
             "W" widen
             "M-TAB" +org/close-all-folds
             "f" org-cycle
             "C-l" recenter-top-bottom
             "F" org-next-block
             "B" org-previous-block
             "w" org-refile
             "A" org-archive-subtree
             "J" org-move-subtree-down
             "K" org-move-subtree-up)
  (:enter org-next-visible-heading
          org-previous-visible-heading
          org-next-block
          org-previous-block))

;;; ** `latex' math navigation repeat map
(define-repeat-map latex-motion
  (:continue
   "f" forward-latex-math
   "b" backward-latex-math)
  (:enter forward-latex-math
          backward-latex-math))

;;; ** `flycheck' error repeat map
(define-repeat-map flycheck
  (:continue
   "n" flycheck-next-error
   "p" flycheck-previous-error
   "h" flycheck-display-error-at-point
   "e" flycheck-explain-error-at-point)
  (:enter flycheck-buffer)
  (:exit  "l" flycheck-list-errors))

;;; ** `multiple-cursor' repeat map
(define-repeat-map mc
  (:enter mc/mark-pop
          mc/mark-next-like-this
          mc/mark-previous-like-this)
  (:continue
   "," mc/mark-pop
   "." jump-to-mark
   "n" mc/mark-next-like-this
   "N" mc/unmark-next-like-this
   "p" mc/mark-previous-like-this
   "P" mc/unmark-previous-like-this))
(repeat-mode)

;;; * `multiple-cursors'
(after! multiple-cursors
  (setq! mc/always-run-for-all nil
         mc/always-repeat-command nil
         mc/cmds-to-run-once (delq #'org-self-insert-command mc/cmds-to-run-once))

  (add-to-list 'mc/cmds-to-run-for-all #'org-self-insert-command)

  (dolist (cmd '(mc/mark-next-like-this|mc-repeat-map
                 mc/mark-previous-like-this|mc-repeat-map
                 mc/unmark-next-like-this|mc-repeat-map
                 mc/unmark-previous-like-this|mc-repeat-map
                 mc/mark-pop|mc-repeat-map
                 jump-to-mark|mc-repeat-map))
    (add-to-list 'mc/cmds-to-run-once cmd)))
;;; * `haskell'
(setq! haskell-compile-command "ghc -Wall -ferror-spans -fforce-recomp -dynamic -c %s")
(add-to-list 'exec-path "/home/aatmun/.ghcup/bin")
(setq! eglot-workspace-configuration '((haskell (plugin (stan (globalOn . :json-false))))))

;; (add-hook! haskell-mode #'hindent-mode)
;; (add-hook 'haskell-mode-hook #'lsp-ui-mode)

;;; * eyecandy
;;; ** theme
(setq! doom-theme 'doom-rouge)
(setq! modus-themes-mixed-fonts t)
;;; ** global eyecandy

(mood-line-mode)
(spacious-padding-mode)
(setq! display-line-numbers-type nil)

(after! spacious-padding
  (setq! spacious-padding-widths
         '(:internal-border-width 15 :right-divider-width 10 :scroll-bar-width 0)))

;;; ** `org-modern'
(add-hook! org-agenda-finalize #'org-modern-agenda #'org-latex-preview-auto-mode)

(after! org
  (setq! org-modern-todo-faces
         '(("PROG" . (:inherit org-modern-todo))
           ("NEXT" .  (:inherit org-modern-todo))
           ("WAIT" . (:inherit org-modern-time-inactive))
           ("RSCH" . (:inherit org-modern-priority))
           ("PROJ" . (:inherit org-modern-todo))
           ("MAYBE" .  (:inhert org-modern-todo))))

  (custom-set-faces!
    '(org-level-1 :inherit outline-1 :height 2.0)
    '(org-level-2 :inherit outline-2 :height 1.8)
    '(org-level-3 :inherit outline-3 :height 1.5)
    '(org-level-4 :inherit outline-4 :height 1.2)
    '(org-level-5 :inherit outline-5 :height 1.0))

  (setq! org-modern-list '((43 . "➤")
                           (45 . "–")
                           (42 . "•"))
         org-modern-footnote (cons nil (cadr org-script-display))
         org-modern-block-name
         '((t . t)
           ("src" "»" "«")
           ("example" "»–" "–«")
           ("quote" "❝" "❞")
           ("export" "⏩" "⏪"))
         org-modern-progress nil
         org-modern-priority nil
         org-modern-horizontal-rule (make-string 36 ?─)
         org-modern-keyword
         '((t . t)
           ("title" . "𝙏")
           ("subtitle" . "𝙩")
           ("author" . "𝘼")
           ("email" . #("" 0 1 (display (raise -0.14))))
           ("date" . "𝘿")
           ("property" . "☸")
           ("options" . "⌥")
           ("startup" . "⏻")
           ("macro" . "𝓜")
           ("bind" . #("" 0 1 (display (raise -0.1))))
           ("bibliography" . "")
           ("print_bibliography" . #("" 0 1 (display (raise -0.1))))
           ("cite_export" . "⮭")
           ("print_glossary" . #("ᴬᶻ" 0 1 (display (raise -0.1))))
           ("glossary_sources" . #("" 0 1 (display (raise -0.14))))
           ("include" . "⇤")
           ("setupfile" . "⇚")
           ("html_head" . "🅷")
           ("html" . "🅗")
           ("latex_class" . "🄻")
           ("latex_class_options" . #("🄻" 1 2 (display (raise -0.14))))
           ("latex_header" . "🅻")
           ("latex_header_extra" . "🅻⁺")
           ("latex" . "🅛")
           ("beamer_theme" . "🄱")
           ("beamer_color_theme" . #("🄱" 1 2 (display (raise -0.12))))
           ("beamer_font_theme" . "🄱𝐀")
           ("beamer_header" . "🅱")
           ("beamer" . "🅑")
           ("attr_latex" . "🄛")
           ("attr_html" . "🄗")
           ("attr_org" . "⒪")
           ("call" . #("" 0 1 (display (raise -0.15))))
           ("name" . "⁍")
           ("header" . "›")
           ("caption" . "☰")
           ("results" . "⮞")))
  (global-org-modern-mode))

;;; ** `visual-fill-column-mode'
(setq! visual-fill-column-width 130
       visual-fill-column-center-text t)
;;; * personal stuff
(require 'setup-personal)

;;; * `elfeed' and `elfeed'
;;; ** `elfeed' helper functions
(defun elfeed-show-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))
    (elfeed-show-visit use-generic-p)))

(defun elfeed-search-eww-open (&optional use-generic-p)
  "open with eww"
  (interactive "P")
  (let ((browse-url-browser-function #'eww-browse-url))))

(defun elfeed-scroll-up-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-up-command arg)
      (error (elfeed-show-next)))))

(defun elfeed-scroll-down-command (&optional arg)
  "Scroll up or go to next feed item in Elfeed"
  (interactive "^P")
  (let ((scroll-error-top-bottom nil))
    (condition-case-unless-debug nil
        (scroll-down-command arg)
      (error (elfeed-show-prev)))))

(defun elfeed-tag-selection-as (mytag)
  "Returns a function that tags an elfeed entry or selection as
MYTAG"
  (lambda ()
    "Toggle a tag on an Elfeed search selection"
    (interactive)
    (elfeed-search-toggle-all mytag)))

;;; ** `elfeed-tube'
(after! elfeed
  (require 'elfeed-tube)
  (require 'elfeed-tube-mpv)
  (elfeed-tube-add-feeds '("LobosJr"
                           "Adam Ragusea"
                           "Ben Felix"
                           "VaatiVidya"
                           "James Hoffman"
                           "@KUN1234"
                           "Tobalog_tokyo | トバログ"
                           "LocoYun /ろこゆん"))

  (setq! elfeed-tube-captions-languages '("en" "jp" "english (auto generated)" "japanese (auto generated)"))
  (elfeed-tube-setup))

;;; ** `elfeed'  keybindings
(map! :map elfeed-show-mode-map
      "F" #'elfeed-tube-fetch
      "C-c C-f" #'elfeed-tube-mpv-follow-mode
      "B"      #'elfeed-show-eww-open
      "S-SPC"    #'elfeed-scroll-down-command
      "SPC"    #'elfeed-scroll-up-command

      :map elfeed-search-mode-map
      "F" #'elfeed-tube-fetch
      "C-c C-w" #'elfeed-tube-mpv-where
      "B"      #'elfeed-show-eww-open
      "d"      (elfeed-tag-selection-as 'junk)
      "l"      (elfeed-tag-selection-as 'readlater))

;;; * keybindings
;;; ** global keybindings
(when (featurep 'activities)
  (setq! edebug-inhibit-emacs-lisp-mode-bindings t))

(map! :map global-map
      (:when (modulep! :completion vertico)
        :desc "Buffer list"           "M-u"           #'consult-buffer
        :desc "Buffer other window"   "M-U"           #'my/switch-buffer-other-window
        :desc "Recent files"        "M-r"             #'consult-recent-file)

      :desc "Consult Dir"           "C-x C-d"         #'consult-dir

      :desc "Other window"          "M-o"             #'other-window
      :desc "Close window"          "M-0"             #'delete-window
      :desc "Close other window"    "M-9"             #'my/close-other-window
      :desc "Avy goto/Lasgun mark"         "M-n"             #'my/avy-lg-mark-char-timer
      :desc "Lasgun mark char timer"         "C-M-n"       #'lasgun-mark-char-timer
      :desc "Backward kill sexp"    "C-M-<backspace>" #'backward-kill-sexp

      "M-l"                                           #'move-to-window-line-top-bottom
      :desc "Goto line"            "M-g M-g"          #'avy-goto-line
      :desc "Hippie expand"        "M-/"            #'hippie-expand

      "C-."                                           #'embark-act
      "M-."                                           #'embark-dwim
      "C-h B"                                         #'embark-bindings
      "C-c o T"                                     #'eat
      "C-c o t"                                     #'eat




      "C-`"                                           #'popper-toggle
      "M-`"                                           #'popper-cycle
      "M-~"                                           #'popper-cycle-backwards
      "C-M-`"                                         #'popper-toggle-type


      "C-M-;"                                         #'better-jumper-set-jump
      "C-,"                                           #'push-mark-no-activate
      "M-,"                                           #'jump-to-mark
      "C-M-,"                                         #'consult-mark
      "C-;"                                           #'iedit-mode

      "C-M-SPC"                                           #'easy-mark
      "M-SPC"                                           #'easy-kill

      "M-*"                                           #'tempel-insert
      "C-<tab>"                                    #'tempel-expand


      :desc "Lasgun" "C-c t g"                        #'lasgun-transient


      :desc "Magit" "C-c g"                           #'magit


      (:when (modulep! :editor multiple-cursors)
        :desc "Mark extended"    "C-c m j"            #'mc/mark-more-like-this-extended
        :desc "Pop and jump"     "C-c m ,"            #'mc/mark-pop)

      (:when (modulep! :editor god)
        :desc "God Mode" "<escape>"                   #'god-mode-all)


      (:when (featurep 'activities)
        (:prefix-map ("C-x C-a" . "activities")
         :desc "Switch activity" "RET"                #'activities-switch
         :desc "New" "C-n"                            #'activities-new
         :desc "Define" "C-d"                         #'activities-define
         :desc "Kill" "C-k"                           #'activities-kill
         :desc "Suspend" "C-s"                        #'activities-suspend
         :desc "Resume activity" "C-a"                #'activities-resume
         :desc "List activities" "l"                  #'activities-list
         :desc "Switch to buffer with activity" "b"   #'activities-switch-buffer
         :desc "Revert state" "g"                     #'activities-revert)))


;;; ** `dired' keybindings
(map! :map dired-mode-map
      "C-c C-c"                                       #'casual-dired-tmenu)

;;; ** `easy-kill'  keybindings
(map! :map easy-kill-base-map
      "M-w"                                           #'copy-region-as-kill
      ","                                             #'easy-kill-expand-region
      "."                                             #'easy-kill-contract-region)

;;; ** `vertico' keybindings
(map! :map vertico-map
      "C-x C-j"                                       #'consult-dir-jump-file
      "C-x C-d"                                       #'consult-dir)
;;; ** `outline-minor-mode'  map
(map! :map outline-minor-mode-map
      "C-c s ," #'consult-outline)
