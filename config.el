;; -*- lexical-binding: t; -*-
;;; Commentary
;;
;; Generally speaking, this configuration is motivated
;; by the tenet of "be as lazy as possible" while
;; not causing any load order errors and accommodating
;; my fickle decision-making about which modules
;; and packages to have active in `init.el' and
;; `packages.el'. Hence, you'll likely find many `after!'
;; blocks that refer to packages that aren't currently installed
;; or toggled in
(doom-load-packages-incrementally '(org
                                    magit
                                    recentf
                                    org-capture
                                    org-agenda
                                    org-modern
                                    embark
                                    consult))

;;; * Some functions
(after! emacs
  (setopt warning-minimum-level :error)
  (defun my/switch-buffer-other-window ()
    (interactive)
    (save-excursion
      (consult-buffer-other-window))))

;;; * Basic emacs stuff
(defun EVA-02-p ()
  (when (equal (system-name) "EVA-02") t))
(defun surfacep ()
  (when (equal (system-name) "fedora") t))


(when (or (EVA-02-p) (surfacep))
  (display-battery-mode))

(setopt display-time-format "%H:%M %d %b %Y")
(display-time-mode)



(global-display-line-numbers-mode -1)
(remove-hook! 'prog-mode-hook #'display-line-numbers-mode)
;;; * Setup load path.
(add-to-list 'load-path "~/.config/doom/")
(add-to-list 'load-path "~/.doom.d/")
(setq local-package-path (expand-file-name "lisp/" doom-user-dir))


;;; * Buffer related config
;;; ** activities
;; (activities-mode)
(map! :map global-map
      (:after activities
         (:prefix-map ("C-x C-a" . "activities")
          :desc "Switch activity"                       "RET"      #'activities-switch
          :desc "New"                                   "C-n"      #'activities-new
          :desc "Define"                                "C-d"      #'activities-define
          :desc "Kill"                                  "C-k"      #'activities-kill
          :desc "Suspend"                               "C-s"      #'activities-suspend
          :desc "Resume activity"                       "C-a"      #'activities-resume
          :desc "List activities"                       "l"        #'activities-list
          :desc "Switch to buffer with activity"        "b"        #'activities-switch-buffer
          :desc "Revert state"                          "g"        #'activities-revert)))

;;; ** dogears
;; (after! dogears)(setopt dogears-idle nil)
;; (dogears-mode)
;; (map!
;;  :desc "Dogears save"                         "M-g M-s"       #'dogears-remember
;;  :desc "Dogears forward"                      "M-g M-f"       #'dogears-forward
;;  :desc "Dogears backward"                     "M-g M-b"       #'dogears-back
;;  :desc "Dogears list"                         "M-g M-l"       #'dogears-list)

;;; * window management
(when (modulep! :ui window-select)
  (setopt aw-scope 'global))
;;; ** window bindings
(map! :map global-map
      :desc "Other window"                                  "M-o"                    #'other-window
      :desc "Move window top/bottom"                        "M-l"                    #'move-to-window-line-top-bottom)

;;; ** popper
(after! popper
    (setopt popper-reference-buffers
         '("\\*Messages\\*"
           "Output\\*$"
           "\\*compilation\\*"
           "\\*Async Shell Command\\*"
           "\\*cider-repl.*?"
           "\\*sly-description\\*"
           "\\*Flycheck errors\\*"
           "\\*Outline .*?\\*"
           "\\*org-roam\\*"
           pdf-outline-mode
           help-mode
           helpful-mode
           compilation-mode))
  (setopt popper-mode-line '(:eval (propertize " 儚 " 'face 'highlight))))

(map! (:after popper
       :map global-map
       "<escape>"                                      #'popper-toggle
       "C-<escape>"                                    #'popper-cycle
       "C-M-<escape>"                                  #'popper-toggle-type))
(popper-mode +1)
(popper-echo-mode)
;;; * eat
;;; ** eat bindings
(map! (:map global-map
       :desc "Open terminal"   "C-c o T"           #'eat
       :desc "Open terminal"   "C-c o t"           #'eat)
      (:map (eat-mode-map
             eat-line-mode-map)
            "M-o" #'other-window
            "M-u" #'consult-buffer)
      (:after julia-repl
       :map   julia-repl-mode-map
       "M-o" #'other-window
       "M-u" #'consult-buffer))

;;; * Editing
(setopt kill-whole-line t)
(add-hook! '(prog-mode-hook text-mode-hook) #'jinx-mode)

(map! :map global-map
      "M-/"          #'hippie-expand
      :map (prog-mode-map text-mode-map)
      "C-;"          #'iedit-mode)
;;; ** easy-kill bindings
(map! (:map global-map
            "C-M-SPC"                                       #'easy-mark
            "M-SPC"                                         #'easy-kill)

      (:after easy-kill
       :map easy-kill-base-map
       ","                                             #'easy-kill-expand-region
       "."                                             #'easy-kill-contract-region))
;;; ** easy-kill and expand-region interop
(after! easy-kill
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
  "Push point to local mark ring, without activating the region."
  (interactive)
  (push-mark (if pt
                 pt
               (point)) t nil)
  (message "Pushed point to mark ring"))

(defun jump-to-mark ()
  "Jump to local mark, respecting mark-ring order."
  (interactive)
  (set-mark-command 1))

(after! better-jumper
  (defvar my/inhibit-better-jumper-push nil
    "Prevent infinite recursion between `push-mark' and `better-jumper-set-jump'.")
  
  (defadvice! my/push-mark-to-better-jumper (&optional location &rest _)
    "Push better jumper list when mark ring pushed"
    :after #'push-mark
    (unless my/inhibit-better-jumper-push
      (let ((my/inhibit-better-jumper-push t)
            (pos (or location (point-marker))))
        (better-jumper-set-jump pos)))))

(map! :map global-map
      "C-,"                                         #'better-jumper-set-jump
      "M-,"                                         #'better-jumper-jump-backward
      "C-M-,"                                       #'better-jumper-jump-forward)
;;; ** Smartparens
(after! smartparens
  (add-hook! prog-mode-hook #'sp-use-smartparens-bindings)

  (map! :map smartparens-mode-map
        "C-<right>" #'sp-forward-slurp-sexp
        "C-<left>" #'sp-forward-barf-sexp
        "C-M-<right>" #'sp-backward-barf-sexp
        "C-M-<left>" #'sp-backward-slurp-sexp
        "M-D"        #'sp-unwrap-sexp))
;;; * Font config
(setq variable-font "EB Garamond"
      fixed-font "JetBrains Mono"
      variable-sans-serif "Rosario"
      doom-font fixed-font)

;;; ** Fontaine
(after! fontaine
  (setopt fontaine-presets
          `((regular-serif
             :variable-pitch-family ,variable-font
             :fixed-pitch-family ,fixed-font
             :default-height 110)
            (regular-sans
             :variable-pitch-family ,variable-sans-serif
             :fixed-pitch-family ,fixed-font
             :default-height 110)
            (office-monitor
             :inherit regular-sans
             :default-height 135)
            (medium-serif
             :inherit regular-serif
             :default-height 140)
            (medium-sans
             :inherit regular-sans
             :variable-pitch-weight light
             :default-height 140)
            (large-serif
             :inherit medium-serif
             :default-height 180)
            (large-sans
             :inherit medium-sans
             :default-height 180)
            (huge-serif
             :inherit medium-serif
             :default-height 210)
            (huge-sans
             :inherit medium-sans
             :default-height 210)
            (t ; our shared fallback properties
             :fixed-pitch-family ,fixed-font
             :fixed-pitch-height 1.0

             :variable-pitch-family ,variable-font
             :variable-pitch-weight regular
             :variable-pitch-height 1.0

             :fixed-pitch-serif-family ,fixed-font
             :fixed-pitch-serif-weight nil
             :fixed-pitch-serif-slant nil
             :fixed-pitch-serif-height 1.0

             :bold-family nil ; use whatever the underlying face has
             :bold-weight bold
             :italic-family nil
             :italic-slant italic
             :line-spacing nil)))
  (fontaine-set-preset 'medium-sans)
  (add-hook! enable-theme-functions #'fontaine-apply-current-preset))

(fontaine-mode)

(add-hook! 'text-mode-hook #'variable-pitch-mode)

;;; * LaTeX
(after! LaTeX
  (setopt TeX-engine 'xetex))

(after! (:and math-delimiters (:or LaTeX org))
  (setopt math-delimiters-compressed-display-math t))

(after! (:and (:or LaTeX org) bibtex)
  (setopt bibtex-dialect 'biblatex))
(after! (:and org laas)
  (add-hook! 'LaTeX-mode-hook
             #'laas-mode
             #'org-latex-preview-mode
             #'cdlatex-mode))

;;; ** Helper functions
(use-package! latex-utils
  :after (:and org latex)
  :load-path local-package-path)

(after! (:and laas (:or org latex))
  (setopt cdlatex-use-dollar-to-ensure-math nil)
  (defadvice! my/org-try-cdlatex-tab ()
    "Try cdlatex tab when in laas-mode"
    :override #'org-try-cdlatex-tab
    (when laas-mode
      (cond
       ;; Before any word on the line: No expansion possible.
       ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
       ;; Just after first word on the line: Expand it.  Make sure it
       ;; cannot happen on headlines, though.
       ((save-excursion
	  (skip-chars-backward "a-zA-Z0-9*")
	  (skip-chars-backward " \t")
	  (and (bolp) (not (org-at-heading-p))))
        (cdlatex-tab) t)
       ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t))))

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
          (forward-char -1))))))

(after! easy-kill
  (add-to-list 'easy-kill-try-things 'sexp))

;;; ** laas-mode for auto expanding snippets
(add-hook! 'org-mode-hook #'aas-mode #'laas-mode)
(use-package! aas
  :after (laas)
  :config
  (aas-set-snippets 'laas-mode
                    :cond #'laas-mathp
                    "opr" '(tempel "\\operatorname{" r "}" q)
                    "^" #'my/cdlatex-sub-superscript
                    "_" #'my/cdlatex-sub-superscript
                    "ox" "\\otimes"
                    "<=" "\\leqslant"
                    ">=" "\\geqslant"
                    "iso" "\\cong"
                    "hom" "\\hom"
                    "ker" "\\ker"
                    "ZZ" "\\mathbb{Z}"
                    "CC" "\\mathbb{C}"
                    "RR" "\\mathbb{R}"
                    "*" "\\ast"
                    "QQ" "\\mathbb{Q}"))

;;; ** LaTeX bindings
;;; * org-mode
(after! org
  (plist-put org-latex-preview-appearance-options
             :page-width 0.8)
  (add-hook 'org-latex-preview-auto-ignored-commands 'next-line)
  (add-hook 'org-latex-preview-auto-ignored-commands 'previous-line)
  (setopt org-latex-preview-numbered t
          org-latex-preview-live t )
  (custom-set-faces!
    '(org-level-1 :inherit outline-1 :height 1.7)
    '(org-level-2 :inherit outline-2 :height 1.5)
    '(org-level-3 :inherit outline-3 :height 1.3)
    '(org-level-4 :inherit outline-4 :height 1.2)
    '(org-level-5 :inherit outline-5 :height 1.1)))
(add-hook! org-agenda-mode-hook (setq-local line-spacing 0.35))

;;; ** org-agenda variables
(after! org-agenda
  (setopt  org-agenda-files '("~/Documents/org/inbox.org"
                             "~/Documents/org/gtd.org"
                             "~/Documents/org/tickler.org"
                             "~/Documents/org/graveyard.org"
                             "~/Documents/org/maybe.org")
          org-agenda-include-deadlines t
          org-agenda-use-time-grid nil
          org-agenda-block-separator nil
          org-agenda-compact-blocks t
          org-agenda-start-day nil ;; i.e. today
          org-agenda-span 5
          org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-todo-ignore-scheduled 'all))
;;; ** org-refile variables
(after! org-refile
  (setopt org-refile-use-outline-path 'file
          org-refile-targets '((("~/Documents/org/gtd.org")   :maxlevel . 2)
                               (("~/Documents/org/inbox.org")   :maxlevel . 2)
                               (("~/Documents/org/tickler.org")  :level . 1)
                               (("~/Documents/org/maybe.org")  :level . 1)
                               (("~/Documents/org/notes.org")   :maxlevel . 3)
                               (("~/Documents/org/braindump.org")   :maxlevel . 2)
                               (("~/Documents/org/graveyard.org") :level . 1)
                               (("~/Documents/org/journal.org") :maxlevel . 5))))
;;; ** org-refile variables
(after! org-capture
  (setopt org-capture-templates '(("n" "Info/IDEA")
                                  ("nn" "Info node"
                                   entry
                                   (file+headline "~/Documents/org/braindump.org" "Braindump")
                                   "* %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %(org-insert-time-stamp (current-time))\n:END:\n")
                                  ("nt" "Daily: Today"
                                   entry
                                   (file+olp+datetree "~/Documents/org/journal.org")
                                   "* %?\n:PROPERTIES:\n:ID: %(org-id-new)\n:END:\n"
                                   :unnarrowed nil)
                                  ("ni" "Daily: Idea"
                                   entry
                                   (file+olp+datetree "~/Documents/org/journal.org")
                                   "* IDEA %?  \n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %(org-insert-time-stamp (current-time))\n:END:\n")
                                  ("t" "Todo" entry (file "~/Documents/org/inbox.org")
                                   "* TODO %?%i\n:PROPERTIES:\n:ID:  %(org-id-new)\n:END:\n%a\n")
                                  ("r" "research" entry (file "~/Documents/org/inbox.org")
                                   "* RSCH %?\n%i\n:PROPERTIES:\n:ID:  %(org-id-new)\n:END:\n%a\n")
                                  ("i" "idea" entry (file "~/Documents/org/inbox.org")
                                   "* IDEA %?\n%i\n%a\n")
                                  ("j" "Journal entry" entry (file+olp+datetree "~/Documents/org/journal.org")
                                   ;; Call with C-u C-u interactive argument to insert inactive stamp
                                   "* %? \n%(funcall 'org-timestamp '(16) 't)"
                                   :empty-lines 1)
                                  ("m" "Email workflow")
                                  ("mf" "Follow Up" entry (file "~/Documents/org/inbox.org")
                                   "* TODO Follow up with %:fromname on %a :email:\n:PROPERTIES:\n:ID:  %(org-id-new)\n:END:\n%i"
                                   :immediate-finish t)
                                  ("mt" "Action Required" entry (file "~/Documents/org/inbox.org")
                                   "* TODO %? \n:PROPERTIES:\n:REFERENCE: %a\n:END:\n%i")
                                  ("mr" "Read Later" entry (file"~/Documents/org/inbox.org")
                                   "* TODO %:subject  :email:\n%a\n:PROPERTIES:\n:ID:  %(org-id-new)\n:END:\n%i"
                                   :immediate-finish t))))
;;; ** org variables
(after! org
  (setopt org-directory "~/Documents/org/"
          org-default-notes-file "~/Documents/org/notes.org"
   org-outline-path-complete-in-steps nil
          org-latex-src-block-backend 'engraved
          org-use-speed-commands t
          org-archive-location ".%s_archive::"
          org-file-apps (quote
                         ((auto-mode . emacs)
                          ("\\.m\\'" . default)
                          ("\\.?html?\\'" . (executable-find "zen"))
                          ("\\.pdf\\'" . emacs)))
          org-export-with-drawers '(not "noex")
          org-highlight-latex-and-related '(native latex)
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
          org-todo-keywords     '((sequence
                                   "TODO(t)"
                                   "IDEA(i)"
                                   "EVENT(e)"
                                   "WAIT(w)"
                                   "PROG(g)"
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
          org-startup-folded t
          org-startup-with-inline-images nil
          org-fontify-whole-heading-line t
          org-fontify-done-headline t
          org-fontify-quote-and-verse-blocks nil
          org-ellipsis "  "
          org-image-actual-width 400
          org-hide-emphasis-markers t
          org-latex-preview-preamble
          "\\documentclass{article}[DEFAULT-PACKAGES]
[PACKAGES]
\\usepackage{xcolor}
\\usepackage{amssymb}
\\usepackage{amsmath}"))

(map! (:when (modulep! :app calendar)
        :map org-agenda-mode-map
        :desc "Calendar" "C" #'=calendar))

;;;  bibtex
(after! (:or bibtex org latex)
  (setopt bibtex-autokey-year-length 4
         bibtex-autokey-name-year-separator "-"
         bibtex-autokey-year-title-separator "-"
         bibtex-autokey-titleword-separator "-"
         bibtex-autokey-titlewords 2
         bibtex-autokey-titlewords-stretch 1
         bibtex-autokey-titleword-length 5))

;;; ** ox-cv
(use-package! ox-awesomecv
  :after org
  :defer t)

;;; ** org-present
(after! org-present
  (setopt org-present-hide-stars-in-headings t
         org-present-text-scale 4.5)

  (add-hook! org-present-mode-hook
    (setq-local visual-fill-column-mode 1)
    (setq-local hl-line-mode nil)
    (org-present-hide-cursor)
    (org-display-inline-images)
    (setq-local spell-fu-mode nil)
    (hide-mode-line-mode))

  (add-hook! org-present-mode-quit-hook
    (setq-local hl-line-mode 1)
    (setq-local visual-fill-column-mode nil)
    (org-present-show-cursor)
    (setq-local spell-fu-mode 1)
    (org-remove-inline-images)
    (hide-mode-line-mode)))

;;; ** org-super-agenda
(after! org
  (setopt org-agenda-custom-commands
         '(("n" "Today's agenda"
            ((agenda "" ((org-super-agenda-groups
                          `((:discard (:file-path "graveyard"))
                            (:discard (:todo "MAYBE"))
                            (:name "Today"
                             :scheduled today
                             :face (:foreground ,(technicolor-get-color 'green) :extend t)
                             :order 2)
                            (:name "Due Today"
                             :face (:background ,(technicolor-relative-darken 'red 90) :extend t)
                             :deadline today
                             :order 1)
                            (:discard anything)))))
             (alltodo ""
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        `((:discard (:file-path "graveyard"))
                          (:discard (:file-path "maybe"))
                          (:discard (:todo "MAYBE"))
                          (:discard (:scheduled t))
                          (:discard (:deadline  t))
                          (:name "To Process"
                           :todo  ("EVENT" "TODO" "PROG" "WAIT")
                           :order 1
                           :face (:height 0.9
                                  :foreground ,(technicolor-relative-darken 'foreground 10)))
                          (:discard (:anything t))))))))
           ("w" "Week agenda"
            ((agenda "" ((org-super-agenda-groups
                          `((:discard (:file-path "graveyard"))
                            (:discard (:todo "MAYBE"))
                            (:discard (:file-path "inbox"))
                            (:auto-planning t)
                            (:auto-planning t)))))
             (alltodo ""
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        `((:discard (:file-path "graveyard"))
                          (:discard (:file-path "maybe"))
                          (:discard (:todo "MAYBE"))
                          (:discard (:scheduled t))
                          (:discard (:deadline  t))
                          (:name "Unscheduled"
                           :todo  ("EVENT" "TODO")
                           :order 0)
                          (:discard (:anything t))))))))
           ("d" "Get back to work!"
            ((alltodo ""
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        `((:discard (:file-path "graveyard"))
                          (:name "Maybe"
                           :todo "MAYBE"
                           :face (:foreground ,(technicolor-relative-darken 'foreground 70)
                                  :height 0.9
                                  :append t)
                           :order 100)
                          (:name "Important"
                           :priority "A"
                           :face (:foreground ,(technicolor-saturate 'red 20) :append t)
                           :order 1)
                          (:name "Ideas"
                           :todo "IDEA"
                           :face (:foreground ,(technicolor-get-color 'cyan)
                                  :height 0.9
                                  :append t)
                           :order 80)
                          (:name "Quick items"
                           :effort< "30"
                           :face (:foreground ,(technicolor-saturate 'blue 20) :append t))
                          (:auto-priority t)

                          (:order-multi (2 (:name "Research"
                                            :tag "research"
                                            :face (:foreground ,(technicolor-get-color 'cyan ) :append t))
                                           (:name "Teaching"
                                            :tag "teaching"
                                            :face (:foreground ,(technicolor-get-color 'green ) :append t))))))))))
           ("l" "Todos"
            ((alltodo ""
                      ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        `((:discard (:file-path "graveyard"))
                          (:discard (:file-path "maybe"))
                          (:auto-planning t)
                          (:name "Ideas"
                           :todo "IDEA"
                           :face (:foreground ,(technicolor-get-color 'cyan)
                                  :height 0.9
                                  :append t)
                           :order 80)
                          (:name "Important"
                           :priority "A"
                           :face (:foreground ,(technicolor-saturate 'red 20) :append t)
                           :order 1)
                          (:order-multi (2 (:name "Research"
                                            :tag "research"
                                            :face (:foreground ,(technicolor-get-color 'cyan ) :append t))
                                           (:name "Teaching"
                                            :tag "teaching"
                                            :face (:foreground ,(technicolor-get-color 'green ) :append t))))
                          (:name "Email"
                           :tag "email"
                           :order 20)
                          (:name "Personal"
                           :tag "personal"
                           :order 3)
                          (:auto-tags t
                           :order 50))))))))))
(after! org-agenda
  (org-super-agenda-mode))

;;; ** org specific expand-region functionality
(after! org
  (defun my/org-beginning-of-defun (&optional arg)
    ";TODO: "
    (interactive "p")
    (if (not (texmathp))
        (org-backward-element)
      (let ((lx (save-mark-and-excursion
                  (LaTeX-backward-environment arg)
                  (point)))
            (beg (org-element-begin (org-element-context))))
        (if (> beg lx) (goto-char beg)
          (run-at-time 0 nil #'goto-char lx)
          lx))))

  (defun my/org-end-of-defun (&optional arg)
    (interactive "p")
    (if (not (texmathp))
        (if (not (org-at-heading-p)))
      (org-forward-element))
    (org-forward-element)
    (forward-char -1)
    (goto-char (min (save-mark-and-excursion
                      (LaTeX-forward-environment (or arg 1))
                      (point))
                    (org-element-end (org-element-context))))))


;;; ** org-mode-hook main
(after! org
  (add-hook! 'org-mode-hook
             #'org-appear-mode
             #'org-latex-preview-mode
             (org-indent-mode -1)
             (display-line-numbers-mode 0)
             (org-modern-mode 1)))
(after! org-agenda
  (add-hook! 'org-agenda-finalize-hook
             #'org-modern-agenda
             #'org-latex-preview-mode))

;;; ** org keybindings
(map! :map global-map
      :desc "Org QL search"                  "C-c s q s"                 #'org-ql-search
      :desc "Org QL find"                    "C-c s q f"                 #'org-ql-find
      :desc "Org QL view"                    "C-c s q v"                 #'org-ql-view
      (:when (modulep! :lang org +roam2)
        :desc "org-roam-ql search"           "C-c n r q"    #'org-roam-ql-search)
      (:when (modulep! :completion vertico)
        :desc "Search org agenda"               "C-c s a"     #'consult-org-agenda))

(map! :map org-mode-map
      :desc "Math delim insert"              "M-m"                       #'math-delimiters-insert
      :desc "Insert bibliography link"       "C-c ]"                     #'org-cite-insert
      :desc "Forward LaTeX math"             "C-c L f"                   #'forward-latex-math
      :desc "Backward LaTeX math"            "C-c L b"                   #'backward-latex-math
      :desc "Add note"                       "C-c z"                     #'org-add-note
      :desc "Outline"                        "C-c s ,"                   #'consult-org-heading
      :desc "Make ink figure"                "C-c i i"                   #'ink-make-figure
      :desc "Make quiver (local)"            "C-c i c l"                 #'open-quiver-local
      :desc "Make quiver (online)"           "C-c i c w"                 #'open-quiver-web
      :desc "Forward LaTeX math"             "M-TAB"                     #'forward-latex-math
      :desc "Backward LaTeX math"            "M-<iso-lefttab>"           #'backward-latex-math)


;;; * org-export
;;; ** FIX: ox-hugo experimental org
(after! org
  (defun fixed?-org-html-format-latex (latex-frag processing-type info)
    "Format a LaTeX fragment LATEX-FRAG into HTML.
PROCESSING-TYPE designates the tool used for conversion.  It can
be `mathjax', `verbatim', `html', nil, t or symbols in
`org-preview-latex-process-alist', e.g., `dvipng', `dvisvgm' or
`imagemagick'.  See `org-html-with-latex' for more information.
INFO is a plist containing export properties."
    (let ((cache-relpath "") (cache-dir ""))
      (unless (or (eq processing-type 'mathjax)
                  (eq processing-type 'html))
        (let ((bfn (or (buffer-file-name)
		       (make-temp-name
		        (expand-file-name "latex" temporary-file-directory))))
	      (latex-header
	       (let ((header (plist-get info :latex-header)))
	         (and header
		      (concat (mapconcat
			       (lambda (line) (concat "#+LATEX_HEADER: " line))
			       (org-split-string header "\n")
			       "\n")
			      "\n")))))
	  (setq cache-relpath
	        (concat (file-name-as-directory org-preview-latex-image-directory)
		        (file-name-sans-extension
		         (file-name-nondirectory bfn)))
	        cache-dir (file-name-directory bfn))
	  (setq latex-frag (concat latex-header latex-frag))))
      (org-export-with-buffer-copy nil ;; <-- this `nil' is the only difference
                                   :to-buffer (get-buffer-create " *Org HTML Export LaTeX*")
                                   :drop-visibility t :drop-narrowing t :drop-contents t
                                   (erase-buffer)
                                   (insert latex-frag)
                                   (org-format-latex cache-relpath nil nil cache-dir nil
		                                     "Creating LaTeX Image..." nil processing-type)
                                   (buffer-string))))

  (advice-add #'fixed?-org-html-format-latex
              :override #'org-html-format-latex))
;;; * org-transclusion
(use-package! org-transclusion
  :after org
  :defer t
  :config
  (org-transclusion-set-extensions
   'org-transclusion-extensions
   (add-to-list 'org-transclusion-extensions 'org-transclusion-src-heading))
  (setopt org-transclusion-src-heading-level-capture-group my/outline-level-capture-group
          org-transclusion-src-heading-name-capture-group 2))

(use-package! org-transclusion-src-heading
  :load-path "~/code/org-transclusion-src-heading/"
  :after (org)
  :config)

;;; * expand-region
(after! expand-region
  (setopt expand-region-fast-keys-enabled nil)
  (defvar-keymap expand-region-repeat-map
    :repeat ((:enter er/expand-region
                     er/contract-region))
    "," #'er/expand-region
    "." #'er/contract-region)

  (advice-add 'er--first-invocation
              :override
              (defun my/er--first-invocation ()
                "t if this is the first invocation of er/expand-region or er/contract-region"
                (not (memq last-command
                           '(er/expand-region er/contract-region
                             easy-kill-expand-region easy-kill-contract-region)))))
  (add-to-list 'expand-region-exclude-text-mode-expansions 'org-mode)
  (add-to-list 'expand-region-exclude-text-mode-expansions 'latex-mode))

;;; ** org-mode and expand-region latex interop

(after! org
  (defun er/add-latex-in-org-mode-expansions ()
    (require 'expand-region)
    ;; Make Emacs recognize \ as an escape character in org
    (modify-syntax-entry ?\\ "\\" org-mode-syntax-table)
    ;; Paragraph end at end of math environment
    (setq paragraph-start (concat paragraph-start "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; (setq paragraph-separate (concat paragraph-separate "\\|\\\\end{\\([A-Za-z0-9*]+\\)}"))
    ;; Better forward/backward defun in Org
    (setq-local beginning-of-defun-function 'my/org-beginning-of-defun)
    ;; Latex mode expansions
    (set (make-local-variable 'er/try-expand-list)
         (append (cl-set-difference er/try-expand-list
                                    '(er/mark-method-call
                                      er/mark-inside-pairs
                                      er/mark-outside-pairs))
                 '(LaTeX-mark-environment
                   er/mark-LaTeX-inside-math
                   er/mark-latex-inside-pairs
                   er/mark-latex-outside-pairs
                   er/mark-LaTeX-math))))
  (add-hook! 'org-mode-hook #'er/add-latex-in-org-mode-expansions))


;;; * tempel
;;; ** Basic setup
(after! tempel
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to completion-at-point-functions.
    ;; tempel-expand only triggers on exact matches. Alternatively use
    ;; tempel-complete if you want to see all matches, but then you
    ;; should also configure tempel-trigger-prefix, such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; tempel-expand *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook! prog-mode-hook #'tempel-setup-capf)
  (add-hook! text-mode-hook #'tempel-setup-capf)

  (setopt tempel-path (directory-files (concat doom-user-dir "templates") t "eld$")
         tempel-auto-reload t))

;;; ** tempel keybindings
(map! :map tempel-map
      "M-n"                                           #'tempel-next
      "M-e"                                           #'tempel-previous
      "C-M-k"                                              #'tempel-abort
      
      :map global-map
       "M-*"                                           #'tempel-insert
       "C-<tab>"                                       #'tempel-expand)


;;; * org-node
(setopt org-mem-watch-dirs '("~/Documents/org/")
        org-mem-do-sync-with-org-id t
        org-node-creation-fn #'org-capture)
(after! org
  (org-mem-updater-mode))



;;; ** org-roam helper functions
;; This code is just  helper stuff that I wrote
;; to migrate my org-roam collection to a single big file
;; as opposed to many small files. Organizationally, I find
;; the latter preferable because the in-buffer search utilities
;; are superior, and I arbitrarily prefer fewer buffers
;; open at a time. You can safely delete this code.
(after! nil
  (defun org-roam-file-to-heading (file buff)
    "Moves content from single org-roam node FILE to a top
level heading in BUFF"
    (org-with-file-buffer file
      (let ((title (org-get-title))
            (tags (mapcar #'substring-no-properties (org-roam-node-tags (org-roam-node-at-point))))
            (props (cl-remove-if (lambda (x)  (member (car x) '("ALLTAGS" "FILE")))
                                 (org-roam-node-properties (org-roam-node-at-point))))
            (content (progn (goto-char (point-min))
                            (org-roam-end-of-meta-data t)
                            (s-replace-regexp
                             org-heading-regexp
                             "*\\1 \\2"
                             (buffer-substring-no-properties (point) (buffer-end 1))))))
        (message (format "Copying file: %s" file))
        (switch-to-buffer buff)
        (org-insert-heading nil t 1)
        (cl-loop for tag in tags
                 initially (insert title " :")
                 finally  (insert ":\n") do
                 (insert ":"  tag))
        (cl-loop for (prop . val) in props
                 finally (insert "\n" content) do
                 (org-set-property prop val)))))

  (defun org-roam-files-to-headings (buff)
    "Refactors all org-roam nodes in one-node-per-file
format to top level headlines in org buffer BUFF"
    (interactive)
    (cl-loop for file in (directory-files org-roam-directory t "\\.org$")
             do
             (my/org-roam-file-to-heading file buff))))

;;; ** Solving org-roam file ID annoyance
(after! org-roam
  (defun my/remove-file-level-org-ID ()
    "Removes file-level org ID property.
org-roam forces new ID creation at the file level
regardless of the type of capture template. I want to use
headlines as entries, hence the adding of this function
to the post-capture hook."
    (save-excursion
      (goto-char (point-min))
      (org-delete-property "ID")))
  (add-hook! org-roam-capture-new-node #'my/remove-file-level-org-ID))

;;; ** org-roam keybindings
(map! :map global-map
      :leader
      (:prefix-map ("n r" . "node")
       :desc "Insert node" "i" #'org-node-insert-link
       :desc "Find node" "f" #'org-node-find
       :desc "Refile node" "w" #'org-node-refile))

;;; * citar
;;; ** citar variables
(after! citar
  (setopt citar-bibliography '("~/Documents/bib/zotero_refs.bib")
         bibtex-completion-library-path '("~/Documents/pdfs/Articles" "~/Documents/pdfs/Reference Books/")))

;;; ** citar related keybindings
(defun my/citar-embark-update-prefix-suffix (cite)
  (citar-org-update-prefix-suffix nil))

(map! :map org-mode-map
:desc "Find node"         "C-c n r f"      #'org-node-find

:map global-map
:desc "Citar open"              "C-c ]"                             #'citar-open
:map citar-embark-map
:desc "Open entry"              "e"        #'citar-open-entry
:desc "Open files"              "f"        #'citar-open-files
:desc "Edit"                    "i"        #'citar-insert-edit
:desc "Open link"               "l"        #'citar-open-links
:desc "Open"                    "o"        #'citar-open
:desc "Copy reference"          "r"        #'citar-copy-reference
:desc "Add to node refs"        "k"        #'citar-org-roam-tag-headline

:map citar-embark-citation-map
:desc "Prefix/Suffix"           "p"        #'my/citar-embark-update-prefix-suffix
:desc "Open entry"              "e"        #'citar-open-entry
:desc "Open files"              "f"        #'citar-open-files
:desc "Edit"                    "i"        #'citar-insert-edit
:desc "Open link"               "l"        #'citar-open-links
:desc "Open notes"              "n"        #'citar-open-notes
:desc "Open"                    "o"        #'citar-open
:desc "Copy reference"          "r"        #'citar-copy-reference
:desc "Add to node refs"        "k"        #'citar-org-roam-tag-headline)

;;; * pdf-view mode
(add-hook! 'pdf-tools-enabled-hook #'pdf-view-themed-minor-mode
#'pdf-view-auto-slice-minor-mode)

;;; ** pdf  keybindings
(map! :map pdf-view-mode-map
      "M-m"                                           #'pdf-view-auto-slice-minor-mode
      "M-f"                                           #'pdf-view-themed-minor-mode
      "n"                                             #'pdf-view-next-line-or-next-page
      "e"                                             #'pdf-view-previous-line-or-previous-page)

;;; * avy
;;; ** avy functions
(use-package! avy-utils
  :after avy
  :load-path local-package-path)


;;; ** avy variables
(after! avy
  (setopt avy-keys '(?a ?r ?s ?t ?n ?e ?i ?o)
         avy-timeout-seconds 0.30
         avy-all-windows t
         avy-all-windows-alt nil

         avy-dispatch-alist '((?m . avy-action-mark)
                              (?. . avy-action-embark)
                              (?x . avy-action-exchange)
                              (?, . avy-action-push-mark-no-activate)
                              (?l . avy-action-kill-line)
                              (?Y . avy-action-yank-line)
                              (?k . avy-action-kill-stay)
                              (?y . avy-action-yank)
                              (?f . avy-action-teleport)
                              (?L . avy-action-copy-whole-line)
                              (?K . avy-action-kill-whole-line)
                              (?Y . avy-action-yank-whole-line)
                              (?T . avy-action-teleport-whole-line))))
;;; ** avy keybindings
(map! :map global-map
      (:prefix "M-g"
       :desc "Goto line"                              "M-g"          #'avy-goto-line
       :desc "Goto char"                              "i"            #'avy-goto-char)

       (:prefix "M-s"
        :desc "Copy line"                             "y"        #'avy-copy-line
        :desc "Goto isearch"                          "C-s"      #'avy-isearch
        :desc "Copy region"                           "M-y"      #'avy-copy-region
        :desc "Kill whole line"                       "M-k"      #'avy-kill-whole-line
        :desc "Goto line above"                       "M-p"      #'avy-goto-line-above
        :desc "Goto line below"                       "M-n"      #'avy-goto-line-below
        :desc "Kill region"                           "C-y"      #'avy-kill-region
        :desc "Kill region save region"               "M-w"      #'avy-kill-ring-save-region
        :desc "Move line"                             "t"        #'avy-move-line
        :desc "Move region"                           "M-t"      #'avy-move-region
        :desc "End of line"                           "M-t"      #'avy-goto-end-of-line))
;;; * corfu
;;; ** corfu  variables
(after! corfu
  (setopt corfu-cycle t
         corfu-auto t
         corfu-auto-prefix 2
         corfu-auto-delay 0.5
         corfu-quit-at-boundary 'separator
         corfu-preview-current 'insert)

  (defun my/eshell-corfu-settings ()
    (setq-local corfu-quit-at-boundary nil
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode)))

;;; ** corfu hook
(after! corfu
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions (cape-capf-super #'tempel-complete))

  (add-hook! eshell-mode-hook #'my/eshell-corfu-settings))

;;; ** corfu  keybindings
(map! (:when (modulep! :completion corfu)
        :map corfu-map
        "M-SPC"                                       #'corfu-insert-separator
        "RET"                                         #'corfu-insert
        "TAB"                                         #'corfu-next
        "S-TAB"                                       #'corfu-previous))


;;; * vertico
;;; ** vertico bindings
(map! (:after consult-dir
       :map vertico-map
              "C-x C-j"                                       #'consult-dir-jump-file
              "C-x C-d"                                       #'consult-dir))
;;; * consult
;;; ** consult-buffer sources
(after! (:and consult org-roam)
  (defvar org-source
    (list :name     "Org Buffer"
          :category 'buffer
          :narrow   ?o
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (org-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (consult--buffer-query :mode 'org-mode :as #'consult--buffer-pair))))
  ;; TODO: adapt this to use org-node
  (setopt consult--org-roam-nodes-source
         (list :name     "org nodes"
               :category 'org-heading
               :face 'org-roam-title
               :narrow   ?n
               :require-match nil
               :action (lambda (cand)
                         (let ((node-name (substring-no-properties cand 2)))
                           (progn
                             (org-roam-node-open (org-roam-node-from-title-or-alias
                                                  node-name t))
                             (when (org-at-heading-p)
                               (org-fold-show-entry t)
                               (recenter-top-bottom 0)))))
               :new (lambda (name)
                      (org-capture "nn")
                      (insert name))
               :items (lambda ()
                        (mapcar
                         (lambda (str)
                           (concat (nerd-icons-faicon "nf-fae-brain") " " str))
                         (org-roam--get-titles)))))
  (add-to-list 'consult-buffer-sources 'consult--org-roam-nodes-source 'append))

;;; ** consult-theme bug fix
;;; ** consult bindings
(map! :map global-map
            :desc "Buffer list"                                   "M-u"                    #'consult-buffer
            :desc "Buffer other window"                           "M-U"                    #'my/switch-buffer-other-window
            :desc "Consult Dir"                                   "C-x C-d"                #'consult-dir
            :desc "Consult mark"                                  "C-M-,"                  #'consult-mark)
;;; ** fix load-theme issues
(defadvice! my/consult-theme (theme)
  :override #'consult-theme
  (interactive
   (list
    (let* ((regexp (consult--regexp-filter
                    (mapcar (lambda (x) (if (stringp x) x (format "\\%s\\" x)))
                            consult-themes)))
           (avail-themes (seq-filter
                          (lambda (x) (string-match-p regexp (symbol-name x)))
                          (cons 'default (custom-available-themes))))
           (saved-theme (car custom-enabled-themes)))
      (consult--read
       (mapcar #'symbol-name avail-themes)
       :prompt "Theme: "
       :require-match t
       :category 'theme
       :history 'consult--theme-history
       :lookup (lambda (selected &rest _)
                 (setq selected (and selected (intern-soft selected)))
                 (or (and selected (car (memq selected avail-themes)))
                     saved-theme))
       :state (lambda (action theme)
                (pcase action
                  ('return (consult-theme (or theme saved-theme)))
                  ((and 'preview (guard theme)) (consult-theme theme))))
       :default (symbol-name (or saved-theme 'default))))))
  (when (eq theme 'default) (setq theme nil))
  (unless (eq theme (car custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (when theme
      (load-theme theme :no-confirm))))

;;; ** consult-dir
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
  (add-to-list 'consult-dir-sources 'consult-dir--source-references)

  (after! org
    (defun consult-dir-org-source ()
      `(,org-directory))

    (defvar consult-dir--source-org-dir
      '(:name "Org"
        :narrow ?o
        :category file
        :face consult-file
        :history file-name-history
        :items consult-dir-org-source)
      "org direcory")
    (add-to-list 'consult-dir-sources 'consult-dir--source-org-dir)))


;;; ** consult keybindings
;;; * lasgun
(use-package! lasgun
  :defer t
  :commands
  (lasgun-mark-char-timer
   lasgun-transient
   lasgun-mark-line
   lasgun-mark-char-2
   lasgun-mark-word-0
   lasgun-mark-symbol-1
   lasgun-mark-subword-0
   lasgun-mark-subword-1
   lasgun-mark-end-of-line
   lasgun-mark-char-2-above
   lasgun-mark-char-2-below
   lasgun-mark-word-0-above
   lasgun-mark-word-0-below
   lasgun-mark-symbol-1-above
   lasgun-mark-symbol-1-below
   lasgun-mark-whitespace-end
   lasgun-mark-whitespace-end-above
   lasgun-mark-whitespace-end-below)
  :config
  (setopt lasgun-also-push-mark-ring t
         lasgun-pop-before-make-multiple-cursors nil)
  (define-lasgun-action lasgun-action-upcase-word t upcase-word)
  (define-lasgun-action lasgun-action-downcase-word t downcase-word)
  (define-lasgun-action lasgun-action-kill-word nil kill-word)
  (define-lasgun-action lasgun-action-kill-whole-line nil kill-whole-line)
  (define-lasgun-action lasgun-action-comment-line t comment-line)
  (define-lasgun-action lasgun-action-jinx-correct t jinx-correct-nearest)

  (defun lasgun-action-helpful ()
    (interactive)
    (dolist (pos (ring-elements lasgun-mark-ring))
      (save-excursion
        (goto-char pos)
        (helpful-at-point)
        (popper--bury-all)))
    (lasgun-clear-lasgun-mark-ring))
  (defun lasgun-action-toggle-math-delims ()
    (interactive)
    (save-excursion
      (dolist (pos (ring-elements lasgun-mark-ring))
        (goto-char pos)
        (forward-latex-math)
        (math-delimiters-insert))))

  (defun lasgun-action-teleport-separated-sexps (ARG)
    "Kill words at lasgun selections and place them at point."
    (interactive "p")
    (let ((size (ring-length lasgun-mark-ring))
          (lasgun-list (ring-elements lasgun-mark-ring)))
      (save-excursion
        (dolist (pos lasgun-list)
          (goto-char pos)
          (kill-sexp ARG)))
      ;; killed words now in kill-ring
      (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
        (dotimes (i size)
          (insert (substring-no-properties (nth i kill-ring)))
          (unless (eq i (1- size))
            (insert separator)))))
    ;; Positions would be messed up if we didn't clear the ring
    (lasgun-clear-lasgun-mark-ring))

  (defun lasgun-action-copy-separated-sexps (ARG)
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

        ;;  sexps now in kill-ring
        (let ((separator (read-from-minibuffer "Separator: " nil nil nil nil " ")))
          (dotimes (i size)
            (insert (substring-no-properties (nth i kill-ring)))
            (unless (eq i (1- size))
              (insert separator)))))
      (lasgun-clear-lasgun-mark-ring)
      (message "Error yanking sexps")))
  (defun lasgun-prompt-action ()
    (interactive)
    (let ((command (read-from-minibuffer "Command: ")))
      (unwind-protect
          (save-excursion
            (dolist (pos (ring-elements lasgun-mark-ring))
              (goto-char pos)
              (call-interactively (intern command) t)))
        (lasgun-clear-lasgun-mark-ring))))

  (defun lasgun-action-pop-and-jump ()
    (interactive)
    (unless (ring-empty-p lasgun-mark-ring)
      (unless (ring-member lasgun-mark-ring (point))
        (push-mark-no-activate (point)))
      (goto-char (ring-ref lasgun-mark-ring 0))
      (ring-remove lasgun-mark-ring 0))
    (message "No lasgun marks")))

(after! (:and avy lasgun)
  (transient-define-prefix lasgun-transient ()
    "Main transient for lasgun."
    [["Marks"
      ("c" "Char timer" lasgun-mark-char-timer :transient t)
      ("l" "Begin of line" lasgun-mark-line :transient t)
      ("s" "Symbol" lasgun-mark-symbol-1 :transient t)
      ("x" "Clear lasgun mark ring" lasgun-clear-lasgun-mark-ring :transient t)
      ("u" "Undo lasgun mark" lasgun-pop-lasgun-mark :transient t)]
     ["Actions"
      ("SPC" "Make cursors" lasgun-make-multiple-cursors)
      ("." "Embark act" lasgun-embark-act-all)
      ("$" "Jinx correct" lasgun-action-jinx-correct :transient t)]
     ["" :description ""
      ("m" "Toggle math delims" lasgun-action-toggle-math-delims :transient t)
      (";" "Comment line" lasgun-action-comment-line :transient t)
      ("?" "Specify action" lasgun-prompt-action :transient t)]
     [""
      ("q" "Quit" transient-quit-one)]])
  (add-hook! transient-exit-hook #'lasgun-clear-lasgun-mark-ring))

;;; ** lasgun actions
(defun my/avy-lg-mark-char-timer (ARG)
  (interactive "P")
  (if (equal ARG '(4))
      (lasgun-mark-char-timer)
    (avy-goto-char-timer)))

;;; ** lasgun bindings
(map!
 "M-n"                    #'my/avy-lg-mark-char-timer
 :desc "Lasgun menu"          "C-c o g" #'lasgun-transient
 
 (:after lasgun
  :desc "Lasgun make multiple cursors"                          "M-g SPC"                #'lasgun-make-multiple-cursors
  :desc "Lasgun mark char timer"                        "M-g M-SPC"                  #'lasgun-mark-char-timer))

;;; * TRAMP-REMOTE-PATH
;; (connection-local-set-profile-variables 'remote-path-with-local-cargo
;;                                         '((tramp-remote-path . ("~/.cargo/bin" tramp-default-remote-path))))
;; (connection-local-set-profiles nil 'remote-path-with-local-cargo)

;;; * embark
(after! embark
  (setopt embark-confirm-act-all nil))
;;; ** embark bindings
(map! :map global-map
       "C-."                                           #'embark-act
       "M-."                                           #'embark-dwim
       "C-h B"                                         #'embark-bindings
       
       :map embark-file-map
       "r"                  #'find-file-read-only
       :map embark-general-map
       "C-."                 #'embark-cycle
       
       :map minibuffer-mode-map
       "C-c C-." #'embark-select)
;;; * quiver
(after! (:or org latex)
  (defun open-quiver-local ()
    "Open quiver program locally"
    (interactive)
    (start-process "open-quiver" nil "zen" "--new-window" "~/working/quiver/src/index.html"))

  (defun open-quiver-web ()
    "Open quiver program on the web"
    (interactive)
    (start-process "open-quiver" nil "zen" "--new-window" "https://q.uiver.app")))

;;; * repeat-mode
;;; ** structural lisp map
(after! (:and repeat smartparens)
  (defvar-keymap structural-editing-repeat-map
    :repeat (:enter (sp-copy-sexp sp-down-sexp
                                  sp-kill-sexp sp-mark-sexp
                                  sp-next-sexp sp-splice-sexp
                                  sp-unwrap-sexp sp-forward-sexp
                                  sp-backward-sexp sp-previous-sexp
                                  sp-transpose-sexp sp-backward-up-sexp
                                  sp-forward-barf-sexp sp-backward-barf-sexp
                                  sp-backward-down-sexp sp-forward-slurp-sexp
                                  sp-backward-slurp-sexp sp-backward-unwrap-sexp
                                  mark-sexp))
    "x"           #'exchange-point-and-mark
    "C-M-x"       #'pp-eval-last-sexp
    "\\"          #'indent-region
    "w"          #'sp-copy-sexp
    "d"          #'sp-down-sexp
    "k"          #'sp-kill-sexp
    "SPC"        #'sp-mark-sexp
    "n"          #'sp-next-sexp
    "M-D" #'sp-backward-unwrap-sexp
    "f"          #'sp-forward-sexp
    "b"          #'sp-backward-sexp
    "p"          #'sp-previous-sexp
    "t"          #'sp-transpose-sexp
    "u"          #'sp-backward-up-sexp
    "C-M-i"      #'sp-forward-barf-sexp
    "C-M-m"      #'sp-backward-barf-sexp
    "i"        #'sp-forward-slurp-sexp
    "m"        #'sp-backward-slurp-sexp
    "<right>" #'sp-forward-slurp-sexp
    "<left>" #'sp-forward-barf-sexp
    "M-<right>" #'sp-backward-slurp-sexp
    "M-<left>" #'sp-backward-barf-sexp))

;;; ** window management repeat map
(after! repeat
  (defvar-keymap window-manage-repeat-map
    :repeat (:enter (split-window-right
                     split-window-below
                     ace-swap-window
                     ace-window)
             :exit (find-file
                    recentf-open-files
                    bookmark-jump
                    eat
                    +eshell/here
                    consult-dir
                    consult-buffer
                    ibuffer))
    "o" #'other-window
    "0" #'delete-window
    "2" #'split-window-below
    "3" #'split-window-right
    "=" #'enlarge-window-horizontally
    "-" #'shrink-window-horizontally
    "}" #'enlarge-window
    "{" #'shrink-window
    "u" #'consult-buffer
    "s" #'ace-swap-window))


;;; ** flycheck error repeat map
(after! flycheck
  (when (modulep! :checkers syntax)
    (defvar-keymap flycheck-repeat-map
      :repeat (:enter (flycheck-buffer
                       flycheck-previous-error
                       flycheck-display-error-at-point
                       flycheck-explain-error-at-point)
               :exit (flycheck-list-errors))
      "n" #'flycheck-next-error
      "p" #'flycheck-previous-error
      "h" #'flycheck-display-error-at-point
      "e" #'flycheck-explain-error-at-point

      "l" #'flycheck-list-errors)))


;;; ** multiple-cursor repeat map
(when (modulep! :editor multiple-cursors)
  (defvar-keymap mc-repeat-map
    :repeat (:enter (mc/mark-pop
                     mc/mark-next-like-this
                     mc/mark-previous-like-this))
    "," #'mc/mark-pop
    "." #'jump-to-mark
    "n" #'mc/mark-next-like-this
    "N" #'mc/unmark-next-like-this
    "p" #'mc/mark-previous-like-this
    "P" #'mc/unmark-previous-like-this))

(after! repeat 
  (setopt repeat-help-popup-type 'which-key
          repeat-help-auto nil)
  (add-hook! 'repeat-mode-hook #'repeat-help-mode))

(add-hook! '(prog-mode-hook text-mode-hook) #'repeat-mode)
;;; * multiple-cursors
(after! multiple-cursors
  (setopt mc/always-run-for-all nil
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

;;; * Programming language configurations
;;; ** julia
(when (modulep! :lang julia +snail)
  (remove-hook! julia-mode #'julia-repl-mode)
  (add-hook! julia-mode-hook #'julia-snail-mode))

;; (add-to-list 'exec-path "~/.juliaup/bin")
;; (when (modulep! :lang julia +lsp)
;;   (setopt eglot-jl-language-server-project "~/.julia/environments/v1.10/"))

(after! julia-snail
  (setopt julia-snail-executable "~/.juliaup/bin/julia"
          julia-snail-extra-args "--threads auto"
          org-babel-julia-command "~/.juliaup/bin/julia"))
;;; ** common-lisp 
;;; *** petalisp indentation fixes
(after! cl-indent
  (put 'lazy 'common-lisp-indent-function '(1 &rest 1))
  (put 'lazy-reduce 'common-lisp-indent-function '(1 &rest 1))
  (put 'lazy-multiple-value 'common-lisp-indent-function '(1 1 &rest 1))
  (put 'lazy-reshape 'common-lisp-indent-function '(1 &rest 1)))


;;; ** outli
(add-hook! 'prog-mode-hook #'outli-mode)
(after! outline
  (setq my/outline-level-capture-group 1)
  (defun my/outline-level ()
    (if  (looking-at outline-regexp)
        (length (match-string my/outline-level-capture-group))
      0)))
;;; ** outli map
(map! :map outline-minor-mode-map
      "C-c s ,"                                       #'consult-outline)

;;; ** gap
(setopt gap-executable (executable-find "gap"))
;;; ** haskell
(after! haskell
  (setopt haskell-compile-command "ghc -Wall -ferror-spans -fforce-recomp -dynamic -c %s")
  (add-to-list 'exec-path "/home/aatmun/.ghcup/bin"))

(when (and (modulep! :haskell +lsp)
           (modulep! :tools +lsp))
  (setopt eglot-workspace-configuration '((haskell (plugin (stan (globalOn . :json-false)))))))

;;; * Eyecandy
;;; ** Theme
(setopt doom-theme 'modus-operandi-tinted
       modus-themes-mixed-fonts t)
;;; ** make theme consistent with qtile
(after! nil
  (defun my/current-theme-type ()
    "Return type of theme"
    (let ((theme (symbol-name (car custom-enabled-themes))))
      (intern (car (split-string theme "-")))))

  (defvar qtile-colors-to-export
    '(bg bg-alt grey
      red orange green
      teal yellow blue
      dark-blue magenta violet
      cyan dark-cyan fg-alt fg))


  (defun my/gen-doom-colors ()
    "Generates python-formatted doom colors"
    (let ((colors '(bg bg-alt grey
                    red orange green
                    teal yellow blue
                    dark-blue magenta violet
                    cyan dark-cyan fg-alt fg))

          (res "cs = {"))
      (cl-loop for color in colors do
               (setq res
                     (concat res
                             "\"" (symbol-name color) "\"" ": \""
                             (if (doom-color color)
                                 (doom-color color)
                               (doom-color 'bg)) "\",\n"))
               (setq res (concat res "}")))
      res))

  (setopt modus-to-universal-palette-translation
         '((bg-main . bg)
           (bg-dim . bg-alt)
           (border . grey)
           (red . red)
           (red-intense . orange)
           (green . green)
           (cyan-faint . teal)
           (yellow . yellow)
           (blue . blue)
           (blue-faint . dark-blue)
           (magenta-faint . magenta)
           (maroon . violet)
           (cyan . cyan)
           (cyan-faint . dark-cyan)
           (fg-alt . fg-alt)
           (fg-main . fg)))

  (defun my/gen-modus-colors ()
    "Generates python-formatted doom colors"
    (let (
          (colors '(bg-main bg-dim border
                    red red-intense green
                    cyan-faint yellow blue
                    blue-faint magenta-faint maroon
                    cyan cyan-faint fg-alt fg-main))
          (res "cs = {"))
      (cl-loop for color in colors do
               (setq res
                     (concat res
                             "\"" (symbol-name
                                   (alist-get color modus-to-universal-palette-translation)) "\"" ": \""
                             (modus-themes-get-color-value color) "\",\n")))
      (setq res (concat res "}"))
      res))

  (defun my/load-python-theme-colors (file)
    "Load current Emacs theme colors to FILE in python syntax"
    ;; workaround, doom does not set this variable when theme is switched-to
    ;; if it has already been loaded
    (setq doom-theme (car custom-enabled-themes))
    (let ((theme-type (my/current-theme-type)))
      (cond ((eql 'doom theme-type)
             (write-region (my/gen-doom-colors) nil  file nil))
            ((eql 'modus theme-type)
             (write-region (my/gen-modus-colors) nil file nil))
            (t (user-error (format "No way to convert colors in format %s" theme-type))))))


  (defvar qtile-colors-file "~/.config/qtile/colors.py")

  (defadvice! my/consult-theme-set-doom-theme (fn theme)
    :around #'consult-theme
    (setopt doom-theme theme)
    (funcall fn theme)))



;;; ** global eyecandy
(custom-set-faces! nil
  '(font-lock-keyword-face :inherit t :italic t)
  '(font-lock-operator-face :italic t)
  '(font-lock-doc-face :weight light))

(add-hook! '(prog-mode-hook
             text-mode-hook
             conf-mode-hook
             help-mode-hook)
           #'rainbow-mode)

(when (not (modulep! :ui modeline))
  (mood-line-mode)
  (setopt mood-line-format '((" "
                             (mood-line-segment-modal)
                             " "
                             (or
                              (mood-line-segment-buffer-status)
                              " ")
                             " "
                             (mood-line-segment-buffer-name)
                             "  "
                             (mood-line-segment-anzu)
                             "  "
                             (mood-line-segment-multiple-cursors)
                             "  ")
                            ((mood-line-segment-vc)
                             "  "
                             (mood-line-segment-major-mode)
                             "  "
                             (mood-line-segment-misc-info)
                             "  "
                             (mood-line-segment-checker)
                             "  "
                             (mood-line-segment-process)
                             "  " " "))))
(spacious-padding-mode)
(setopt spacious-padding-widths
       '(:internal-border-width 15 :right-divider-width 5 :scroll-bar-width 0))


;;; ** technicolor 
(defun technicolor-relative-darken (color alpha)
    (technicolor-blend 'background color alpha))
  (defun technicolor-relative-lighten (color alpha)
    (technicolor-blend 'foreground color alpha))
(after! emacs
  (setq prot-theme-mappings
         '((foreground . fg-main)
           (background . bg-main)
           (violet . magenta-cooler)
           (green . green-warmer)
           (teal . cyan-cooler)))
  (setq miasma-theme-mappings
         '((foreground . miasma-light-gray)
           (background . miasma-light-charcoal)
           (red . miasma-terracota)
           (violet . miasma-fire)
           (blue . miasma-river)
           (green . miasma-eucalyptus)
           (teal . miasma-moss)
           (cyan . miasma-sky)))
  (after! ef-themes
    (defun my/get-ef-theme-color (color)
      (if-let* ((curr-theme doom-theme)
                (palette-name (intern (format "%s-palette" curr-theme)))
                (pos (cl-position color (mapcar #'car (symbol-value palette-name)))))
          (cadr (nth pos (symbol-value palette-name)))
        'unspecified)))

  (setopt technicolor-colors '(foreground background
                              red blue
                              green magenta
                              violet teal
                              cyan)

         technicolor-themes `(,technicolor-doom-themes-data
                              ("^modus-.*" modus-themes-get-color-value
                               ,prot-theme-mappings)

                              ("^ef-.*" my/get-ef-theme-color
                               ,prot-theme-mappings)

                              ("^catppuccin" technicolor--get-catppuccin-color
                               ((foreground . text)
                                (background . base)
                                (magenta . pink)
                                (violet . mauve)
                                (cyan . sky)))
                              ("miasma" miasma-theme-get-color
                               ,miasma-theme-mappings)))

  (setq technicolor-org-src-block-faces '(("julia"      (technicolor-relative-darken  'magenta 90))
                                          ("python"     (technicolor-relative-darken  'teal 85))
                                          ("go"         (technicolor-relative-darken  'cyan 90))
                                          ("lisp"       (technicolor-relative-darken  'green 90))
                                          ("emacs-lisp" (technicolor-relative-darken  'magenta 85))
                                          ("rust"       (technicolor-relative-darken  'red 80))
                                          ("sh"         (technicolor-relative-darken  'green 85))))

  (defun my/technicolor-update-org-src-block-faces ()
    "Update org-src-block-faces list"
    (progn
      (setq org-src-block-faces
            (cl-loop for cell in technicolor-org-src-block-faces collect
                     `(,(car cell) (:background ,(eval (nth 1 cell)) :extend t))))
      (when (equal major-mode #'org-mode)
        (font-lock-fontify-buffer t))))
  (my/technicolor-update-org-src-block-faces)

  (defadvice! my/technicolor-reload-org-src-block-faces (THEME &optional NO-CONFIRM NO-ENABLE)
    "Apply org src block customizations from technicolor"
    :after #'load-theme
    (my/technicolor-update-org-src-block-faces)))

;;; *** customizing general faces
(custom-theme-set-faces! nil
  `(region :background ,(technicolor-blend 'background 'violet 60))
  `(font-lock-keyword-face :slant italic)
  `(font-latex-math-face :slant normal :foreground ,(technicolor-blend 'foreground 'violet 20)))

;;; *** org-modern face customization
(after! technicolor
  (setopt org-modern-block-fringe nil)
  (defface org-modern-idea `((t :inherit org-modern-todo :foreground ,(technicolor-relative-lighten 'cyan 10 )))
    "Face for org modern IDEA tag")
  (defface org-modern-draft `((t :inherit org-modern-todo :foreground ,(technicolor-lighten 'cyan 10)))
    "Face for org modern IDEA tag")
  (defface org-modern-event `((t :inherit org-modern-wait :foreground ,(technicolor-lighten 'red 10)))
    "Face for org modern IDEA tag")
  (defface org-modern-wait `((t :inherit org-modern-todo :foreground ,(technicolor-get-color 'red)))
    "Face for org modern WAIT tag")
  (defface org-modern-prog `((t :inherit org-modern-todo :foreground ,(technicolor-relative-lighten  'green 20)))
    "Face for org modern PROG tag")
  (defface org-modern-maybe `((t :inherit org-modern-todo :foreground ,(technicolor-relative-darken 'green 60)))
    "Face for org modern MAYBE tag"))

(after! technicolor
  (custom-theme-set-faces! nil
    `(org-super-agenda-header   :foreground ,(technicolor-get-color 'blue)
      :background unspecified :box nil :height 1.0)
    
    `(org-agenda-date   :foreground ,(technicolor-get-color 'foreground)
      :background unspecified :box nil :underline nil :height 1.1)
    
    `(org-agenda-date-weekend   :foreground ,(technicolor-blend 'foreground 'background 50)
      :background unspecified :box nil :underline nil :height unspecified)
    
    `(org-agenda-date-weekend-today   :foreground ,(technicolor-blend 'foreground 'background 50)
      :background unspecified :box t :height unspecified)
    
    `(org-modern-idea  :foreground ,(technicolor-get-color 'background) :background ,(technicolor-lighten 'cyan 10))
    `(org-modern-todo  :foreground ,(technicolor-get-color 'background)
      :background ,(technicolor-blend 'background 'green 10))
    `(org-modern-draft  :foreground ,(technicolor-lighten 'cyan 10))
    `(org-modern-wait  :foreground ,(technicolor-blend 'foreground 'red 20))
    `(org-modern-maybe  :background ,(technicolor-blend 'background 'green 70))
    `(org-modern-prog  :foreground ,(technicolor-blend 'foreground 'green 10)
      :background unspecified)
    
    `(org-modern-time-inactive  :foreground ,(technicolor-blend 'background 'green 20))
    `(org-modern-date-inactive  :inherit 'org-modern-label
      :background ,(technicolor-blend 'background 'red 95)
      :foreground ,(technicolor-saturate (technicolor-blend 'foreground 'background 80) 20))
    `(org-modern-date-inactive  :inherit 'org-modern-label
      :background ,(technicolor-saturate (technicolor-blend 'background 'blue 95) 20)
      :foreground ,(technicolor-blend 'foreground 'background 100))
    `(org-modern-time-active  :inherit 'org-modern-label
      :background ,(technicolor-blend 'background 'green 85)
      :foreground ,(technicolor-blend 'foreground 'background 90))
    `(org-modern-date-active  :inherit 'org-modern-label :
      background ,(technicolor-blend 'background 'blue 85)
      :foreground ,(technicolor-blend 'foreground 'background 90))))

;;; ** org-modern glyphs
(after! org-modern
  (setopt org-modern-list '((43 . "➤")
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

  (setopt org-modern-todo-faces
          `(("IDEA" . org-modern-idea)
            ("EVENT" . org-modern-event)
            ("TODO" . org-modern-todo)
            ("WAIT" . org-modern-wait)
            ("PROG" . org-modern-prog)
            ("MAYBE" . org-modern-maybe)
            ("DRAFT" . org-modern-draft))))

;;; ** visual-fill-column-mode
;; (after! visual-fill-column
;;   (setopt visual-fill-column-width 130
;;          visual-fill-column-center-text t))

;; (add-hook! (text-mode-hook prog-mode-hook) #'visual-fill-column-mode)

;;; * wttr
(setopt wttrin-default-cities '("College Station" "Colleyville"))
;;; ** wttr hacky fix
;; Fix: https://github.com/bcbcarl/emacs-wttrin/issues/16#issuecomment-658987903
(defadvice! wwtrin-fetch-raw-string (query)
  "Make sure we fetch the acutual weather view"
  :override #'wttrin-fetch-raw-string
  (let ((url-user-agent "curl"))
    (add-to-list 'url-request-extra-headers wttrin-default-accept-language)
    (with-current-buffer
        (url-retrieve-synchronously
         (concat "http://wttr.in/" query "?A")
         (lambda (status) (switch-to-buffer (current-buffer))))
      (decode-coding-string (buffer-string) 'utf-8))))

;;; * elfeed and elfeed-tube
(setopt rmh-elfeed-org-files '("notes.org"))

;;; ** elfeed helper functions
(after! elfeed
  (defun elfeed-show-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((browse-url-browser-function #'eww-browse-url))
      (elfeed-show-visit use-generic-p)))

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
      (elfeed-search-toggle-all mytag))))
;;; ** elfeed keybindings
(after! elfeed
  (map! :map elfeed-show-mode-map
        "F"       #'elfeed-tube-fetch
        "C-c C-f" #'elfeed-tube-mpv-follow-mode
        "B"       #'elfeed-show-eww-open
        "S-SPC"   #'elfeed-scroll-down-command
        "SPC"     #'elfeed-scroll-up-command

        :map elfeed-search-mode-map
        "F"       #'elfeed-tube-fetch
        "C-c C-w" #'elfeed-tube-mpv-where
        "B"       #'elfeed-show-eww-open
        "d"      (elfeed-tag-selection-as 'junk)
        "l"      (elfeed-tag-selection-as 'readlater)))


;;; * email
(when (EVA-02-p)
  (require 'setup-email))


;; (when init-file-debug
;;   (use-package! benchmark-init
;;     :ensure t
;;     :config
;;     ;; To disable collection of benchmark data after init is done.
;;     (add-hook 'after-init-hook 'benchmark-init/deactivate)))

;; Local Variables:
;; outline-regexp: ";;; \\(\\*+\\) \\(.*\\)$"
;; End:
