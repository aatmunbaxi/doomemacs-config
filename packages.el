;; -*- no-byte-compile: t; -*-

;;; $ Org related packages
(package! org :recipe
  (:host nil
   :repo "https://code.tecosaur.net/tec/org-mode"
   :branch "dev")
  :pin nil)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"
           :files (:defaults "extensions/*")))
(package! org-present :recipe (:host github :repo "rlister/org-present"))
(package! ox-hugo)
(package! ox-hugo)
(package! org-super-agenda)
(package! org-ref)
(package! org-appear)
(package! org-modern)
(package! org-pdftools)
(package! org-contrib :recipe (:host nil :repo "https://git.sr.ht/~bzg/org-contrib"
                               :files ("lisp/*.el")) :pin "6422b265f1150204f024e33d54f2dcfd8323005c")
(package! org-ql)
(package! org-transclusion)
(package! citar-org-roam :recipe (:host github :repo "emacs-citar/citar-org-roam"))

;;; $ Editing packages
(package! jinx)
(package! tempel)
(package! auto-activating-snippets :recipe
  (:host github
   :repo "ymarco/auto-activating-snippets"))
(package! lasgun :recipe (:host github :repo "aatmunbaxi/lasgun.el"))
(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
(package! easy-kill :recipe (:host github :repo "leoliu/easy-kill"))
(package! cdlatex)
(package! iedit)
(package! math-delimiters :recipe
  (:host github
   :repo "oantolin/math-delimiters"))

;;; $ Theming and Appearance
(package! fontaine)
(package! mood-line)
(package! modus-themes)
(package! standard-themes)
(package! spacious-padding)
(package! eldoc-box)

;;; $ Utilities
(package! vlf)
(package! consult-dir :recipe (:host github :repo "karthink/consult-dir"))
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
(package! casual-dired :recipe (:host github :repo "kickingvegas/casual-dired"))
(package! sage-shell-mode)
(package! hy-mode)
(package! pinentry)
(package! gap-mode)
(package! popper :recipe (:host github :repo "karthink/popper"))
(package! eat :recipe
  (:host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))
(package! activities)
(unpin! activities)
(package! elfeed-tube)
(package! elfeed-tube-mpv)
(package! keycast :recipe (:host github :repo "tarsius/keycast"))
;;; $ Disable packages
(package! avy-migemo :disable t)
