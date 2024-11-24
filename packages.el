;; -*- no-byte-compile: t; -*-
;;; $ Org related packages
(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
   (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
   :files
   (:defaults "etc")
   :build t :pre-build
   (with-temp-file "org-version.el"
     (require 'lisp-mnt)
     (let
         ((version
           (with-temp-buffer
             (insert-file-contents "lisp/org.el")
             (lm-header "version")))
          (git-version
           (string-trim
            (with-temp-buffer
              (call-process "git" nil t nil "rev-parse" "--short" "HEAD")
              (buffer-string)))))
       (insert
        (format "(defun org-release () \"The release version of Org.\" %S)\n" version)
        (format "(defun org-git-version () \"The truncate git commit hash of Org mode.\" %S)\n" git-version)
        "(provide 'org-version)\n"))))
  :pin nil)
(unpin! org)
(package! org-super-agenda)
(package! org-appear)
(package! org-modern)
(package! org-pdftools)
(package! org-ql)
(package! citar-org-roam :recipe (:host github :repo "emacs-citar/citar-org-roam"))
(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))
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
(package! math-delimiters :recipe (:host github
                                   :repo "oantolin/math-delimiters"))

;;; $ Theming and Appearance
(package! fontaine)
(package! mood-line)
(package! modus-themes)
(package! spacious-padding)
;; (package! catppuccin-theme)
(package! ef-themes)
(package! technicolor :recipe (:host github :repo "aatmunbaxi/technicolor"))
(package! stimmung-themes
  :recipe (:host github :repo "motform/stimmung-themes"))
(package! tao-themes
  :recipe (:host github :repo "11111000000/tao-theme-emacs"))
(package! emacs-miasma
  :recipe (:host github :repo "daut/miasma-theme.el"))


;;; $ Utilities
(package! indent-bars)
(package! hyperbole)
(package! consult-org-roam)
(package! lsp-booster
  :recipe (:host github :repo "blahgeek/emacs-lsp-booster"))
(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"))
(package! el-easydraw
  :recipe (:host github :repo "misohena/el-easydraw"))
(package! rainbow-mode)
(package! outli :recipe  (:host github :repo "jdtsmith/outli"))
(package! define-repeat-map :recipe (:host nil :repo "https://tildegit.org/acdw/define-repeat-map.el"))
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
(package! visual-fill-column)
(package! sage-shell-mode)
(package! activities)
;; (package! keycast :recipe (:host github :repo "tarsius/keycast"))
(package! no-littering)
(package! dogears)
(when (equal (system-name) "pop-os")
  (package! elffmpeg :recipe ( :local-repo "~/repos/elffmpeg")))
(package! engraved-faces :recipe (:host github :repo "tecosaur/engrave-faces"))
;; (package! benchmark-init)
(package! sudoku)

;;; $ Disable packages
(package! avy-migemo :disable t)

(when (modulep! :ui modeline)
  (package! mood-line :disable t))
(package! activities :disable t)
