;; -*- no-byte-compile: t; lexical-binding: t; -*-
;;; $ Org related packages
(package! org :recipe
  (:host nil :repo "https://git.tecosaur.net/mirrors/org-mode.git" :remote "mirror" :fork
   (:host nil :repo "https://git.tecosaur.net/tec/org-mode.git" :branch "dev" :remote "tecosaur")
   :files
   (:defaults "etc")
   :build t
   :pre-build
   (with-temp-file "org-version.el"
     (require 'lisp-mnt)
     (let ((version
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
(package! eldoc-box)
(package! org-msg :recipe (:host github :repo "jeremy-compostella/org-msg"))
(package! org-super-agenda)
(package! uniline :recipe
  (:host github :repo "tbanel/uniline"))
(package! org-chef)
(package! no-littering)
(package! org-appear)
(package! org-modern)
(package! org-web-tools)
(package! org-protocol-capture-html)
(package! org-pdftools)
(package! pdftotxt.el
  :recipe (:host github :repo "tecosaur/pdftotext.el"))
(package! org-ql)
(package! org-cv
  :recipe (:host gitlab
           :repo "Titan-C/org-cv"))
(package! doct)
(package! org-pomodoro)
(package! doric-themes :recipe
  (:host github
         :repo "protesilaos/doric-themes"))
;;; $ Editing packages
(package! super-save)
(package! tempel)
(package! org-transclusion)
(package! auto-activating-snippets :recipe
  (:host github
   :repo "ymarco/auto-activating-snippets"))
(package! lasgun :recipe (:host github :repo "aatmunbaxi/lasgun.el"))

(package! laas
  :recipe (:host github :repo "tecosaur/LaTeX-auto-activating-snippets"))
(package! easy-kill :recipe (:host github :repo "leoliu/easy-kill"))
(package! cdlatex)
(package! iedit)
(package! org-present)
(package! visual-fill-column)
(package! olivetti)
(package! math-delimiters :recipe (:host github
                                   :repo "oantolin/math-delimiters"))

(package! envrc)
;;; $ Theming and Appearance
(package! fontaine)
(package! mood-line)
(package! modus-themes)
(package! stimmung-themes)
(package! spacious-padding)
(package! ef-themes)
(package! technicolor :recipe (:host github :repo "aatmunbaxi/technicolor"))
(package! lambda-themes
  :recipe (:host github :repo "Lambda-Emacs/lambda-themes"))

;;; $ Utilities
(package! timeout
  :recipe (:host github :repo "karthink/timeout"))
(package! wttrin
  :recipe (:host github :repo "bcbcarl/emacs-wttrin"))
(package! org-node
  :recipe (:host github :repo "meedstrom/org-node"))
(package! lsp-booster
  :recipe (:host github :repo "blahgeek/emacs-lsp-booster"))
(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"))
(package! el-easydraw
  :recipe (:host github :repo "misohena/el-easydraw"))
(package! nov.el)
(package! rainbow-mode)
(package! outli :recipe  (:host github :repo "jdtsmith/outli"))
(package! gap-mode)
(package! popper :recipe (:host github :repo "karthink/popper"))

(package! age.el
  :recipe (:host github :repo "anticomputer/age.el"))

(package! eat :recipe
  (:host codeberg
   :repo "akib/emacs-eat"
   :files ("*.el" ("term" "term/*.el") "*.texi"
           "*.ti" ("terminfo/e" "terminfo/e/*")
           ("terminfo/65" "terminfo/65/*")
           ("integration" "integration/*")
           (:exclude ".dir-locals.el" "*-tests.el"))))
(package! sage-shell-mode)
(package! ob-sagemath)
(package! activities)

(package! jinx)
(package! reader
  :recipe (:host codeberg
           :repo "MonadicSheep/emacs-reader" ; Replace with the actual repo if different
           :files ("*.el" "*.so" "Makefile" "default.nix" "src")
           ;; :pre-build ( "nix-shell" "--run" "make" "all")
	   :pre-build (("bash" "-l" "-c" "nix-shell --run 'make all'"))
	   :build (:not compile)))
(package! ox-hugo)
(package! consult-dir)
(when (modulep! :email notmuch)
  (package! notmuch-indicator))

;; (package! keycast :recipe (:host github :repo "tarsius/keycast"))

(package! engrave-faces :recipe (:host github :repo "tecosaur/engrave-faces"))
(package! benchmark-init)

(package! nixos-options
  :recipe (:host github :repo "nix-community/nix-emacs"))


(package! posframe)
(package! repeat-help)
(package! popterm :recipe (:host github :repo "CsBigDataHub/popterm.el"))
(package! overleaf :recipe (:host github :repo "vale981/overleaf.el"))
;;; $ Disable packages
(unpin! emacs-everywhere)
(disable-packages! avy-migemo
                   anaconda-mode
                   org-noter
                   org-noter-nov
                   org-noter-djvu)

(when (modulep! :ui modeline)
  (package! mood-line :disable t))
