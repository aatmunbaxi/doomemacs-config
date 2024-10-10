prog-mode

(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

emacs-lisp-mode

(atload & ";;;###autoload")
(pt "(point)")
(vr "(defvar " p "\n  \"" p "\")")
(lcl "(defvar-local " p "\n  \"" p "\")")
(cnst "(defconst " p "\n  \"" p "\")")
(cstom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(fce "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(grp "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(mcr "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(als "(defalias '" p " '" p ")")
(fn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whnlt "(when-let (" p ")" n> r> ")")
(iflt* "(if-let* (" p ")" n> r> ")")
(whnlt* "(when-let* (" p ")" n> r> ")")
(andlt* "(and-let* (" p ")" n> r> ")")
(cnd "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(lt "(let (" p ")" n> r> ")")
(lt* "(let* (" p ")" n> r> ")")
(rc "(letrec (" p ")" n> r> ")")
(dtimes "(dotimes (" p ")" n> r> ")")
(dlist "(dolist (" p ")" n> r> ")")
(lp "(cl-loop for " p " in " p " do" n> r> ")")
(cmmnd "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advce "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
       "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(prvide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
        ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name))) " ends here" n)
(lg "(message \"LOG %s " (s sexp) ": %S"
    "\" (format-time-string \"%X\") " sexp ")")
(package-header ";;; " (p (file-name-base (or (buffer-file-name) (buffer-name))) file) ".el --- " (p "summary" summary) " -*- lexical-binding: t -*-" n n

                ";; Author: " (p (user-full-name)) n
                ";; Version: " (p "version") n
                ";; Package-Requires: " (p "dependencies") n
                ";; Homepage: " (p "homepage") n
                ";; Keywords: " (p "keywords") n n

                ";; This file is not part of GNU Emacs" n n

                (p (concat
                    ";; This program is free software: you can redistribute it and/or modify\n"
                    ";; it under the terms of the GNU General Public License as published by\n"
                    ";; the Free Software Foundation, either version 3 of the License, or\n"
                    ";; (at your option) any later version.\n\n"

                    ";; This program is distributed in the hope that it will be useful,\n"
                    ";; but WITHOUT ANY WARRANTY; without even the implied warranty of\n"
                    ";; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n"
                    ";; GNU General Public License for more details.\n\n"

                    ";; You should have received a copy of the GNU General Public License\n"
                    ";; along with this program.  If not, see <https://www.gnu.org/licenses/>."))

                n n

                ";;; Commentary:" n n

                ";; " (p "commentary") n n

                ";;; Code:" n n

                (p "(message \"Hello, World\")") n n

                "(provide '" (s file) ")" n
                ";;; " (s file) ".el ends here")




clojure-mode
(fun "(defn " (p "name" name)  n > "\""
     (p  (concat "FIXME: Document " )) (s name) "\"" n> "[" p  "]" n>  q ")")

(dotimes "(dotimes [" (p "var" ) " " (p "times") "]" n> q ")")
(doseq "(doseq [" p "]" n> q ")")

lisp-mode

(mcr "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(als "(defalias '" p " '" p ")")
(fn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whnlt "(when-let (" p ")" n> r> ")")
(iflt* "(if-let* (" p ")" n> r> ")")
(whnlt* "(when-let* (" p ")" n> r> ")")
(andlt* "(and-let* (" p ")" n> r> ")")
(cnd "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(lt "(let (" p ")" n> r> ")")
(lt* "(let* (" p ")" n> r> ")")
(lp "(loop :for " p " :in " p " :do" n> r> ")")
(cmmnd "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")


text-mode

(box "┌─" (make-string (length str) ?─) "─┐" n
     "│ " (s str)                       " │" n
     "└─" (make-string (length str) ?─) "─┘" n)


latex-mode org-mode :when (texmathp)

(op "\\oplus " r)
(bx "\\boxtimes " r)
(opr "\\operatorname{" r "}")
(im "\\operatorname{im} ")
(hom "\\operatorname*{Hom}" r)
(par "\\left( " r " \\right)" q)
(sqr "\\left[ " r " \\right]" q)
(angle "\\left\\langle " r " \\right\\rangle" q)
(sum "\\sum_{" p "}^{" p "}" r )
(curl "\\left\\{ " r " \\right\\}" q)
(over "\\overset{" (p "over") "}{" (p "lower") "}" q)
(bx "\\boxtimes")
(xrow "\\xrightarrow{" p "}")

(bigop (r (completing-read "Operator: "
                           '("\\bigoplus"
                             "\\bigotimes"
                             "\\bigcap"
                             "\\bigcup"
                             "\\bigsqcup"
                             "\\bigsqcup"
                             "\\bigstar"
                             "\\bigodot"
                             "\\bigvee"
                             "\\bigwedge"))) "_{" p "}^{" p "}")
(cfp "[" (p "\\mathcal{C}") " \\overset{" (p "G") "}{\\boxtimes} " (p "\\mathcal{D}") " ]_{" (p "B") "}" q)
(subr (r elem "x")  "_{" (p "1") "}" (p "separator" separator)
      " \\cdots "
      (s separator)  (s elem) "_{" (p "n") "}" q)

(set "\\left\\{ " (r "elems")  " \\mid " (p "restrictions") " \\right\\} " q)
(FR "\\operatorname{FR}^{" p "," p "}_{" p "}" q)
(frac "\\frac{" (r "numerator" num) "}{" (p "denominator" denom) "} " q )

org-mode :when (save-excursion (backward-sexp) (bolp))
(caption "#+caption: ")
(drawer ":" p ":" n r ":end:")
(begin "#+begin_" (s name) n> r> n "#+end_" name)
(quote "#+begin_quote" n> r> n "#+end_quote")
(sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
(marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
(example "#+begin_example" n> r> n "#+end_example")
(center "#+begin_center" n> r> n "#+end_center")
(ascii "#+begin_export ascii" n> r> n "#+end_export")
(html "#+begin_export html" n> r> n "#+end_export")
(latex "#+begin_export latex" n> r> n "#+end_export")
(comment "#+begin_comment" n> r> n "#+end_comment")
(verse "#+begin_verse" n> r> n "#+end_verse")
(src "#+begin_src " p n r n "#+end_src" )

(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(title "#+title: " p n "#+author: Aatmun Baxi" n "#+language: en")


;; regular org mode expansions
org-mode
(he "\\( " q " \\)" )
(sh "\\[ " n q n "\\]")


(beg "\\begin{" (p  (completing-read "Environment: " texmathp-environments nil nil) env nil) "}"
     n r n
     "\\end{" env "}")






;; Local Variables:
;; outline-regexp: "[a-z]"
;; major-mode: #'lisp-data-mode
;; End:
