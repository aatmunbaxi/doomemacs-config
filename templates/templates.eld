fundamental-mode
(date (format-time-string "%Y-%m-%d") r)
(time (format-time-string "%Y-%m-%d %I:%M:%S %p") r)

prog-mode
(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")




emacs-lisp-mode

(atload & ";;;###autoload")
(pt "(point)")
(var "(defvar " p "\n  \"" p "\")")
(local "(defvar-local " p "\n  \"" p "\")")
(const "(defconst " p "\n  \"" p "\")")
(custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
(fce "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
(grp "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(alias "(defalias '" p " '" p ")")
(fn "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(iflet "(if-let (" p ")" n> r> ")")
(whenlt "(when-let (" p ")" n> r> ")")
(iflet* "(if-let* (" p ")" n> r> ")")
(whenlet* "(when-let* (" p ")" n> r> ")")
(andlet* "(and-let* (" p ")" n> r> ")")
(cond "(cond" n "(" q "))" >)
(pcase "(pcase " (p "scrutinee") n "(" q "))" >)
(lt "(let (" p ")" n> r> ")")
(lt* "(let* (" p ")" n> r> ")")
(rc "(letrec (" p ")" n> r> ")")
(dtimes "(dotimes (" p ")" n> r> ")")
(dlist "(dolist (" p ")" n> r> ")")
(loop "(cl-loop for " p " in " p " do" n> r> ")")
(command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
(advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
       "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
(provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
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

(macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
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
(lrs "\\prescript{" (p "\\mathcal{P}") "}{"
     (p "\\mathcal{P}^{\\mathrm{rev}}}")
     (p "{\\boxtimes} "))
(o "\\circ" q)
(op "\\oplus " q)
(bx "\\boxtimes " q)
(opr "\\operatorname{" r "}")
(FF "\mathbb{F}")
(im "\\operatorname{im} ")
(hom "\\operatorname*{Hom}" r)
(FF "\\mathbb{F}" r)
(par "\\left( " r " \\right)" q)
(sqr "\\left[ " r " \\right]" q)
(angle "\\left\\langle " r " \\right\\rangle" q)
(sum "\\sum_{" p "}^{" p "}" q )
(curl "\\left\\{ " r " \\right\\}" q)
(over "\\overset{" (p "over") "}{" (p "lower") "}" q)
(under "\\underbrace{" (p "content") "}_{" (p "label") "}" q)
(bx "\\boxtimes")
(xrow "\\xrightarrow{" p "}")
(inv "^{-1}" q)
(id "\\operatorname{id}")
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
                             "\\bigwedge"))) "_{" p "}^{" p "}" q)
(cfp "[" (p "\\mathcal{C}") " \\overset{" (p "G") "}{\\boxtimes} " (p "\\mathcal{D}") " ]_{" (p "B") "}" q)
(pointed "\\mathcal{C}(" (p "A") "," (p "q") ")")
(fp  " \\overset{" (p "G") "}{\\boxtimes} ")
(ss "\\subset ")
(subr (r elem "x")  "_{" (p "1") "}" (p "separator" separator)
      " \\cdots "
      (s separator)  (s elem) "_{" (p "n") "}" q)

(set "\\left\\{ " (r "elems")  " \\mid " (p "restrictions") " \\right\\} " q)
(FR "\\operatorname{FR}^{" p "," p "}_{" p "}" q)
(frac "\\frac{" (r "numerator" num) "}{" (q "denominator" denom) "} " )
(psup "\\prescript{" p "}{" p "}{" q "}")

org-mode :when (save-excursion (backward-sexp) (bolp))
(transhead "#+transclude: [[" (p "file") "]] :src " (p "lang") " :heading \"" (r "heading") "\"" q
           :post (org-transclusion-add))
(newcell "#+ipynb-newcell")
(jupy "#+begin_src jupyter-python :exports both :session " (file-name-base)  r n q n "#+end_src" )
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
(src "#+begin_src " p n r n "#+end_src" :post (org-edit-src-code) )

(gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
(elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
(title "#+title: " p n "#+author: Aatmun Baxi" n "#+language: en")



nix-mode

(buildphase > "buildPhase= ''" n (p "Build Instructions") n " '';")
(checkPhase > "checkPhase= ''" n (p "") n " '';")
(configurephase > "configurePhase= ''" n (p "") n " '';")
(fixupphase > "fixupPhase= ''" n (p "") n " '';")
(distphase > "distPhase= ''" n (p "") n " '';")
(patchphase > "patchPhase= ''" n (p "") n " '';")
(unpackphase > "unpackPhase= ''" n (p "") n " '';")
(installCheckPhasephase > "installCheckPhasePhase= ''" n (p "") n " '';")
(installphase > "installphase= ''" n p " mkdir -p $out/bin" n> "for f in $(find . -executable -type f);" n> "do" n> "cp $f $out/bin" n> "done}" n> " '';")

<<<<<<< HEAD
(gitpackage "{ lib" n ", stdenv" n ", fetchFromGitHub" n ", " (p "inputs") n ", " (p "inputs") n "}:" n n>
            "stdenv.mkDerivation rec {" n> "pname = \"" (p "" pkgn nil) "\";" n> "version = \"" p "\";" n n>
            "src = fetchFromGitHub {" n> "owner = \"" (p "" own) "\";" n> "repo = \"" (s pkgn) "\";" n>
            "rev = \"" "v${version}" "\";" n> "sha256 = \"" "${lib.fakeSha256}" "\";" n> "};" n n>
            "nativeBuildInputs = [ " (p "makeWrapper") " ];" n n> "BuildInputs = [ " (p) " ];" n n>
            "meta = with lib; {" n>
            "homepage = \"" "https://github.com/" (s own) "/" (s pkgn) "\";" n>
            "description = \"" (p) "\";" n>
            "license = licenses." (p (completing-read "License: " '("agpl3" "asl20" "bsd1" "bsd2" "bsd3" "free" "gpl2" "gpl2Only" "gpl2Plus" "gpl3" "gpl3Only" "gpl3Plus" "isc" "lgpl21Only" "lgpl21Plus" "lgpl2Only" "lgpl2Plus" "lgpl3" "lgpl3Only" "mit" "mpl20" "ofl" "unfree"))) ";" n>
            "maintainers = with maintainers; [ " (s own) " ];" n>
            "platforms = platforms."
            (p (completing-read "Platform: " '("all" "allBut" "arm" "cygwin" "darwin" "freebsd" "gnu" "i686" "illumos" "linux" "mesaPlatforms" "mips" "netbsd" "none" "openbsd" "unix" "x86"))) ";" n> q "};" n> "}")
=======
nix-mode
(rustenv "let
  pkgs = import <nixpkgs> {};
in
with pkgs;
mkShell {
  buildInputs = [
    rustc
    rust-analyzer
    udev pkg-config # this is needed by cargo-embed
    clippy
    rustfmt
  ];
}")

(pkgmodule
 "{ config, pkgs, ... }:

{

	home.packages = with pkgs; [\n"
 r
	"\n];

  programs. = {
    enable = true;

  };

}
")
>>>>>>> 6b66c5d (Update)


;; Local Variables:
;; outline-regexp: "[a-z]"
;; End:
