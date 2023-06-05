;;; doom-onehalf-dark-theme.el
(require 'doom-themes)

;;
(defgroup doom-onehalf-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-onehalf-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-onehalf-dark-theme
  :type 'boolean)

(defcustom doom-onehalf-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-onehalf-dark-theme
  :type 'boolean)

(defcustom doom-onehalf-dark-comment-bg doom-onehalf-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-onehalf-dark-theme
  :type 'boolean)

(defcustom doom-onehalf-dark-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-onehalf-dark-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-onehalf-dark
  "Doom onehalf-dark theme"

  ;; name        default   256       16
  ((bg         '("#282C34" nil       nil            ))
   (bg-alt     '("#3A404C" nil       nil            ))
   (base0      '("#141619" "#121212" "black"        ))
   (base1      '("#272B33" "#303030" "brightblack"  ))
   (base2      '("#3B414C" "#444444" "brightblack"  ))
   (base3      '("#4E5765" "#585858" "brightblack"  ))
   (base4      '("#626C7E" "#5F5F87" "brightblack"  ))
   (base5      '("#778296" "#878787" "brightblack"  ))
   (base6      '("#9099A9" "#9E9E9E" "brightblack"  ))
   (base7      '("#A9B1BD" "#B2B2B2" "brightblack"  ))
   (base8      '("#C3C8D0" "#C6C6C6" "white"        ))
   (fg-alt     '("#E3E5E9" "#E4E4E4" "brightwhite"  ))
   (fg         '("#DCDFE4" "#E4E4E4" "white"        ))

   (grey       base4)
   (red        '("#E06C75" "#D75F87" "red"          ))
   (orange     '("#E29678" "#D78787" "brightred"    ))
   (green      '("#76AF4E" "#87AF5F" "green"        ))
   (teal       '("#98C379" "#87AF87" "brightgreen"  ))
   (yellow     '("#E5C07B" "#D7AF87" "yellow"       ))
   (blue       '("#61AFEF" "#5FAFFF" "brightblue"   ))
   (dark-blue  '("#2490E9" "#0087D7" "blue"         ))
   (magenta    '("#AE42CF" "#AF5FD7" "magenta"      ))
   (violet     '("#C678DD" "#D787D7" "brightmagenta"))
   (cyan       '("#56B6C2" "#5FAFAF" "brightcyan"   ))
   (dark-cyan  '("#3B98A5" "#5F87AF" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-onehalf-dark-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-onehalf-dark-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      yellow)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      cyan)
   (numbers        magenta)
   (region         dark-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-onehalf-dark-brighter-modeline)
   (-modeline-pad
    (when doom-onehalf-dark-padded-modeline
      (if (integerp doom-onehalf-dark-padded-modeline) doom-onehalf-dark-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-onehalf-dark-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (mode-line-buffer-id
    :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (telephone-line-accent-active
    :inherit 'mode-line
    :background (doom-lighten bg 0.2))
   (telephone-line-accent-inactive
    :inherit 'mode-line
    :background (doom-lighten bg 0.05))
   (telephone-line-evil-emacs
    :inherit 'mode-line
    :background dark-blue)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-onehalf-dark-theme.el ends here