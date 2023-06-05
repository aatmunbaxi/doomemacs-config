;;; doom-visibone-alt-theme.el
(require 'doom-themes)

;;
(defgroup doom-visibone-alt-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-visibone-alt-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-visibone-alt-theme
  :type 'boolean)

(defcustom doom-visibone-alt-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-visibone-alt-theme
  :type 'boolean)

(defcustom doom-visibone-alt-comment-bg doom-visibone-alt-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-visibone-alt-theme
  :type 'boolean)

(defcustom doom-visibone-alt-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-visibone-alt-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-visibone-alt
  "Doom visibone-alt theme"

  ;; name        default   256       16
  ((bg         '("#333333" nil       nil            ))
   (bg-alt     '("#474747" nil       nil            ))
   (base0      '("#141414" "#121212" "black"        ))
   (base1      '("#292929" "#262626" "brightblack"  ))
   (base2      '("#3D3D3D" "#3A3A3A" "brightblack"  ))
   (base3      '("#525252" "#4E4E4E" "brightblack"  ))
   (base4      '("#666666" "#626262" "brightblack"  ))
   (base5      '("#7A7A7A" "#767676" "brightblack"  ))
   (base6      '("#8F8F8F" "#8A8A8A" "brightblack"  ))
   (base7      '("#A3A3A3" "#A8A8A8" "brightblack"  ))
   (base8      '("#B8B8B8" "#BCBCBC" "white"        ))
   (fg-alt     '("#D6D6D6" "#D7D7D7" "brightwhite"  ))
   (fg         '("#CCCCCC" "#D0D0D0" "white"        ))

   (grey       base4)
   (red        '("#CC6699" "#D75F87" "red"          ))
   (orange     '("#E69999" "#D78787" "brightred"    ))
   (green      '("#99CC66" "#87D75F" "green"        ))
   (teal       '("#CCFF99" "#D7FF87" "brightgreen"  ))
   (yellow     '("#FFCC99" "#FFD787" "yellow"       ))
   (blue       '("#99CCFF" "#87D7FF" "brightblue"   ))
   (dark-blue  '("#6699CC" "#5F87D7" "blue"         ))
   (magenta    '("#CC99FF" "#D787FF" "magenta"      ))
   (violet     '("#9966CC" "#875FD7" "brightmagenta"))
   (cyan       '("#99FFCC" "#87FFD7" "brightcyan"   ))
   (dark-cyan  '("#66CC99" "#5FD787" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-visibone-alt-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-visibone-alt-brighter-comments dark-cyan base5) 0.25))
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
   (-modeline-bright doom-visibone-alt-brighter-modeline)
   (-modeline-pad
    (when doom-visibone-alt-padded-modeline
      (if (integerp doom-visibone-alt-padded-modeline) doom-visibone-alt-padded-modeline 4)))

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
    :background (if doom-visibone-alt-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-visibone-alt-theme.el ends here