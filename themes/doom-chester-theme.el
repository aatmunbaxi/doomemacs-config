;;; doom-chester-theme.el
(require 'doom-themes)

;;
(defgroup doom-chester-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-chester-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-chester-theme
  :type 'boolean)

(defcustom doom-chester-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-chester-theme
  :type 'boolean)

(defcustom doom-chester-comment-bg doom-chester-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-chester-theme
  :type 'boolean)

(defcustom doom-chester-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-chester-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-chester
  "Doom chester theme"

  ;; name        default   256       16
  ((bg         '("#2C3643" nil       nil            ))
   (bg-alt     '("#3C495B" nil       nil            ))
   (base0      '("#191919" "#1C1C1C" "black"        ))
   (base1      '("#333333" "#303030" "brightblack"  ))
   (base2      '("#4D4D4D" "#4E4E4E" "brightblack"  ))
   (base3      '("#666666" "#626262" "brightblack"  ))
   (base4      '("#808080" "#808080" "brightblack"  ))
   (base5      '("#999999" "#949494" "brightblack"  ))
   (base6      '("#B2B2B2" "#B2B2B2" "brightblack"  ))
   (base7      '("#CCCCCC" "#D0D0D0" "brightblack"  ))
   (base8      '("#E6E6E6" "#E4E4E4" "white"        ))
   (fg-alt     '("#FFFFFF" "#FFFFFF" "brightwhite"  ))
   (fg         '("#FFFFFF" "#FFFFFF" "white"        ))

   (grey       base4)
   (red        '("#FA5E5B" "#FF5F5F" "red"          ))
   (orange     '("#FCA664" "#FFAF5F" "brightred"    ))
   (green      '("#12A171" "#00AF5F" "green"        ))
   (teal       '("#16C98D" "#00D787" "brightgreen"  ))
   (yellow     '("#FEEF6D" "#FFFF5F" "yellow"       ))
   (blue       '("#278AD6" "#0087D7" "brightblue"   ))
   (dark-blue  '("#288AD6" "#0087D7" "blue"         ))
   (magenta    '("#B52B75" "#AF0087" "magenta"      ))
   (violet     '("#D34590" "#D75F87" "brightmagenta"))
   (cyan       '("#27DEDE" "#00D7D7" "brightcyan"   ))
   (dark-cyan  '("#28DDDE" "#00D7D7" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-chester-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-chester-brighter-comments dark-cyan base5) 0.25))
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
   (-modeline-bright doom-chester-brighter-modeline)
   (-modeline-pad
    (when doom-chester-padded-modeline
      (if (integerp doom-chester-padded-modeline) doom-chester-padded-modeline 4)))

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
    :background (if doom-chester-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-chester-theme.el ends here