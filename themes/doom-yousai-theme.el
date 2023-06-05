;;; doom-yousai-theme.el
(require 'doom-themes)

;;
(defgroup doom-yousai-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-yousai-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-yousai-theme
  :type 'boolean)

(defcustom doom-yousai-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-yousai-theme
  :type 'boolean)

(defcustom doom-yousai-comment-bg doom-yousai-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-yousai-theme
  :type 'boolean)

(defcustom doom-yousai-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-yousai-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-yousai
  "Doom yousai theme"

  ;; name        default   256       16
  ((bg         '("#F5E7DE" nil       nil            ))
   (bg-alt     '("#EACDBA" nil       nil            ))
   (base0      '("#ECEAE9" "#EEEEEE" "black"        ))
   (base1      '("#D9D5D3" "#D7D7D7" "brightblack"  ))
   (base2      '("#C6C0BD" "#C0C0C0" "brightblack"  ))
   (base3      '("#B2ACA6" "#AFAFAF" "brightblack"  ))
   (base4      '("#9F9790" "#949494" "brightblack"  ))
   (base5      '("#8C827A" "#808080" "brightblack"  ))
   (base6      '("#766D66" "#6C6C6C" "brightblack"  ))
   (base7      '("#605953" "#585858" "brightblack"  ))
   (base8      '("#4A4440" "#444444" "white"        ))
   (fg-alt     '("#2A2624" "#262626" "brightwhite"  ))
   (fg         '("#34302D" "#303030" "white"        ))

   (grey       base4)
   (red        '("#992E2E" "#870000" "red"          ))
   (orange     '("#AC5E47" "#AF5F5F" "brightred"    ))
   (green      '("#4C3226" "#3A3A3A" "green"        ))
   (teal       '("#664233" "#444444" "brightgreen"  ))
   (yellow     '("#BF8F60" "#AF875F" "yellow"       ))
   (blue       '("#5986B2" "#5F87AF" "brightblue"   ))
   (dark-blue  '("#4C7399" "#5F5F87" "blue"         ))
   (magenta    '("#D9AE98" "#D7AF87" "magenta"      ))
   (violet     '("#BF9986" "#AF8787" "brightmagenta"))
   (cyan       '("#F2854A" "#FF875F" "brightcyan"   ))
   (dark-cyan  '("#D97742" "#D7875F" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-yousai-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-yousai-brighter-comments dark-cyan base5) 0.25))
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
   (-modeline-bright doom-yousai-brighter-modeline)
   (-modeline-pad
    (when doom-yousai-padded-modeline
      (if (integerp doom-yousai-padded-modeline) doom-yousai-padded-modeline 4)))

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
    :background (if doom-yousai-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-yousai-theme.el ends here