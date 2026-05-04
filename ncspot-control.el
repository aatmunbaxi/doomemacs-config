;; -*- lexical-binding: t; -*-
;;
(require 'ncspot-control-lib)
(require 'transient)

;;;###autoload (autoload 'listen-menu "listen" nil t)
(transient-define-prefix ncspot-control-queue ()
  "ncspot queue controller"
  [["Jump to"
    ("ml" "Library" (lambda ()
                      (interactive)
                      (spotify-focus-control-library)
                      (ncspot-control))
     :transient transient--do-replace)
    ("ms" "New query" ncspot-control-search :transient transient--do-recurse)
    ("mp" "Prev Search" (lambda ()
                          (interactive)
                          (spotify-focus-control-search)
                          (ncspot-control))
     :transient transient--do-replace)]

   ["Manage Queue"
    ("d" "Remove" ncspot-control-delete-track :transient t)
    ("qc" "Clear queue" ncspot-control-clear-queue :transient t)
    ;; TODO make this prompt for playlist name
    ("qs" "Save queue" ncspot-control-save-queue :transient nil)]
   ["Selected track..."
    ("sq" "Add to queue"     ncspot-control-queue-up :transient t)
    ("sl" "Similar" ncspot-control-similar-selected :transient t)
    ("so" "Open"  ncspot-control-open-selected :transient t)]
    ["Current track..."
    ("cs" "Save" ncspot-control-save :transient t)
    ("cl" "Similar" ncspot-control-similar-current :transient t)
    ("co" "Open" ncspot-control-open-current :transient t)]]
  [["Playback"
    ("RET" "Play selected" ncspot-control-play-selected :transient t)
    ("P" "Play/Pause" ncspot-control-play/pause :transient t)
    ("<" "Previous" ncspot-control-previous-track :transient t)
    ("r" "Repeat"     ncspot-control-repeat :transient t)]
   [""
    ""
    ("S" "Stop" ncspot-control-stop :transient t)
    (">" "Next" ncspot-control-next-track :transient t)
    ("z" "Shuffle"     ncspot-control-shuffle :transient t)]
   ["Navigation"
    ("C-p" "Up"  ncspot-control-navup :transient t)
    ("C-n" "Down"  ncspot-control-navdown :transient t)
    ("C-b" "Left"  ncspot-control-navleft :transient t)
    ("C-f" "Right"  ncspot-control-navright :transient t)]
   [""
    ("C-/" "Back" ncspot-control-back :transient t)
    ("M-<" "Top" ncspot-control-move-top :transient t)
    ("M->" "Bottom" ncspot-control-move-bottom :transient t)
    ("C-s" "Search view" ncspot-control-search-/ :transient transient--do-replace)]
   ["ncspot"
    ("R" "Return" (lambda ()
                    (interactive)
                    (dotimes (i 10)
                      (eat-term-send-string eat-terminal (kbd "C-q")))
                    (previous-buffer))  :transient nil)
    ("U" "Update library" ncspot-control-update :transient t)
    ("Q" "Quit ncspot" ncspot-control-close :transient nil)]])

;;;###autoload
(defun ncspot-control-open-queue-manage ()
  (interactive)
  (progn (spotify-focus-control-queue)
         (ncspot-control-queue)))

(transient-define-prefix ncspot-control-search-interaction ()
  "Interactive search moving for ncspot-control"
  [["Nav"
    ("C-r" "Previous occurrence" ncspot-control-prev-search-occurence :transient t)
    ("C-s" "Next occurrence" ncspot-control-next-search-occurence :transient t )]])

;;;###autoload (autoload 'listen-menu "listen" nil t)
(transient-define-prefix ncspot-control-quick-menu ()
  "Quick ncspot controls.

These are meant to be a selection of commands that are used when
another buffer is focused."
  [["Jump to"
    ("ml" "Library" (lambda ()
                              (interactive)
                              (spotify-focus-control-library)
                              (ncspot-control))
     :transient transient--do-replace)
    ("mq" "Queue" ncspot-control-open-queue-manage :transient transient--do-replace)
    ("mb" "New query" (lambda ()
                     (interactive)
                     (ncspot-control-search)
                     (ncspot-control))
     :transient transient--do-replace)
    ("mp" "Prev query" spotify-focus-control-search :transient t)]
   ["Playback"
    ("pp" "Play/Pause" ncspot-control-play/pause :transient t)


    ("b" "Seek -1000" ncspot-control-seek-1000 :transient t)

    ("<" "Previous Track" ncspot-control-previous-track :transient t)]
   [""
    ("ps" "Stop" ncspot-control-stop :transient t)
    ("f" "Seek +1000" ncspot-control-seek+1000 :transient t)
    (">" "Next Track" ncspot-control-next-track :transient t)]
   ["Current Track"
    ("s"  "Save"
     ncspot-control-save :transient t)
    ("O" "Open"  ncspot-control-open-current :transient transient--do-replace)]
   ["ncspot"
    ("F" "Focus" (lambda ()
                   (interactive)
                   (spotify-focus-control-ncspot)
                   (ncspot-control))
     :transient transient--do-replace)

    ("Q" "Quit ncspot" ncspot-control-close :transient nil)]])


;;;###autoload (autoload 'listen-menu "listen" nil t)
(transient-define-prefix ncspot-control ()
  "ncspot spotify controller.

This transient is meant to be open with the
ncspot terminal window open and visible, otherwise
some commands like toggling repeat and shuffle don't make
much sense."
  [["Jump to"
    ("ml" "Library" spotify-focus-control-library :transient t)
    ("mq" "Queue" (lambda ()
                    (interactive)
                    (spotify-focus-control-queue)
                    (ncspot-control-queue))
     :transient transient--do-replace)
    ("ms" "Browse new query" ncspot-control-search :transient transient--do-replace)
    ("mp" "Browse previous search" spotify-focus-control-search :transient t)]

   ["Selected track..."
    ("sq" "Add to queue"     ncspot-control-queue-up :transient t)
    ("sl" "Similar" ncspot-control-similar-selected :transient t)
    ("so" "Open"  ncspot-control-open-selected :transient t)]
    ["Current track..."
    ("cs" "Save" ncspot-control-save :transient t)
    ("cl" "Similar" ncspot-control-similar-current :transient t)
    ("co" "Open" ncspot-control-open-current :transient t)]]
  [["Playback"
    ("RET" "Play selected" ncspot-control-play-selected :transient t)
    ("P" "Play/Pause" ncspot-control-play/pause :transient t)
    ("<" "Previous" ncspot-control-previous-track :transient t)
    ("r" "Repeat"     ncspot-control-repeat :transient t)]
   [""
    ""
    ("S" "Stop" ncspot-control-stop :transient t)
    (">" "Next" ncspot-control-next-track :transient t)
    ("z" "Shuffle"     ncspot-control-shuffle :transient t)]
   ["Navigation"
    ("C-b" "Left"  ncspot-control-navleft :transient t)
    ("C-f" "Right"  ncspot-control-navright :transient t)
    ("C-p" "Up"  ncspot-control-navup :transient t)
    ("C-n" "Down"  ncspot-control-navdown :transient t)]
   [""
    ("C-/" "Back" ncspot-control-back :transient t)
    ("M-<" "Top" ncspot-control-move-top :transient t)
    ("M->" "Bottom" ncspot-control-move-bottom :transient t)
    ("C-s" "Search view" ncspot-control-search-/ :transient transient--do-recurse)]
   ["ncspot"
    ("R" "Return" (lambda ()
                    (interactive)
                    (dotimes (i 10)
                      (eat-term-send-string eat-terminal (kbd "C-q")))
                    (previous-buffer))  :transient nil)
    ("U" "Update library" ncspot-control-update :transient t)
    ("Q" "Quit ncspot" ncspot-control-close :transient nil)]])



(transient-define-suffix ncspot-control-search ()
  "Search and open control for the transient"
  :key "ms"
  :transient t
  :description "Browse"
  (interactive (let ((query (read-string "Search query: " nil nil nil nil)))
                 (prog1
                   (spotify-focus-control-search)
	           (eat-term-send-string eat-terminal (kbd "C-z"))
                   (eat-term-send-string eat-terminal query)
                   (eat-term-send-string eat-terminal (kbd "RET"))
                   (ncspot-control)))))

(transient-define-suffix ncspot-control-search-/ ()
  "Search and open control for the transient"
  :key "/"
  :transient t
  :description "Search"
  (interactive (let ((query (read-string "Search: " nil nil nil nil)))
                 (prog1
	           (eat-term-send-string eat-terminal (kbd "/"))
                   (eat-term-send-string eat-terminal query)
                   (eat-term-send-string eat-terminal (kbd "RET"))
                   (ncspot-control-search-interaction)))))


(transient-define-suffix ncspot-control-open-current ()
  "Search and open control for the transient"
  :key "o"
  :transient nil
  :description "Open"
  (interactive)
  (progn
    (spotify)
    (eat-term-send-string eat-terminal (kbd "O"))
    (ncspot-control)))

(defun ncspot-control-dispatch ()
  "Dispatch the correct spotify control menu"
  (interactive)
  (if (equal (current-buffer) (get-buffer "spotify"))
      (ncspot-control)
    (ncspot-control-quick-menu)))

(provide 'ncspot-control)
