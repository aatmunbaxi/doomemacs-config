(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8d146df8bd640320d5ca94d2913392bc6f763d5bc2bb47bed8e14975017eea91" "c63d2e6502d4f78e359d1fa610f137dd04778d39133d7d9e75255d2e13c6b8c4" "9d5124bef86c2348d7d4774ca384ae7b6027ff7f6eb3c401378e298ce605f83a" "e5e253a4d31d709f1b7147fe6bb237ed2b9353685eea9a9e18652ac917f48823" "436b2a4af465aa0499c27aa5906a2808d5a381b5329e59db3c028aceb084be3d" "276c08753eae8e13d7c4f851432b627af58597f2d57b09f790cb782f6f043966" "ccff17f0cb616e239e2de4bd78f0b2e8f1f49291aa43c50845e250203be27a95" "93011fe35859772a6766df8a4be817add8bfe105246173206478a0706f88b33d" "38c0c668d8ac3841cb9608522ca116067177c92feeabc6f002a27249976d7434" "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e" default))
 '(elfeed-feeds
   '("https://www.youtube.com/feeds/videos.xml?channel_id=UCGjV4bsC43On-YuiLZcfL0w" "https://www.youtube.com/feeds/videos.xml?channel_id=UCYiw5-IDUwtWUc2e6t-hcXg" "https://www.youtube.com/feeds/videos.xml?channel_id=UCe0DNp0mKMqrYVaTundyr9w" "https://www.youtube.com/feeds/videos.xml?channel_id=UCMb0O2CdPBNi-QqPk5T3gsQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCM6SJb18voXy12YI0WWvcWQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UC1B_JfwK3vkhm7VmB-3X_hA" "https://www.youtube.com/feeds/videos.xml?channel_id=UC9_p50tH3WmMslWRWKnM7dQ" "https://www.youtube.com/feeds/videos.xml?channel_id=UCDXTQ8nWmx_EhZ2v-kp7QxA"
     #("Not found!" 0 10
       (face error))
     ("https://rss.arxiv.org/atom/math.QA" math)
     ("https://golem.ph.utexas.edu/category/rss.html" math)
     ("https://www.nhk.or.jp/rss/news/cat1.xml" japanese news business)
     ("https://www.nhk.or.jp/rss/news/cat6.xml" japanese news culture)
     ("https://www.nhk.or.jp/rss/news/cat7.xml" japanese news sports)
     ("https://tonosamaginta.jp/?xml" japanese blog)
     ("http://planet.lisp.org/rss20.xml" tech lisp programming lisp blog)
     ("https:reddit.com/r/emacs.rss" tech Emacs Emacs reddit)
     ("https://planet.emacslife.com/atom.xml" tech Emacs Emacs blog)
     ("https://karthinks.com/index.xml" tech Emacs Emacs)))
 '(magit-todos-insert-after '(bottom) nil nil "Changed by setter of obsolete option `magit-todos-insert-at'")
 '(org-agenda-files
   '("~/Documents/org/maybe.org" "/home/aatmun/Documents/org/gtd.org" "/home/aatmun/Documents/org/tickler.org" "/home/aatmun/Documents/org/graveyard.org"))
 '(safe-local-variable-values
   '((major-mode function lisp-data-mode)
     (consult-org-roam-mode . 1))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "unspecified-bg")))
 '(header-line ((t :box (:line-width 4 :color "unspecified-bg" :style nil))))
 '(header-line-highlight ((t :box (:color "unspecified-fg"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "unspecified-bg")))
 '(mode-line ((t :box (:line-width 6 :color "unspecified-bg" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color nil :style nil))))
 '(mode-line-highlight ((t :box (:color "unspecified-fg"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color nil :style nil))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(org-modern-date-active ((t (:background "#5c5e66"))))
 '(org-modern-date-inactive ((t (:background "#ebecef"))))
 '(org-modern-time-active ((t (:background "#696a4f"))))
 '(org-modern-time-inactive ((t (:background "#f1eae8"))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-current ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "unspecified-bg" :foreground "unspecified-bg")))
 '(window-divider ((t (:background "unspecified-bg" :foreground "unspecified-bg"))))
 '(window-divider-first-pixel ((t (:background "unspecified-bg" :foreground "unspecified-bg"))))
 '(window-divider-last-pixel ((t (:background "unspecified-bg" :foreground "unspecified-bg")))))
(put 'projectile-ripgrep 'disabled nil)
(put 'projectile-grep 'disabled nil)
(put 'customize-group 'disabled nil)
(put 'narrow-to-region 'disabled nil)
