;; -*- lexical-binding: t; -*-
(require 'eat)
(require 'transient)

(defun open-spotify ()
  (interactive)
  (save-excursion
    (eat)
    (switch-to-buffer "*eat*")
    (eat-term-send-string eat-terminal "ncspot")
    (eat-line-send-input)
    (rename-buffer "spotify" nil)))

(defun spotify ()
  (interactive)
  (if (get-buffer "spotify")
      (switch-to-buffer "spotify")
    (open-spotify)))

(defmacro ncspot-control-define-control (key control)
  (declare (indent 1))
  `(defun ,(intern (concat "ncspot-control-"  control)) ()
     (interactive)
     (if (get-buffer "spotify")
         (progn (set-buffer "spotify")
	        (eat-term-send-string eat-terminal ,(kbd key)))
       (when (yes-or-no-p "No spotify buffer open. Open?")
         (spotify)))))

(defmacro ncspot-control-define-focus-control (key control)
  (declare (indent 1))
  `(defun ,(intern (concat "ncspot-control-focus-control-"  control)) ()
     (interactive)
     (if (get-buffer "spotify")
         (progn
           (switch-to-buffer "spotify" :no-record)
	   (eat-term-send-string eat-terminal ,(kbd key)))
       (when (yes-or-no-p "No spotify buffer open. Open?")
         (spotify)
         (switch-to-buffer "spotify")
         (dotimes (i 10)
           (eat-term-send-string eat-terminal ,(kbd "C-q")))
	 (eat-term-send-string eat-terminal ,(kbd key))))))
;;;###autoload
(ncspot-control-define-control "P" "play/pause")

;;;###autoload
(ncspot-control-define-control "s" "save")
;;;###autoload
(ncspot-control-define-control "r" "repeat")
;;;###autoload
(ncspot-control-define-control "S" "stop")
;;;###autoload
(ncspot-control-define-control "M" "similar-current")
;;;###autoload
(ncspot-control-define-control "m" "similar-selected")
;;;###autoload
(ncspot-control-define-control "SPC" "queue-up")
;;;###autoload
(ncspot-control-define-control "C-s" "save-queue")
;;;###autoload
(ncspot-control-define-control "RET" "play")
;;;###autoload
(ncspot-control-define-control "SPC" "queue-down-1")
;;;###autoload
(ncspot-control-define-control "+" "volup")
;;;###autoload
(ncspot-control-define-control "-" "voldown")
;;;###autoload
(ncspot-control-define-control "]" "volwayup")
;;;###autoload
(ncspot-control-define-control "[" "volwaydown")
;;;###autoload
(ncspot-control-define-control ">" "next-track")
;;;###autoload
(ncspot-control-define-control "<" "previous-track")
;;;###autoload
(ncspot-control-define-control "C-a" "navleft")
;;;###autoload
(ncspot-control-define-control "C-p" "navup")
;;;###autoload
(ncspot-control-define-control "C-e" "navright")
;;;###autoload
(ncspot-control-define-control "C-n" "navdown")
;;;###autoload
(ncspot-control-define-control "C-b" "move-bottom")
;;;###autoload
(ncspot-control-define-control "C-t" "move-top")
;;;###autoload
(ncspot-control-define-control "z" "shuffle")
;;;###autoload
(ncspot-control-define-control "b" "seek-1000")
;;;###autoload
(ncspot-control-define-control "f"    "seek+1000")
;;;###autoload
(ncspot-control-define-control "C-q" "back")
;;;###autoload
(ncspot-control-define-control "RET" "play-selected")
;;;###autoload
(ncspot-control-define-control "d" "delete-track")
;;;###autoload
(ncspot-control-define-control "o" "open-selected")
;;;###autoload
(ncspot-control-define-control "O" "open-current")
;;;###autoload
(ncspot-control-define-control "U" "update")
;;;###autoload
(ncspot-control-define-control "c" "clear-queue")
;;;###autoload
(ncspot-control-define-control "n" "next-search-occurence")
;;;###autoload
(ncspot-control-define-control "N" "prev-search-occurence")
;;;###autoload
(ncspot-control-define-focus-control "C-r" "queue")
;;;###autoload
(ncspot-control-define-focus-control "C-d" "library")
;;;###autoload
(ncspot-control-define-focus-control "C-z" "search")
;;;###autoload
(ncspot-control-define-focus-control nil "ncspot")
;;;###autoload
(defun ncspot-control-close ()
  (interactive)
  (kill-buffer "spotify"))

(provide 'ncspot-control-lib)
;;; ncspot-control-lib ends here
