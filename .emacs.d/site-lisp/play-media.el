;;; play-media.el --- Launch videos and stuff from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2020  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.5
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Play media from Emacs using an external player.

;;; Code:

(require 'url-parse)
(require 'url-util)

(defgroup play-media nil
  "Open media with an external player."
  :group 'external)

(defcustom play-media-program "mpv"
  "The name by which to invoke a media player."
  :type 'string)

(defcustom play-media-switches nil
  "Options specified for `play-media-program'."
  :type '(repeat string))

(defcustom play-media-at-point-functions
  '(play-media-eww-at-point
    play-media-feed-at-point
    play-media-url-at-point
    play-media-file-at-point
    symbol-at-point)
  "List of functions to try in sequence to get media at point.
Elements should return URL, file name, etc. at point,
otherwise nil."
  :type 'hook
  :options '(play-media-eww-at-point
             play-media-feed-at-point
             play-media-url-at-point
             play-media-file-at-point
             symbol-at-point))

(defun play-media-eww-at-point ()
  "Return shr or EWW URL at point, otherwise nil."
  (or (get-text-property (point) 'shr-url)
      (if (fboundp 'eww-current-url) (eww-current-url))))

(defun play-media-feed-at-point ()
  "Return elfeed or newsticker link at point, otherwise nil."
  (or (when (and (fboundp 'elfeed-search-selected)
                 (fboundp 'elfeed-entry-link))
        (let ((entry (elfeed-search-selected t)))
          (when entry
            (elfeed-entry-link entry))))
      (get-text-property (point) :nt-link)))

(defun play-media-url-at-point ()
  "Return URL at point, otherwise nil."
  (or (thing-at-point 'url)
      (url-get-url-at-point)))

(defun play-media-file-at-point ()
  "Return file name at point, otherwise nil."
  (let* ((fn (run-hook-with-args-until-success 'file-name-at-point-functions))
         (dir (and fn (file-name-as-directory fn))))
    (unless (equal dir fn) fn)))

(defun play-media-start-process (program &rest args)
  "Thin wrapper for `start-process'."
  (message "Playing %s %s" program (mapconcat #'identity args " "))
  (apply #'start-process "play-media" nil program args))

;;;###autoload
(defun play-media (url)
  "Play media from URL."
  (interactive
   (let* ((str (run-hook-with-args-until-success 'play-media-at-point-functions))
          (prompt (if str (format "Play media (default %s): " str)
                    "Play media: ")))
     (list (read-string prompt nil nil str))))
  (cond
   ((file-readable-p url) (setq url (expand-file-name url)))
   ((not (url-type (url-generic-parse-url url)))
    (setq url (concat "ytdl://" url))))
  (apply #'play-media-start-process play-media-program
         (append play-media-switches (list url))))

;;;###autoload
(defun play-media-at-point ()
  "Try to play media at point.
See `play-media' and `play-media-media-at-point'."
  (interactive)
  (let ((link (run-hook-with-args-until-success 'play-media-at-point-functions)))
    (if link (play-media link)
      (message "No link found"))))

(provide 'play-media)

;;; play-media.el ends here
