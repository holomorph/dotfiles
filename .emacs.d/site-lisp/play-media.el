;;; play-media.el --- Launch videos and stuff from Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2014-2017  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.4.1
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
;; Written originally to use mpv <http://mpv.io/> and livestreamer
;; <http://docs.livestreamer.io/>, or some other player.  For mpv, at
;; least v0.7.0 is required for its ytdl hook (turned on by default in
;; v0.7.2). For livestreamer, at least v1.9.0 is needed for the
;; "--default-stream" config option.

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

(defun play-media-start-process (program &rest args)
  "Thin wrapper for `start-process'."
  (message "Playing %s %s" program (mapconcat #'identity args " "))
  (apply #'start-process "play-media" nil program args))

(defun play-media-media-at-point ()
  "Return URL, file name, etc. of media at point, otherwise nil."
  (or (get-text-property (point) 'shr-url)
      (if (fboundp 'eww-current-url) (eww-current-url))
      (get-text-property (point) :nt-link)
      (thing-at-point 'url)
      (url-get-url-at-point)
      (let* ((fn (run-hook-with-args-until-success 'file-name-at-point-functions))
             (dir (and fn (file-name-as-directory fn))))
        (unless (equal dir fn) fn))
      ;; catch incomplete URLs
      (thing-at-point 'symbol)))

;;;###autoload
(defun play-media (url)
  "Play media from URL."
  (interactive
   (let* ((str (play-media-media-at-point))
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
  (let ((link (play-media-media-at-point)))
    (if link (play-media link)
      (message "No link found"))))

(provide 'play-media)

;;; play-media.el ends here
