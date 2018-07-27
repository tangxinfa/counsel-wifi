;;; counsel-wifi.el --- Connect WiFi with Ivy  -*- lexical-binding: t; -*-

;; Copyright (C) 2015-2018  Free Software Foundation, Inc.

;; Author: tangxinfa <tangxinfa@gmail.com>
;; Keywords: matching

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ivy)

;;** `counsel-wifi'
(defface counsel-wifi-connected
  '((t :inherit ivy-highlight-face))
  "Face used by `counsel-wifi' for connected WiFi."
  :group 'ivy-faces)

(defface counsel-wifi-header
  '((t :inherit font-lock-comment-face))
  "Face used by `counsel-wifi' for header."
  :group 'ivy-faces)

(defun counsel-wifi-action (x)
  "Action on candidate X."
  (let* ((face (get-text-property 0 'face x))
         (connected (and face (face-equal 'counsel-wifi-connected face)))
         (header (and face (face-equal 'counsel-wifi-header face)))
         (name (first (split-string x " " t))))
    (unless (or connected header)
      (call-process-shell-command (concat "nmcli con up id '" name "' &") nil 0))))

(defun counsel--wifi-candidates ()
  "Return list of `counsel-wifi' candidates."
  (let* ((lines (split-string (shell-command-to-string "nmcli d wifi") "\n"))
         (header)
         (connected)
         (candidates (mapcar (lambda (line)
                               (cond ((string-prefix-p "*" line)
                                      (setq connected (propertize (trim-string (substring line 1)) 'face 'counsel-wifi-connected))
                                      nil)
                                     ((string-prefix-p "IN-USE" line)
                                      (setq header (propertize (trim-string (substring line 6)) 'face 'counsel-wifi-header))
                                      nil)
                                     (t (trim-string line))))
                             lines)))
    (push connected candidates)
    (push header candidates)
    (remove nil candidates)))

;;;###autoload
(defun counsel-wifi ()
  "Connect WiFi with Ivy."
  (interactive)
  (ivy-read "wifi: " (counsel--wifi-candidates)
            :history 'counsel-wifi-history
            :action #'counsel-wifi-action
            :caller 'counsel-wifi
            :require-match t))

(provide 'counsel-wifi)
;;; counsel-wifi.el ends here
