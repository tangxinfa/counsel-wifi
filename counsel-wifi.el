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
(ivy-set-actions 'counsel-wifi `(("c" counsel-wifi--connect "connect")
                                 ("d" counsel-wifi--disconnect "disconnect")))

(defface counsel-wifi-connected '((t :inherit ivy-highlight-face))
  "Face used by `counsel-wifi' for connected WiFi."
  :group 'ivy-faces)

(defun counsel-wifi--connect (x)
  "Connect candidate X."
  (let* ((face (get-text-property 0 'face x))
         (connected (and face
                         (face-equal 'counsel-wifi-connected face)))
         (name (second (split-string x "\t" t))))
    (unless connected (call-process-shell-command (concat "nmcli con up id '" name "' &") nil 0))))

(defun counsel-wifi--disconnect (x)
  "Disconnect candidate X."
  (let* ((face (get-text-property 0 'face x))
         (connected (and face
                         (face-equal 'counsel-wifi-connected face)))
         (name (second (split-string x "\t" t))))
    (when connected (call-process-shell-command (concat "nmcli con down id '" name "' &") nil 0))))

(defvar counsel-wifi-connect
  #'(lambda ()
      "Connect wifi in wifi detail buffer."
      (interactive)
      (let ((name (buffer-local-value 'wifi-name (current-buffer))))
        (message "Connect WiFi %s" name)
        (call-process-shell-command (concat "nmcli con up id '" name "' &") nil 0)
        (kill-buffer (current-buffer)))))

(defvar counsel-wifi-disconnect
  #'(lambda ()
      "Disconnect wifi in wifi detail buffer."
      (interactive)
      (let ((name (buffer-local-value 'wifi-name (current-buffer))))
        (message "Disconnect WiFi %s" name)
        (call-process-shell-command (concat "nmcli con down id '" name "' &") nil 0)
        (kill-buffer (current-buffer)))))

(defun counsel-wifi--show (x)
  "Show candidate X."
  (let* ((face (get-text-property 0 'face x))
         (connected (and face
                         (face-equal 'counsel-wifi-connected face)))
         (name (second (split-string x "\t" t)))
         (buffer-name (format "*WiFi-%s*" name)))
    (shell-command (concat "nmcli con show id '" name "'") buffer-name buffer-name)
    (with-current-buffer buffer-name
      (set (make-local-variable 'wifi-name) name)
      (local-set-key (kbd "C-c C-c") #'counsel-wifi-connect)
      (local-set-key (kbd "C-c C-k") #'counsel-wifi-disconnect)
      (read-only-mode)
      (goto-char (point-min)))
    (switch-to-buffer-other-window buffer-name)))

(defun counsel--wifi-candidates ()
  "Return list of `counsel-wifi' candidates."
  (let* ((lines (split-string (shell-command-to-string
                               "nmcli -g SSID,IN-USE,BARS d wifi | sed -E -e 's/ :/#:/g' | sed -e 's/:/\t/g' | sort -r | awk -F'\t' '{print $2\"\t\"$3\"\t\"$1}' | uniq -f 2 | sed -E -e 's/^#/ /g' | LC_ALL=C sort -r") "\n"))
         (connected)
         (candidates (mapcar (lambda (line)
                               (cond ((string-prefix-p "*" line)
                                      (setq connected (propertize (string-trim (substring line 1))
                                                                  'face 'counsel-wifi-connected))
                                      nil)
                                     (t (string-trim line)))) lines)))
    (push connected candidates)
    (remove nil candidates)))

;;;###autoload
(defun counsel-wifi ()
  "Connect WiFi with Ivy."
  (interactive)
  (ivy-read "wifi: " (counsel--wifi-candidates)
            :history 'counsel-wifi-history
            :action #'counsel-wifi--show
            :caller 'counsel-wifi
            :require-match t))

(provide 'counsel-wifi)
;;; counsel-wifi.el ends here
