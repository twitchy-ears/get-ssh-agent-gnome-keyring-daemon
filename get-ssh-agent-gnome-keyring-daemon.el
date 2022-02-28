;;; get-ssh-agent-gnome-keyring-daemon.el --- Gets SSH_AUTH_SOCK and SSH_AUTH_PID for an SSH agent running under Gnome Keyring Daemon -*- lexical-binding: t -*-

;; Copyright 2021 - Twitchy Ears

;; Author: Twitchy Ears https://github.com/twitchy-ears/
;; URL: https://github.com/twitchy-ears/get-ssh-agent-gnome-keyring-daemon
;; Version: 0.1
;; Package-Requires ((emacs "26.1"))
;; Keywords: ssh agent

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; History
;;
;; 2022-02-27 - initial version

;;; Commentary:

;; All you really need us something like this:
;; (use-package get-ssh-agent-gnome-keyring-daemon
;;  :config
;;  (get-ssh-agent-gnome-keyring-daemon))
;;
;; It will attempts to look for a socket in
;; /run/user/<uid>/keyring/.ssh then get the PID for the ssh-agent
;; associated.
;;
;; This was written because on Ubuntu 21.10 my
;; keychain-refresh-environment doesn't work, so now for my portable
;; config I have:
;;
;; (use-package get-ssh-agent-gnome-keyring-daemon
;;  :config
;;  (get-ssh-agent-gnome-keyring-daemon))
;; 
;; (use-package keychain-environment
;;  :ensure t
;;  :config
;;  (if (not get-ssh-agent-gnome-keyring-daemon)
;;      (keychain-refresh-environment)))
;;
;; Which means if it gets it from the gnome-keyring-daemon it doesn't
;; try keychain-refresh-environment.

;;; Code:
(defvar get-ssh-agent-gnome-keyring-daemon nil
  "Defaults to nil, if get-ssh-agent-gnome-keyring-daemon is run and successfully locates an ssh-agent PID it will be set to t")

(defun get-ssh-agent-gnome-keyring-daemon (&optional SOCKETNAME)
  "Looks for a socket at /run/user/<uid>/keyring/.ssh and if it finds one it uses pgrep to try and find the ssh-agent command associated with it and get its PID, if it has both it sets the SSH_AGENT_SOCK and SSH_AGENT_PID environment variables and sets 'get-ssh-agent-gnome-keyring-daemon' to t, otherwise this variable is set to nil.

Takes an optional argument of SOCKETNAME and if set will use that instead of trying to generate one.

WARNING: there is a non-zero chance that this string could be used to try and exploit your shell via a shell escape."
  (let* ((socktmp
          ;; Attempt to remove any single quotes that end up in this
          ;; to safe it a little from any escapes.
          (replace-regexp-in-string "'" ""
                                    (if SOCKETNAME
                                        SOCKETNAME
                                      (concat "/run/user/"
                                              (format "%i" (user-uid))
                                              "/keyring/.ssh"))))
         ;; (agentpid nil)
         (pgrep-command (format "pgrep -nfi 'ssh-agent -D -a %s'" socktmp))
         (kill-buffer-query-functions nil))
      
      ;; If the socket exists get the ssh-agent PID associated with
      ;; it, use replace-regexp-in-string to remove any ending
      ;; newlines and convert it to a number for test purposes,
      ;; however setenv expects a string, so leave it unconverted
      ;; there.
      (if (file-exists-p socktmp)

          (let ((agentpid (replace-regexp-in-string
                           "\n\\'" ""
                           (shell-command-to-string pgrep-command))))
              
              ;; If we get a number then set the variables.
              (if (numberp (string-to-number agentpid))
                  (progn
                    (setenv "SSH_AGENT_SOCK" socktmp)
                    (setenv "SSH_AGENT_PID" agentpid)
                    (setq get-ssh-agent-gnome-keyring-daemon t))))
        
        ;; Otherwise indicate failure
        (setq get-ssh-agent-gnome-keyring-daemon nil))))

(provide 'get-ssh-agent-gnome-keyring-daemon)
