;;; mogc.el --- Moritz wrapper for gcloud common commands

;; Copyright (C) 2011 Free Software Foundation, Inc.

;; Author: Marcos G Moritz <marcosmoritz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((ivy 0.13.0) (hydra "0.15.0"))
;; Keywords: gcloud, google, cli
;; URL: https://mo-profile.com/mogc

;;; Commentary:
;; This is package to quickly create video

;; This is package wraps commonly used gcloud commands
;; and run it in a subprocess

(require 'ivy)
(defun mogc-config-get-project ()
  (interactive)
  (message (shell-command-to-string
            "echo -n $(gcloud config list --format 'value(core.project)')")))

(defun mogc-projects-list ()
  (let ((project-list (split-string
                       (shell-command-to-string
                        "gcloud projects list --quiet --format=\"[no-heading](PROJECT_ID)\"")
                       "\n")))
    (mapc 'message project-list)))

(defun mogc--config-set-project (name)
  "Start a Processing sketch"
  (let ((process-name "gcloud-config-set-project"))
    (start-process name name "gcloud" "config" "set" "project" name)))

(defun mogc--get-roles (project &optional filter)
  (let ((roles-list (split-string
                     (shell-command-to-string
                      (format "gcloud iam list-grantable-roles //cloudresourcemanager.googleapis.com/projects/%s --filter \"%s\" --format=\"value(name)\"" project filter))
                     "\n")))
    (mapc 'message roles-list)))


(defun mogc-config-set-project ()
  (interactive)
  (ivy-read "set project: "
            (mogc-projects-list)
            :action (lambda (candidate)
                      (mogc--config-set-project candidate))))

(require 'hydra)

(defhydra hydra-mogc ()
  "Moritz gcloud"
  ("p" mogc-config-get-project "Get active project")
  ("P" mogc-config-set-project "Set project")
  ("q" nil "quit" :color blue))

(defalias 'gpset 'mogc-config-set-project)
(defalias 'gpget 'mogc-config-get-project)

(provide 'mogc)
