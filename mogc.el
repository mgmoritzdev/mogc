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

(defun mogc-config-active-project ()
  (interactive)
  (message (shell-command-to-string
            "gcloud config list --format 'value(core.project)'")))

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

(defun mogc-config-set-project-ivy ()
  (interactive)
  (ivy-read "set project: "
            (mogc-projects-list)
            :action (lambda (candidate)
                      (mogc--config-set-project candidate))))


(require 'hydra)
(defhydra hydra-mogc ()
  "Moritz gcloud"
  ("p" mogc-config-active-project "Get active project")
  ("P" mogc-config-set-project-ivy "Set project")
  ("q" nil "quit" :color blue))

(provide 'mogc)
