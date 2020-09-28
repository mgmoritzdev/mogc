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

(defun mogc--get-list-from-command (command &optional column-name)
  (let ((list-items (split-string
                     (shell-command-to-string
                      (format "%s --quiet%s"
                              command
                              (if column-name
                                  (format " --format=\"[no-heading](%s)\"" column-name)
                                "")))
                     "\n")))
    (cl-remove-if
     (lambda (str)
       (string= "" str)) list-items)))

(defun mogc-config-get-project ()
  (interactive)
  (message (shell-command-to-string
            "echo -n $(gcloud config list --format 'value(core.project)')")))

(defun mogc--projects-list ()
  (mogc--get-list-from-command "gcloud projects list" "PROJECT_ID"))

(defun mogc--compute-zones-list ()
  (mogc--get-list-from-command "gcloud compute zones list" "name"))

(defun mogc--config-set-project (name)
  "Start a Processing sketch"
  (let ((process-name "gcloud-config-set-project"))
    (start-process name name "gcloud" "config" "set" "project" name)))

(defun mogc--compute-instances-start (instance)
  "Start a compute instance"
  (let ((process-name "gcloud-compute-instances-start"))
    (start-process process-name process-name
                   "gcloud" "compute" "instances" "start"
                   (alist-get 'name instance)
                   (format "--zone=%s" (alist-get 'zone instance)))))

(defun mogc--compute-instances-stop (instance)
  "Start a compute instance. Accepts a association list
with name, zone and status of a virtual machine"
  (let ((process-name "gcloud-compute-instances-stop"))
    (start-process process-name process-name
                   "gcloud" "compute" "instances" "stop"
                   (alist-get 'name instance)
                   (format "--zone=%s" (alist-get 'zone instance)))))

(defun mogc--get-roles (project &optional filter)
  (let ((roles-list (split-string
                     (shell-command-to-string
                      (format "gcloud iam list-grantable-roles //cloudresourcemanager.googleapis.com/projects/%s --filter \"%s\" --format=\"value(name)\"" project filter))
                     "\n")))
    (mapc 'message roles-list)))

(defun mogc-compute-instances-list ()
  "List virtual machines in this project"
  (interactive)
  (let* ((command "gcloud compute instances list")
         (buffer (get-buffer-create (format "*%s*" command))))
    (with-current-buffer buffer
      (erase-buffer)
      (insert
       (seq-reduce
        (lambda (a b)
          (concat a "\n" b))
        (mogc--get-list-from-command command "name,zone,status,networkInterfaces[0].networkIP,networkInterfaces[0].accessConfigs[0].natIP")
        ""))
      (goto-char (point-min))
      (delete-blank-lines))
    (switch-to-buffer-other-window buffer)))

(defun mogc--compute-instances-list ()
  (mapcar
   (lambda (line)
     (let ((props (cl-remove-if (lambda (str) (string= "" str)) (split-string line " "))))
       `((name . ,(car props))
         (zone . ,(cadr props))
         (status . ,(caddr props)))))
   (mogc--get-list-from-command "gcloud compute instances list" "name,zone,status")))

(defun mogc--compute-instance-stop-candidates-function (str pred _)
  "List all instances in status RUNNING"
  (let ((candidates (cl-remove-if-not
                     (lambda (c) (string= "RUNNING" (alist-get 'status c)))
                     (mogc--compute-instances-list))))
    (let ((cand-names (mapcar (lambda (c)
                                (cdr (assoc 'name c)))
                              candidates)))
      (cl-mapcar (lambda (s p) (propertize s 'property p))
                 cand-names
                 candidates))))

(defun mogc--compute-instance-start-candidates-function (str pred _)
  "List all instances in status TERMINATED"
  (let ((candidates (cl-remove-if-not
                     (lambda (c) (string= "TERMINATED" (alist-get 'status c)))
                     (mogc--compute-instances-list))))
    (let ((cand-names (mapcar (lambda (c)
                                (cdr (assoc 'name c)))
                              candidates)))
      (cl-mapcar (lambda (s p) (propertize s 'property p))
                 cand-names
                 candidates))))

(defun mogc-compute-instances-start ()
  (interactive)
  (ivy-read "start instance: "
            #'mogc--compute-instance-start-candidates-function
            :action (lambda (candidate)
                      (mogc--compute-instances-start (get-text-property 0 'property candidate)))))

(defun mogc-compute-instances-stop ()
  (interactive)
  (ivy-read "Stop instance: "
            #'mogc--compute-instance-stop-candidates-function
            :action (lambda (candidate)
                      (mogc--compute-instances-stop (get-text-property 0 'property candidate)))))

(defun mogc-config-set-project ()
  (interactive)
  (ivy-read "set project: "
            (mogc--projects-list)
            :action (lambda (candidate)
                      (mogc--config-set-project candidate))))

(require 'hydra)

(defhydra hydra-mogc ()
  "Moritz gcloud"
  ("p" mogc-config-get-project "Get active project")
  ("u" mogc-compute-instances-start "Start Instance")
  ("d" mogc-compute-instances-stop "Stop Instance")
  ("P" mogc-config-set-project "Set project")
  ("q" nil "quit" :color blue))

(defalias 'gpset 'mogc-config-set-project)
(defalias 'gpget 'mogc-config-get-project)
(defalias 'gci-list 'mogc-compute-instances-list)
(defalias 'gci-start 'mogc-compute-instances-start)
(defalias 'gci-stop 'mogc-compute-instances-stop)

(provide 'mogc)
