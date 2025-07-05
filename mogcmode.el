(require 'ivy)
(unintern 'mogcmode-mode-map)
(unintern 'mogcmode-map)

(add-to-list 'auto-mode-alist '("\\.mgc$" . mogcmode-mode))

(defun mogcmode--goto-object-root ()
  (beginning-of-line)
  (let ((object-name nil))
    (while (not object-name)
      (if (mogcmode--at-object-name-p)
          (setq object-name (thing-at-point 'line t))
        (forward-line -1)))
    (mogcmode--format-object object-name)))

(defun mogcmode--get-object-at-point ()
  (save-excursion
    (mogcmode--goto-object-root)))

(defun mogcmode-goto-next-object ()
  "Go to next object"
  (interactive)
  (mogcmode--move-to-object 1))

(defun mogcmode-goto-previous-object ()
  "Go to next object"
  (interactive)
  (mogcmode--move-to-object -1))

(defun mogcmode--at-instace-name-p ()
  (save-excursion
    (beginning-of-line)
    (and (> (line-number-at-pos (point)) 5)
         (= (following-char) 32)
         (not (string-equal (substring (thing-at-point 'line t) 0 6) "  NAME"))
         (not (string-equal (substring (thing-at-point 'line t) 0 6) "  No r")))))

(defun mogcmode--at-object-name-p ()
  (save-excursion
    (beginning-of-line)
    (not (= (following-char) 32))))

(defun mogcmode--move-to-object (arg)
  (beginning-of-line)
  (forward-line arg)
  (let ((object-name nil))
    (while (not object-name)
      (if (mogcmode--at-object-name-p)
          (setq object-name (thing-at-point 'line t))
        (if (eq (forward-line arg) 1)
            (setq object-name "EOF"))))
    (message (mogcmode--format-object object-name))))

(defun mogcmode--format-object (object-name)
  (downcase
   (replace-regexp-in-string
    "\s" ""
    (replace-regexp-in-string ":\n" "" object-name))))

(defun mogcmode--get-project ()
  (save-excursion
    (message
     (save-match-data
       (string-match "Project: \\(\.+\\)"
                     (buffer-substring-no-properties (point-min) (point-max)))
       (match-string
        1
        (buffer-substring-no-properties (point-min) (point-max)))))))

(defun mogcmode--get-namespace ()
  (save-excursion
    (save-match-data
      (string-match "Namespace: \\(\.+\\)"
                    (buffer-substring-no-properties (point-min) (point-max)))
      (match-string
       1
       (buffer-substring-no-properties (point-min) (point-max))))))

(defun mogcmode--format-output (output)
  (let* ((lines (split-string output "\n"))
         (formatted-output (cl-map 'list
                                   'mogcmode--add-two-spaces
                                   lines)))
    (mapconcat 'identity formatted-output "\n")))

(defun mogcmode--add-two-spaces (string)
  (format "  %s" string))

(defun mogcmode--insert-command-result (command)
  (read-only-mode -1)
  (end-of-line)
  (newline)
  (insert (mogcmode--format-output (shell-command-to-string command)))
  (delete-char -3)
  (read-only-mode 1))

(defun mogcmode-refresh-object ()
  (interactive)
  (if (< (line-number-at-pos) 4)
      (message "Not on an object.")
    (progn
      (let ((object (mogcmode--goto-object-root))
            (namespace (mogcmode--get-project)))
        (save-excursion
          (mogcmode--clear-object-list-maybe)
          (mogcmode--insert-command-result
           (format "gcloud %s --projectid %s"
                   object
                   namespace))))
      )))

(defun mogcmode--clear-object-list-maybe ()
  (mogcmode--goto-object-root)
  (save-excursion
    (forward-line 1)
    (if (not (mogcmode--at-object-name-p))
        (mogcmode--clear-object-list))))

(defun mogcmode--clear-object-list ()
  (read-only-mode -1)
  (save-excursion
    (let ((start nil)
          (end nil))
      (if (not (mogcmode--at-object-name-p))
          (mogcmode-goto-previous-object))
      (beginning-of-line)
      (forward-line 1)
      (setq start (point))
      (mogcmode-goto-next-object)
      (setq end (point))
      (delete-region start end)))
  (read-only-mode 1))

(defun mogcmode-hide-show ()
  (interactive)
  (if (mogcmode--at-object-name-p)
      (progn
        (save-excursion
          (forward-line 1)
          (if (mogcmode--at-object-name-p)
              (progn
                (forward-line -1)
                (mogcmode-refresh-object))
            (mogcmode--clear-object-list-maybe))))))

(defalias 'mogcmode--get-projects 'mogc--projects-list)

(defun mogcmode--update (regexp match newValue)
  (save-excursion
    (read-only-mode -1)
    (goto-char (point-min))
    (re-search-forward regexp)
    (let* ((beg (match-beginning match))
           (end (match-end match)))
      (delete-region beg end)
      (goto-char beg)
      (insert newValue)))
  (read-only-mode 1))

(defun mogcmode--update-project (newValue)
  (shell-command (format "kubectl config use-context %s" newValue))
  (mogcmode--update "Project: \\(\.+\\)" 1 newValue))

(defun mogcmode--update-namespace (newValue)
  (mogcmode--update "Namespace: \\(\.+\\)" 1 newValue))

(defun mogcmode-set-project-ivy ()
  (interactive)
  (ivy-read "Projects: "
            (mogcmode--get-projects)
            :action (lambda (candidate)
                      (mogcmode--update-context candidate))))

(defun mogcmode--get-namespaces ()
  (split-string
   (shell-command-to-string "kubectl get namespaces -o name | cut -d'/' -f2 | head -c-1")
   "\n"))

(defun mogcmode-set-namespace-ivy ()
  (interactive)
  (ivy-read "Namespaces: "
            (mogcmode--get-namespaces)
            :action (lambda (candidate)
                      (mogcmode--update-namespace candidate))))

(defun mogcmode--parse-instance-name ()
  (nth 2 (split-string
          (thing-at-point 'line t) " ")))

(defun mogcmode-describe-object ()
  (interactive)
  (if (mogcmode--at-instace-name-p)
      (let* ((object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "describe-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "describe" object instance "-n" namespace)
        (switch-to-buffer name))
    (message "No resource at point")))

(defun mogcmode-edit-object ()
  (interactive)
  (if (mogcmode--at-instace-name-p)
      (let* ((object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "edit-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "edit" object instance "-n" namespace))
    (message "No resource at point")))

(defun mogcmode-delete-object ()
  (interactive)
  (if (mogcmode--at-instace-name-p)
      (let* ((object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "delete-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "delete" object instance "-n" namespace))
    (message "No resource at point")))

(defun mogcmode-log-pod (arg)
  (interactive "P")
  (let ((arg (number-to-string (if arg
                                   arg
                                 500))))
    (if (and (mogcmode--at-instace-name-p)
             (string-equal (mogcmode--get-object-at-point) "pods"))
        (let* ((object (mogcmode--get-object-at-point))
               (instance (mogcmode--parse-instance-name))
               (namespace (mogcmode--get-namespace))
               (name (format "log-%s-%s-%s"
                             namespace
                             object
                             instance)))
          (start-process name name
                         "kubectl" "logs" "--tail" arg "-f"
                         instance "-n" namespace)
          (switch-to-buffer name)
          (add-hook 'after-change-functions 'ansi-color-after-change nil t)))))

(defun mogcmode--clear-buffer-and-insert (name object namespace)
  (with-current-buffer (get-buffer-create name)
    (let ((inhibit-read-only t)
          (current-position (point))
          (using-region (use-region-p))
          (beg (if (use-region-p)
                   (region-beginning)))
          (end (if (use-region-p)
                   (region-end))))
      (erase-buffer)
      (mogcmode--insert-command-result
       (format "kubectl get %s -n %s"
               object
               namespace))

      (goto-char current-position)
      (if using-region
          (if (< current-position end)
              (progn
                (set-mark end)
                (goto-char beg))
            (progn
              (set-mark beg)
              (goto-char end)))))))

(defun mogcmode-watch-object (arg)
  (interactive "P")
  (let ((arg (if arg
                   arg
                 10)))
    (let* ((object (mogcmode--get-object-at-point))
           (namespace (mogcmode--get-namespace))
           (name (format "watch-%s-%s"
                         namespace
                         object)))
      (if (not (get-buffer name))
          (run-at-time t arg 'mogcmode--clear-buffer-and-insert name object namespace))
      (switch-to-buffer name))))

(defun mogcmode-bash-pod ()
  (interactive)
  (if (and (mogcmode--at-instace-name-p)
           (string-equal (mogcmode--get-object-at-point) "pods"))
      (let* ((object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "bash-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (progn
          (ansi-term "/bin/zsh" name)
          (set-buffer (format "*%s*" name))
          (term-send-raw-string (format "kubectl exec -it %s -n %s -- /bin/bash \n"
                                        instance namespace)))
        (switch-to-buffer (format "*%s*" name)))
    (message "no pod at point")))

(defun mogcmode-port-forward-pod-or-service (arg)
  (interactive "P")
  (if (and (mogcmode--at-instace-name-p)
           (or (string-equal (mogcmode--get-object-at-point) "pods")
               (string-equal (mogcmode--get-object-at-point) "services")))
      (let* ((remote-port (if arg
                              (read-number "Remote Service Port: ")
                            80))
             (port (read-number "Port to forward: "))
             (object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "portforward-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name "kubectl" "port-forward"
                       (format "%s/%s" object instance)
                       (format "%d:%d" port remote-port)
                       "-n" namespace))
    (message "no pod or service at point")))

(defun mogcmode-exec-pod ()
  (interactive)
  (if (and (mogcmode--at-instace-name-p)
           (string-equal (mogcmode--get-object-at-point) "pods"))
      (let* ((cmd (read-string "Command: "))
             (object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "exec-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (apply 'start-process
               (append (list name name "kubectl" "-n" namespace "exec" instance)
                       (split-string cmd " ")))
        (switch-to-buffer name))
    (message "no pod at point")))

(defun mogcmode-top ()
  (interactive)
  (if (and (mogcmode--at-instace-name-p)
           (string-equal (mogcmode--get-object-at-point) "pods"))
      (let* ((object (mogcmode--get-object-at-point))
             (instance (mogcmode--parse-instance-name))
             (namespace (mogcmode--get-namespace))
             (name (format "top-%s-%s-%s"
                           namespace
                           object
                           instance)))
        (start-process name name
                       "kubectl" "top" object instance "-n" namespace)
        (switch-to-buffer name))
    (let* ((name "top-cluster"))
      (start-process name name
                     "kubectl" "top" "nodes")
      (switch-to-buffer name))))

(defvar mogcmode-map
  (let ((map (make-sparse-keymap)))
    ;; (define-key map (kbd "o") 'mogcmode--get-object-at-point)
    ;; (define-key map (kbd "u") 'mogcmode-goto-previous-object)
    ;; (define-key map (kbd "C-c C-p") 'mogcmode-goto-previous-object)
    ;; (define-key map (kbd "C-c C-n") 'mogcmode-goto-next-object)
    (define-key map (kbd "p") 'previous-line)
    (define-key map (kbd "n") 'next-line)
    ;; (define-key map (kbd "g") 'mogcmode-refresh-object)
    ;; (define-key map (kbd "TAB") 'mogcmode-hide-show)
    ;; (define-key map (kbd "C") 'mogcmode-set-context-ivy)
    ;; (define-key map (kbd "N") 'mogcmode-set-namespace-ivy)
    ;; (define-key map (kbd "d") 'mogcmode-describe-object)
    ;; (define-key map (kbd "f") 'mogcmode-port-forward-pod-or-service)
    ;; (define-key map (kbd "w") 'mogcmode-watch-object)
    ;; (define-key map (kbd "t") 'mogcmode-top)
    ;; (define-key map (kbd "e") 'mogcmode-edit-object)
    ;; (define-key map (kbd "E") 'mogcmode-exec-pod)
    ;; (define-key map (kbd "k") 'mogcmode-delete-object)
    ;; (define-key map (kbd "b") 'mogcmode-bash-pod)
    ;; (define-key map (kbd "q") 'bury-buffer)
    ;; (define-key map (kbd "?") 'describe-mode)
    ;; (define-key map (kbd "l") 'mogcmode-log-pod)
    map)
  "The keymap used in `mogcmode-mode'.")

(defvar mogcmode-highlights
  '(("Compute Instances:\\|" . font-lock-function-name-face)
    ("  NAME\.*" . font-lock-comment-delimiter-face)
    ("Region:\\|Zone:\\|Project:" . font-lock-constant-face)
    ("Project: \\(\.+\\)" . (1 font-lock-comment-face))
    ("Region: \\(\.+\\)" . (1 font-lock-comment-face))
    ("Zone: \\(\.+\\)" . (1 font-lock-comment-face))))

(define-derived-mode mogcmode-mode special-mode "mogcmode"
  "Moritz kubernetes major mode.
\\{mogcmode-map}"
  :group 'mogcmode-modes

  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq font-lock-defaults '(mogcmode-highlights))
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'nlinum-mode)
             (bound-and-true-p global-nlinum-mode))
    (nlinum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1))
  (use-local-map mogcmode-map))

(add-hook 'mogcmode-mode-hook 'hl-line-mode)

;; use ansi colors in logs
(defun moritz/ansi-color (&optional beg end)
  "Interpret ANSI color esacape sequence by colorifying cotent.
  Operate on selected region on whole buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (ansi-color-apply-on-region beg end))

(defun ansi-color-after-change (beg end length)
  (save-excursion
    (goto-char beg)
    (if (string-match "\^\[" (buffer-substring beg end))
        (moritz/ansi-color beg end))))
