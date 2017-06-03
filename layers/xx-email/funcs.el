(defvar xx-email--notmuch-hello-hidden-mailboxes nil
  "List of mailboxes titles whose contents are hidden")

(defun xx-email//notmuch-get-maildirs-1 (mailroot mdir)
  "Get maildirs, recursively, as a list of relative paths.
The code is stolen from `mu4e'."
  (let* ((dentries
          (directory-files-and-attributes (concat mailroot "/" mdir) nil "^[^.]"))
         dirs)
    (dolist (dentry dentries)
      (when (and (booleanp (cadr dentry)) (cadr dentry))
        (if (file-accessible-directory-p
             (concat mailroot "/" mdir "/" (car dentry) "/cur"))
            (setq dirs (cons (concat mdir (car dentry)) dirs)))
        (unless (member (car dentry) '("cur" "new" "tmp"))
          (setq dirs
                (append dirs (xx-email//notmuch-get-maildirs-1
                                  mailroot (concat mdir (car dentry) "/")))))))
    dirs))

;; FIXME
(defun xx-email//notmuch-get-maildirs-2 ()
  "Get maildirs, recursively, as a list of relative paths.
The code is stolen from `mu4e'."
  (let* ((mdir (if  mdir ""))
         (mailroot
          (car (process-lines notmuch-command "config" "get" "database.path")))
         (dentries
          (directory-files-and-attributes (concat mailroot "/" mdir) nil "^[^.]"))
         dirs)
    (dolist (dentry dentries)
      (when (and (booleanp (cadr dentry)) (cadr dentry))
        (if (file-accessible-directory-p
             (concat mailroot "/" mdir "/" (car dentry) "/cur"))
            (setq dirs (cons (concat mdir (car dentry)) dirs)))
        (unless (member (car dentry) '("cur" "new" "tmp"))
          (setq dirs
                (append dirs (xx-email//notmuch-get-maildirs-1
                              mailroot (concat mdir (car dentry) "/")))))))
    dirs))

(defun xx-email//notmuch-get-maildirs ()
  (let* ((mailroot
          (car (process-lines notmuch-command "config" "get" "database.path")))
         (maildirs
          (sort (xx-email//notmuch-get-maildirs-1 mailroot "")
                (lambda (s1 s2)
                  (string< (downcase s1) (downcase s2))))))
    maildirs))

;; (mapc (lambda (maildir)
;;         (message "The maildir %s has %s messages.\n"
;;                  maildir
;;                  (car (process-lines notmuch-command "count" (concat "folder:" maildir)))))
;;       (xx-email//notmuch-get-maildirs))

;; TODO check the function `mu4e-maildirs-extension-load-maildirs'

(defun xx-email//notmuch-hello-qurry-maildir (maildirs)
  (let ((maildirs (xx-email//notmuch-get-maildirs)))
    (mapcar (lambda ()
              ))))

(defun xx-email//notmuch-hello-insert-maildirs (&optional title &rest options)
  "Insert the mailboxes section in notmuch-hello buffer."
  (apply 'notmuch-hello-insert-searches
         (or title "Maildirs")
         ))

(defun xx-email//notmuch-count-query (query)
  (with-temp-buffer
    (insert query "\n")
    (unless (= (call-process-region (point-min) (point-max) notmuch-command
                                    t t nil "count" "--batch") 0)
      (notmuch-logged-error "notmuch count --batch failed"
                            "Please update your notmuch."))

    (goto-char (point-min))
    (let ((n (read (current-buffer))))
      (if (= n 0)
          nil
        (notmuch-hello-nice-number n)))))

(defun xx-email//notmuch-hello-query-insert (cnt query elem)
  (if cnt
      (let* ((str (format "%s" cnt))
             (widget-push-button-prefix "")
             (widget-push-button-suffix "")
             (oldest-first (case (plist-get elem :sort-order)
                             (newest-first nil)
                             (oldest-first t)
                             (otherwise notmuch-search-oldest-first))))
        (widget-create 'push-button
                       :notify #'notmuch-hello-widget-search
                       :notmuch-search-terms query
                       :notmuch-search-oldest-first oldest-first
                       :notmuch-search-type 'tree
                       str)
        (widget-insert (make-string (- 8 (length str)) ? )))
    (widget-insert "        ")))

(defun xx-email//notmuch-hello-insert-searches ()
  "Insert the saved-searches section."
  (widget-insert (propertize "New     Total      Key  Tags/Searches\n" 'face 'xx-email-notmuch-hello-header-face))
  (mapc (lambda (elem)
          (when elem
            (let* ((q_tot (plist-get elem :query))
                   (q_new (if (string= q_tot "*")
                              "tag:unread"
                            (concat q_tot " AND tag:unread")))
                   (n_tot (xx-email//notmuch-count-query q_tot))
                   (n_new (xx-email//notmuch-count-query q_new)))
              (xx-email//notmuch-hello-query-insert n_new q_new elem)
              (xx-email//notmuch-hello-query-insert n_tot q_tot elem)
              (widget-insert "   ")
              (widget-insert (plist-get elem :key))
              (widget-insert "    ")
              (widget-insert (plist-get elem :name))
              (widget-insert "\n")
              )))
        notmuch-saved-searches))

(defun xx-email//notmuch-hello-insert-recent-searches ()
  "Insert recent searches."
  (when notmuch-search-history
    (widget-insert "Recent searches:")
    (widget-insert "\n\n")
    (let ((start (point)))
      (loop for i from 1 to notmuch-hello-recent-searches-max
            for search in notmuch-search-history do
            (let ((widget-symbol (intern (format "notmuch-hello-search-%d" i))))
              (set widget-symbol
                   (widget-create 'editable-field
                                  ;; Don't let the search boxes be less than 8 characters wide.
                                  :size (max 8
                                             (- (window-width)
                        ;; Leave some space at the start and end of the boxes.
                                                (* 2 notmuch-hello-indent)
                        ;; 1 for the space before the `[del]' button. 5 for the
                        ;; `[del]' button.
                                                1 5))
                                  :action (lambda (widget &rest ignore)
                                            (notmuch-hello-search (widget-value widget)))
                  search))
          (widget-insert " ")
          (widget-create 'push-button
                 :notify (lambda (widget &rest ignore)
                       (when (y-or-n-p "Are you sure you want to delete this search? ")
                     (notmuch-hello-delete-search-from-history widget)))
                 :notmuch-saved-search-widget widget-symbol
                 "del"))
        (widget-insert "\n"))
      (indent-rigidly start (point) notmuch-hello-indent))
    nil))

