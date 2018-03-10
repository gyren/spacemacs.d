;;; packages.el --- xx-email layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <xx@TPx240>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `xx-email-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `xx-email/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `xx-email/pre-init-PACKAGE' and/or
;;   `xx-email/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq xx-email-packages
      '(;; bbdb
        gnus
        notmuch
        mu4e
        ;; wanderlust
        ;; org
        ))

(defun xx-email/post-init-gnus ()
  (with-eval-after-load 'gnus
    (progn
      (use-package gnus-notifications
        :defer t
        :config
        (add-hook 'gnus-after-getting-new-news-hook 'gnus-notifications))

      (setq message-directory "~/Mail"
            ;gnus-directory "~/.new"
            gnus-secondary-select-methods
            '((nnmaildir "QQmail"       ; each maildir corresponding to a group in Gnus
                        (directory "~/Mail/QQmail/")
                        (directory-files nnheader-directory-files-safe)
                        (get-new-mail nil)))
            gnus-save-newsrc-file nil
            gnus-read-newsrc-file nil
            ;; mail-sources '((maildir :path "~/Mail"))
            ;; gnus-select-method '(nnmaildir "QQmail"
            ;;                                (directory "~/Mail/QQmail/")
            ;;                                (directory-files nnheader-directory-files-safe)
            ;;                                (get-new-mail nil))
            gnus-parameters
            '(("QQmail"
               (to-group . "\\1")
               (visible . t)
               (display . 50))
              )
            )

      (when (configuration-layer/layer-usedp 'org)
        )
      )))

;; (defun xx-email/init-bbdb ()
;;   (use-package bbdb
;;     :defer t
;;     :init
;;     :config
;;     (progn
;;       (setq bbdb-offer-save 1           ; 1 means save-without-asking
;;             bbdb-use-pop-up t           ; allow popups for address
;;             bbdb-electric-p t           ; be disposable with SPC
;;             bbdb-popup-target-lines 1   ; very small
;;             bbdb-dwim-net-address-allow-redundancy t ; alway use full name
;;             ))
;;     ))

;; (defun xx-email/init-wanderlust ()
;;   (use-package wanderlust
;;     :defer t
;;     :init
;;     :config
;;     (progn
;;       (setq elmo-maildir-folder-path "~/Mail"

;;             wl-stay-folder-window t     ; show the folder pane (left)
;;             wl-folder-window-width 25   ; toggle on/off with `i'
;;             ))))

(defun xx-email/init-notmuch ()
  (use-package notmuch
    :init (spacemacs/set-leader-keys "a n" 'notmuch)
    :config
    (progn
      (setq notmuch-init-file (concat dotspacemacs-directory "notmuch-config")
            notmuch-hello-thousands-separator ","
            notmuch-hello-hide-tags '("attachment")
            notmuch-maildir-use-notmuch-insert nil
            notmuch-fcc-dirs '(("zhou.pan@foxmail" . "QQmail/Sent"))

            notmuch-saved-searches
            '((:key "i" :name "inbox" :query "tag:Inbox" :sort-order newest-first)
              (:key "S" :name "Sent" :query "tag:Sent")
              (:key "u" :name "unread" :query "tag:unread")
              (:key "D" :name "Deleted" :query "tag:deleted")
              (:key "d" :name "Draft" :query "tag:draft")
              (:key "a" :name "All" :query "*"))
            notmuch-search-oldest-first nil)

      (defface xx-email-notmuch-hello-header-face
        '((t :foreground "white"
             :background "blue"
             :weight bold))
        "Font for the header in `xx-email//notmuch-hello-insert-searches`."
        :group 'notmuch-faces)

      (setq notmuch-hello-sections
            (list ;; #'notmuch-hello-insert-header
                  ;; #'xx-email//notmuch-hello-insert-mailboxes
                  #'notmuch-hello-insert-alltags
                  #'xx-email//notmuch-hello-insert-searches))
      )))

(defun xx-email/post-init-mu4e ()
  (with-eval-after-load 'mu4e
    (progn
      (spacemacs/set-leader-keys "aM" nil)
      (spacemacs/set-leader-keys "am" 'mu4e)
      (setq mail-user-agent 'mu4e-user-agent

            mu4e-user-mail-address-list '("zhou.pan@foxmail.com"
                                          "18554867@qq.com"
                                          "anyzhou@gmail.com")

            mu4e-maildir "~/Mail"
            mu4e-sent-folder "/QQmail/Sent"
            mu4e-drafts-folder "/QQmail/Drafts"
            mu4e-trash-folder "/QQmail/Deleted"

            ;; mu4e-use-fancy-chars t

            mu4e-headers-fields '((:human-date . 12)
                                  (:flags . 6)
                                  (:mailing-list . 10)
                                  (:from-or-to . 22)
                                  (:subject))

            ;; `offlineimap' is already launched in ~/.profile when system starts up.
            ;; mu4e-get-mail-command "offlineimap"

            mu4e-view-show-images t

            mu4e-view-show-addresses t

            mu4e-update-interval (* 60 5)
            mu4e-html2text-command
            (cond
             ((executable-find "w3m")
              "w3m -dump -cols 80 -T text/html")
             ((executable-find "textutil")
              "textutil -stdin -format html -convert txt -stdout")
             (t
              'html2text))

            mu4e-contexts
            `(,(make-mu4e-context
                :name "QQmail"
                :enter-func (lambda () (mu4e-message "Switch to the QQmail context"))
                ;; leave-func not defined
                :match-func (lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg :to "zhou.pan@foxmail.com")))
                :vars '((user-mail-address . "zhou.pan@foxmail.com")
                        (user-full-name . "周盼")))
              ,(make-mu4e-context
                :name "Gmail"
                :enter-func (lambda () (mu4e-message "Switch to the Gmail context"))
                ;; leave-fun not defined
                :match-func (lambda (msg)
                              (when msg
                                (mu4e-message-contact-field-matches msg :to "anyzhou@gmail.com")))
                :vars '((user-mail-address . "anyzhou@gmail.com")
                        (user-full-name . "周盼")
                        (mu4e-drafts-folder . "/drafts"))))

            ;; set `mu4e-context-policy` and `mu4e-compose-policy` to tweak when mu4e should
            ;; guess or ask the correct context, e.g.

            ;; start with the first (default) context;
            ;; default is to ask-if-none (ask when there's no context yet, and none match)
            ;; (setq mu4e-context-policy 'pick-first)
            mu4e-context-policy 'pick-first
            ;; compose with the current context is no context matches;
            ;; default is to ask
            ;; '(setq mu4e-compose-context-policy nil)
            )

      (evilified-state-evilify-map mu4e-main-mode-map
        :mode mu4e-main-mode
        :bindings
        (kbd "j") #'mu4e~headers-jump-to-maildir)

      (evilified-state-evilify-map mu4e-headers-mode-map
        :mode mu4e-headers-mode
        :bindings
        (kbd "J") #'mu4e~headers-jump-to-maildir
        (kbd "j") #'mu4e-headers-next
        (kbd "k") #'mu4e-headers-prev)

      (evilified-state-evilify-map mu4e-view-mode-map
        :mode mu4e-view-mode
        :bindings
        (kbd "J") #'mu4e~headers-jump-to-maildir
        (kbd "n") #'mu4e-view-headers-next
        (kbd "p") #'mu4e-view-headers-prev
        (kbd "C-n") #'mu4e-view-headers-next-unread
        (kbd "C-p") #'mu4e-view-headers-prev-unread
        (kbd "C-j") #'mu4e-view-headers-next
        (kbd "C-k") #'mu4e-view-headers-prev)
      )))

(defun xx-email/post-init-mu4e-alert ()
  (with-eval-after-load 'mu4e-alert
    ;; Enable Desktop notifications
    (mu4e-alert-set-default-style 'libnotify)))

;; (defun xx-email/post-init-org ()
;;   ;; load org-notmuch when org is actually loaded
;;   (with-eval-after-load 'org (require 'org-notmuch nil 'noerror)))

;;; packages.el ends here
