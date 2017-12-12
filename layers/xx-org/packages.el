;;; packages.el --- xx-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 xx
;;
;; Author:  <xx>
;; URL: https://github.com/gyren/dotspacemacsd

;;; Code:
(setq xx-org-packages
      '((ob :location built-in)     ; ob, org are installed by `org-plus-contrib'
        org2web
        org-attach-screenshot
        org-pomodoro
        (org :location built-in)))

;; ob, org-babel
(defun xx-org/post-init-ob ()
  (progn
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '(;;(R . t)
                                   (ditaa . t)
                                   (plantuml . t)
                                   (gnuplot . t)
                                   (latex . t)
                                   (ledger . t)
                                   (shell . t)))

    (setq org-plantuml-jar-path (concat dotspacemacs-directory "assets/plantuml.jar"))))

(defun xx-org/init-org-attach-screenshot ()
  (use-package org-attach-screenshot
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "iS" 'org-attach-screenshot)))

(defun xx-org/init-org2web ()
  (use-package org2web
    :defer t
    :init
    (setq org2web-temporary-directory "~/tmp/org2web")
    :config
    (org2web-add-project
     '("posts"
       :repository-directory "~/org/site-source/"
       :remote (git "https://github.com/gyren/gyren.github.com.git" "master")
       :site-domain "https://gyren.github.io/"
       :site-main-title "②分"
       :site-sub-title "行善不以名，而名从之；名不与利期，而利归之；利不与争期，而争及之。故君子必慎为善。---《列子》"
       :default-category "documents"
       :category-ignore-list ("attachments" "themes")
       :theme-root-directory "~/org/site-theme"
       :theme (worg killjs)
       :force-absolute-url t
       ;:source-browse-url ("GitHub" "https://github.com/gyren/gyren.github.com")
       ;:personal-avatar "/media/img/horse.jpg"
       ;:preparation-function org2web-el2org-preparation-function
       :org-export-function org2web-el2org-org-export-function
       :web-server-docroot "~/tmp/www/"))
    ))

(defun xx-org/post-init-org ()
  (with-eval-after-load 'org
    (progn
      ;; 文件结构
      (setq ; org-directory "~/org" ; default
       org-agenda-files (list "~/org/agenda")
       ; org-default-notes-file (concat org-directory "/agenda/.notes.org")
       )

      ;; list
      (setq org-list-demote-modify-bullet '(("-" . "+")
                                            ("+" . "*")
                                            ("1." . "1)")
                                            ("A." . "a.")
                                            ("A)" . "a)"))
            ;; FIXME the follow should be set before org.el is loaded.
            org-list-allow-alphabetical t
            )

      ;; `TODO' keyword
      (setq org-todo-keywords '((sequence "TODO(t!)" "NEXT(n)" "|" "DONE(d!)")
                                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE" "MEETING")
                                (sequence "DRAFT(D!)" "AMEND(A@/!)" "|" "FINAL(F!)"))
            org-todo-keyword-faces '(("TODO" :foreground "red" :weight bold)
                                     ("NEXT" :background "blue" :weight bold)
                                     ("DONE" :foreground "forest green" :weight bold)
                                     ("WAITING" :foreground "orange" :weight bold)
                                     ("HOLD" :foreground "magenta" :weight bold)
                                     ("CANCELLED" :foreground "forest green" :weight bold)
                                     ("MEETING" :foreground "forest green" :weight bold)))

      (setq org-log-into-drawer t)

      ;; `TAG'
      (setq org-tag-alist '((:startgroup)
                            ("@errand" . ?e)
                            ("@office" . ?o)
                            ("@home" . ?h)
                            ("@farm" . ?f)
                            (:endgroup)
                            ("crypt" . ?c) ; this tag is provided by `org-crypt.el'
                            ("jekyll" . ?j)
                            ("export" . ?x))

            org-tags-exclude-from-inheritance '(crypt))

      ;; `C-j' will be treated as space when export to html. If the last
      ;; character of a line and the begining character of the next line are
      ;; chinese, the html will appear a space between the two characters. The
      ;; following code will diminish the unwanted space. Stolen form chinese
      ;; layer.
      (defadvice org-html-paragraph (before org-html-paragraph-advice
                                            (paragraph contents info) activate)
        "Join consecutive Chinese lines into a single long line without
unwanted space when exporting org-mode to html."
        (let* ((origin-contents (ad-get-arg 1))
               (fix-regexp "[[:multibyte:]]")
               (fixed-contents
                (replace-regexp-in-string
                 (concat
                  "\\(" fix-regexp "\\) *\n *\\(" fix-regexp "\\)") "\\1\\2" origin-contents)))
          (ad-set-arg 1 fixed-contents)))

      ;; org-capture
      (use-package org-capture
        :defer t
        :init
        (setq org-capture-templates
             (quote (("t" "todo" entry (file "~/org/agenda/capture.org")
                      "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                     ("r" "respond" entry (file "~/org/agenda/capture.org")
                      "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
                     ("n" "note" entry (file "~/org/agenda/capture.org")
                      "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                     ("j" "Journal" entry (file+datetree "~/org/journal/journal.org.gpg")
                      "* %?\n%U\n" :clock-in t :clock-resume t)
                     ("w" "org-protocol" entry (file "~/org/agenda/capture.org")
                      "* TODO Review %c\n%U\n" :immediate-finish t)
                     ("m" "Meeting" entry (file "~/org/agenda/capture.org")
                      "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
                     ("p" "Phone call" entry (file "~/org/agenda/capture.org")
                      "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
                     ("h" "Habit" entry (file "~/org/agenda/capture.org")
                      "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n")))))

      ;; crypt
      (use-package org-crypt
        :defer t
        :config
        (org-crypt-use-before-save-magic)
        ;; TODO & FIXME
        (add-hook 'org-mode-hook (lambda ()
                                   (if (assoc "crypt" (org-get-buffer-tags))
                                       (auto-save-mode -1)
                                     (auto-save-mode 1))))
        (add-hook 'org-after-tags-change-hook (lambda ()
                                                (if (assoc "crypt" (org-get-buffer-tags))
                                                    (auto-save-mode -1)
                                                  (auto-save-mode 1))))
        )

      ;; habit
      (add-to-list 'org-modules 'org-habit)

      ;; org-checklist
      (add-to-list 'org-modules 'org-checklist)

      ;; MobileOrg
      (with-eval-after-load 'org-mobile
        ;; 或者本地路径 "~/org/mobileorg/"
        (setq org-mobile-directory "/ssh:xx@45.32.251.39:~/mobileorg/"))

      )))

(defun xx-org/post-init-org-projectile ()
  (with-eval-after-load 'org-agenda
    (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))))

;; (with-eval-after-load 'org-agenda
;;   (progn
;;     (require 'org-projectile)
;;     (setq org-agenda-files (append org-agenda-files (org-projectile:todo-files)))))

(defun xx-org/post-init-org-pomodoro ()
  (with-eval-after-load 'org-pomodoro
    (setq org-pomodoro-start-sound (concat dotspacemacs-directory "assets/bell.wav")
          org-pomodoro-finished-sound (concat dotspacemacs-directory "assets/bell.wav")
          org-pomodoro-short-break-sound (concat dotspacemacs-directory "assets/bell.wav")
          org-pomodoro-long-break-sound (concat dotspacemacs-directory "assets/bell_multiple.wav")
          org-pomodoro-ticking-sound (concat dotspacemacs-directory "assets/tick.wav"))))

;;; packages.el ends here
