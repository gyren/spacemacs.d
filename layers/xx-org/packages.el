;;; packages.el --- xx-org layer packages file for Spacemacs.
;;
;; Copyright (c) 2016 xx
;;
;; Author:  <xx>
;; URL: https://github.com/gyren/dotspacemacsd

;;; Code:
(setq xx-org-packages
      '(
        ego
        ;; ob, org are installed by `org-plus-contrib'
        (ob :location built-in)
        org2jekyll
        org-attach-screenshot
        ;;org-projectile
        org-pomodoro
        ;;(org-projectile :toggle (configuration-layer/package-usedp 'projectile))
        (org :location built-in)))

;; EGO is a static site generator that depends on Emacs, Git and Org-mode.
;; https://github.com/emacs-china/EGO
(defun xx-org/init-ego ()
    (use-package ego
      :commands (ego-add-to-alist)
      :defer t
      :config
      (ego-add-to-alist 'ego-project-config-alist
                        `(("2fen" ; 站点工程的名字
                           :repository-directory "~/repo/gyren.github.io" ; 站点的本地目录
                           :site-domain "http://gyren.github.io/" ; 站点的网址
                           :site-main-title "2fen" ; 站点的标题
                           :site-sub-title "2fen" ; 站点的副标题
                           :theme (default) ; 使用的主题
                           :summary (("years" :year :updates 10) ("authors" :authors) ("tags" :tags)) ; 导航栏的设置，有 category 和 summary 两种
                           :source-browse-url ("Github" "https://github.com/gyren") ; 你的工程源代码所在的位置
                           :personal-disqus-shortname "emacs-china" ; 使用 disqus 评论功能的话，它的短名称
                           :personal-duoshuo-shortname "emacs-china" ; 使用 多说 评论功能的话，它的短名称
                           :confound-email nil ; 是否保护邮件名称呢？t 是保护，nil 是不保护，默认是保护
                           :ignore-file-name-regexp "readme.org" ; 有些不想发布成 html 的 org 文件（但是又想被导入 git 进行管理），可以用这种正则表达的方式排除
                           :web-server-docroot "~/tmp/gyren.github.io" ; 本地测试的目录
                           :web-server-port 5432); 本地测试的端口

                          ;; 你可以在此添加更多的站点设置
                        ))))

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

(defun xx-org/init-org2jekyll ()
  (use-package org2jekyll
    :defer t
    :init
    (spacemacs/declare-prefix-for-mode 'org-mode "mj" "jekyll")
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "jp" 'org2jekyll-publish
      "jd" 'org2jekyll-create-draft)

    (spacemacs/declare-prefix "aoj" "jekyll")
    (spacemacs/set-leader-keys
      "aojp" 'org2jekyll-publish
      "aojd" 'org2jekyll-create-draft)
    :config
    (progn
      (setq org2jekyll-blog-author "②分"
            org2jekyll-source-directory (expand-file-name "~/org/jekyll-org/")
            org2jekyll-jekyll-directory (expand-file-name "~/jekyll-sites/")
            org2jekyll-jekyll-drafts-dir ""
            org2jekyll-jekyll-posts-dir "_posts/")
      (setq org-publish-project-alist
            (append org-publish-project-alist
                    `(("default"
                       :base-directory ,(org2jekyll-input-directory)
                       :base-extension "org"
                       :publishing-directory ,(org2jekyll-output-directory)
                       :publishing-function org-html-publish-to-html
                       :headline-levels 4
                       :section-numbers nil
                       :with-toc nil
                       :html-preamble t
                       :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                       :recursive t
                       :make-index t
                       :html-extension "html"
                       :body-only t)
                      ("post"
                       :base-directory ,(org2jekyll-input-directory)
                       :base-extension "org"
                       :publishing-directory ,(org2jekyll-output-directory org2jekyll-jekyll-posts-dir)
                       :publishing-function org-html-publish-to-html
                       :headline-levels 4
                       :section-numbers nil
                       :with-toc nil
                       :html-preamble t
                       :html-head "<link rel=\"stylesheet\" href=\"./css/style.css\" type=\"text/css\"/>"
                       :recursive t
                       :make-index t
                       :html-extension "html"
                       :body-only t)
                      ("assets"
                       :base-directory ,(org2jekyll-input-directory "assets")
                       :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php\\|yml\\|html\\|scss\\|ttf\\|el"
                       :publishing-directory ,(org2jekyll-output-directory "assets")
                       :publishing-function org-publish-attachment
                       :recursive t)
                      ("attachments"
                       :base-directory ,(org2jekyll-input-directory "attachments")
                       :base-extension ""
                       :publishing-directory ,(org2jekyll-output-directory "attachments")
                       :publishing-function org-publish-attachment
                       :recursive t)))))))

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
