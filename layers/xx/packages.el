;;; packages.el --- xx layer packages file for Spacemacs.

(defconst xx-packages
  '((cal-china-plus :location local)
    cal-china-x
    ;; ego
    pass
    ;; helm-pass
    ;; auth-password-store
    plantuml-mode
    pyim
    (sdcv-mode :location local)
    (xx-fonts :location local)
    youdao-dictionary))

;; calendar
(defun xx/init-cal-china-plus ()
  (use-package cal-china-plus
    ))

(defun xx/init-cal-china-x ()
  (use-package cal-china-x
    :config
    (setq xx-statutory-holidays '((holiday-fixed 1 1 "元旦")
                                  ;; 除夕
                                  ;; (holiday-chinese-new-year)
                                  (holiday-lunar 1 1 "春节")
                                  (holiday-solar-term "清明" "清明节")
                                  (holiday-fixed 5 1 "劳动节")
                                  (holiday-lunar 5 5 "端午节")
                                  (holiday-lunar 8 15 "中秋节")
                                  (holiday-fixed 10 1 "国庆节"))
          xx-general-holidays '((holiday-lunar 1 15 "元宵节")
                                (holiday-fixed 3 8 "妇女节")
                                (holiday-fixed 3 12 "植树节")
                                (holiday-fixed 5 4 "青年节")
                                (holiday-fixed 6 1 "儿童节")
                                (holiday-lunar 7 7 "七夕")
                                (holiday-fixed 9 10 "教师节")
                                (holiday-lunar 9 9 "重阳"))
          xx-anniversaries '((holiday-lunar 4 30 "我的生日")
                             (holiday-lunar 7 12 "父亲生日")
                             (holiday-lunar 7 24 "王友莲生日")
                             (holiday-lunar 10 29 "母亲生日")
                             (holiday-lunar 11 30 "妹妹生日")
                             (holiday-lunar 3 19 "母亲忌日")
                             (holiday-fixed 5 2 "母亲忌日")
                             (holiday-fixed 5 27 "结婚纪念日"))
          calendar-holidays (append xx-statutory-holidays
                                    xx-general-holidays
                                    xx-anniversaries))

    (defadvice calendar-mark-holidays (around mark-different-holidays activate)
      "Mark holidays with different priorities."
      (let ((calendar-holiday-marker 'xx-anniversaries-face)
            (calendar-holidays xx-anniversaries))
        ad-do-it)
      (let ((calendar-holiday-marker 'xx-statutory-holidays-face)
            (calendar-holidays xx-statutory-holidays))
        ad-do-it)
      (let ((calendar-holiday-marker 'xx-general-holidays-face)
            (calendar-holidays xx-general-holidays))
        ad-do-it)
      (let ((calendar-holidays
             (cl-remove-if (lambda (i)
                             (or (member i xx-anniversaries)
                                 (member i xx-statutory-holidays)
                                 (member i xx-general-holidays)))
                           calendar-holidays)))
        ad-do-it))
    ))

;; password-store
(defun xx/init-pass ()
  (use-package pass
    :defer t
    ))

;; plantuml-mode
(defun xx/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :init
    (setq plantuml-jar-path (concat dotspacemacs-directory "assets/plantuml.jar"))))

;; pyim
(defun xx/init-pyim ()
  (use-package pyim
    ;; :bind
    ;; (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
    ;;  ("C-;" . pyim-delete-word-from-personal-buffer))
    :init
    (progn
      (setq pyim-use-tooltip t
            pyim-page-tooltip 'popup
            pyim-directory (expand-file-name "pyim/" spacemacs-cache-directory)
            pyim-dicts-directory (expand-file-name "assets/pyimdicts/" dotspacemacs-directory)
            pyim-dcache-directory (expand-file-name "dcache/" pyim-directory)
            pyim-personal-file (expand-file-name "pyim-personal.txt" pyim-directory)
            pyim-bigdict-file (expand-file-name "pyim-bigdict.pyim" pyim-dicts-directory)
            pyim-greatdict-file (expand-file-name "pyim-greatdict.pyim" pyim-dicts-directory)
            pyim-dicts `((:name "greatdict" :file ,pyim-greatdict-file)
                         (:name "bigdict" :file ,pyim-bigdict-file))

            default-input-method "pyim"
            pyim-page-length 9

            pyim-english-input-switch-functions
            '(pyim-probe-auto-english
              pyim-probe-dynamic-english
              pyim-probe-evil-normal-mode
              pyim-probe-isearch-mode
              pyim-probe-org-structure-template))

      ;; 让 Emacs 启动时自动加载 pyim 词库
      ;; (add-hook 'emacs-startup-hook
      ;;           #'(lambda () (pyim-restart-1 t)))
      )))

;; sdcv-mode
(defun xx/init-sdcv-mode ()
  (use-package sdcv-mode
    :defer t
    :commands (sdcv-search)
    :init
    (spacemacs/set-leader-keys "os" 'sdcv-search)))

;; xx-fonts
(defun xx/init-xx-fonts ()
  (use-package xx-fonts
    :demand                             ; To override `defer' keyword.
    :commands (xx//set-font xx/increase-fontsize xx/decrease-fontsize)
    :init
    (spacemacs/set-leader-keys "o+" 'xx/increase-fontsize)
    (spacemacs/set-leader-keys "o-" 'xx/decrease-fontsize)
    :config
    ;;(xx//set-font)
    (add-hook 'after-init-hook 'xx//set-font)))

;; youdao-dictionary. Stolen from chinese layer.
(defun xx/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point-tooltip)
    :config
    (setq url-automatic-caching t
          ;; set file path for saving search history
          youdao-dictionary-search-history-file (concat spacemacs-cache-directory ".youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t)))


;;; packages.el ends here
