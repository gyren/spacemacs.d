;;; packages.el --- xx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <xx>
;; URL: https://github.com/gyren/dotspacemacsd
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
;; added to `xx-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `xx/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `xx/pre-init-PACKAGE' and/or
;;   `xx/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst xx-packages
  '((cal-china-plus :location local)
    cal-china-x
    ;; chinese-fonts-setup
    ;; ego
    plantuml-mode
    (sdcv-mode :location local)
    youdao-dictionary)
  "The list of Lisp packages required by the xx layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;; plantuml-mode
(defun xx/init-plantuml-mode ()
  (use-package plantuml-mode
    :defer t
    :init
    (setq plantuml-jar-path (concat dotspacemacs-directory "assets/plantuml.jar"))))

;; sdcv-mode
(defun xx/init-sdcv-mode ()
  (use-package sdcv-mode
    :defer t
    :commands (sdcv-search)
    :init
    (spacemacs/set-leader-keys "os" 'sdcv-search)))

;; youdao-dictionary. Stolen from chinese layer.
(defun xx/init-youdao-dictionary ()
  (use-package youdao-dictionary
    :defer
    :init
    (spacemacs/set-leader-keys "oy" 'youdao-dictionary-search)
    :config
    (setq url-automatic-caching t
          ;; set file path for saving search history
          youdao-dictionary-search-history-file (concat spacemacs-cache-directory ".youdao")
          ;; Enable Chinese word segmentation support
          youdao-dictionary-use-chinese-word-segmentation t)))

;; calendar
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

(defun xx/init-cal-china-plus ()
  (use-package cal-china-plus
    ))

;; chinese-fonts-setup
;; (defun xx/init-chinese-fonts-setup ()
;;   (use-package chinese-fonts-setup
;;     :init
;;     (setq cfs-profiles '("normal" "prog" "org-mode")
;;           cfs-profiles-directory (concat dotspacemacs-directory "chinese-fonts-setup/"))))

;;; packages.el ends here
