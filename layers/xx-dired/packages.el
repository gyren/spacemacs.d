;;; packages.el --- xx-dired layer packages file for Spacemacs.
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
;; added to `xx-dired-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `xx-dired/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `xx-dired/pre-init-PACKAGE' and/or
;;   `xx-dired/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(setq xx-dired-packages
      '((dired :location built-in)
        dired+
        dired-rainbow
        dired-narrow
        find-by-pinyin-dired
        peep-dired
        ))

(defun xx-dired/post-init-dired ()
  (spacemacs/set-leader-keys-for-major-mode 'dired-mode
    ;; choose program by `dired-guess-shell-alist-user/default'
    "ox" 'dired-do-shell-command
    "oX" 'xx-dired/open-file-by-xdg-open
    "sn" 'xx-dired/dired-sort-by-name
    "sx" 'xx-dired/dired-sort-by-extension
    "st" 'xx-dired/dired-sort-by-time
    "ss" 'xx-dired/dired-sort-by-size)
  (add-hook 'dired-after-readin-hook 'xx-dired/dired-sort-dirs-before-files)
  (add-hook 'dired-load-hook 'xx-dired/dired-sort-dirs-before-files)

  (use-package dired
    :init
    :config

    (use-package dired-aux
      :config (setq dired-isearch-filenames 'dwim))

    (use-package dired-x
      ;; :diminish dired-omit-mode
      :config
      (add-hook 'dired-mode-hook
                '(lambda ()
                   (dired-omit-mode 1)
                   (setq dired-omit-files "^#\\|^\\..*"
                         dired-omit-size-limit 100000)
                   (setq dired-guess-shell-alist-user
                         (list '("\\.mpe?g$\\|\\.avi$\\|.rmvb$" "gmplayer")
                               '("\\.pdf$" "evince")
                               '("\\.docx?\\'\\|\\.xlsx?\\'\\|\\.pptx?\\'" "xdg-open")))))
      (bind-key ")" 'dired-omit-mode dired-mode-map))

    (with-eval-after-load 'dired-x
      (spacemacs|hide-lighter dired-omit-mode))


))

(defun xx-dired/init-dired+ ()
  (use-package dired+
    :ensure t
    :init
    (put 'dired-find-alternate-file 'disabled nil)
    (setq diredp-hide-details-initially-flag nil)
    :config
    (bind-key (kbd "RET") 'diredp-find-file-reuse-dir-buffer dired-mode-map)
    (bind-key (kbd "<backspace>") 'diredp-up-directory-reuse-dir-buffer dired-mode-map)))

(defun xx-dired/init-dired-rainbow ()
  (use-package dired-rainbow
    :config
    (dired-rainbow-define-chmod executable-unix "Green" "-.*x.*")
    (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "avi" "flac" "ogg"))
    (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
    (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")))

(defun xx-dired/init-dired-narrow ()
  (use-package dired-narrow
    :bind (:map dired-mode-map
                ("/" . dired-narrow))))

;; Stolen from chinese layer.
(defun xx-dired/init-find-by-pinyin-dired ()
  (use-package find-by-pinyin-dired
    :defer t))

(defun xx-dired/init-peep-dired ()
  (use-package peep-dired
    :defer t
    :bind (:map dired-mode-map
                ("P" . peep-dired))))

;;; packages.el ends here
