(setq-default fill-column 80)

;; (spacemacs/declare-prefix "o" "XX's own")

;; (spacemacs/declare-prefix "ot" "Toggle")

(when (string-equal system-type "windows-nt")
  (let ((xx-win-paths
         `(,(concat xx-win-driver-letter ":/msys2/usr/bin")
           ,(concat xx-win-driver-letter ":/msys2/mingw64/bin")
           ,(concat xx-win-driver-letter ":/msys2/mingw32/bin"))))

    (setenv "PATH" (mapconcat 'identity xx-win-paths ";"))

    (setq exec-path (append xx-win-paths exec-path))))

(spacemacs|add-toggle iimage
  :status iimage-mode
  :on (iimage-mode)
  :off (iimage-mode -1)
  :documentation "Enable iimage mode"
  :evil-leader "oti")

;; calendar
(setq calendar-latitude +24.52
      calendar-longitude +117.35
      calendar-location-name "ZhangZhou"
      calendar-week-start-day 1
      calendar-mark-holidays-flag t)

(setq holiday-christian-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-bahai-holidays nil
      holiday-general-holidays nil)

(setq calendar-day-abbrev-array ["日" "一" "二" "三" "四" "五" "六"])

(defface xx-anniversaries-face
  '((((class color) (background light))
     :background "blue")
    (((class color) (background dark))
     :background "blue")
    (t
     :inverse-video t))
  "Face for indicating `xx-anniversaries'.")

(defface xx-statutory-holidays-face
  '((((class color) (background light))
     :background "red")
    (((class color) (background dark))
     :background "red")
    (t
     :inverse-video t))
  "Face for indicating `xx-statutory-holidays'.")

(defface xx-general-holidays-face
  '((((class color) (background light))
     :background "green"
     :foreground "black")
    (((class color) (background dark))
     :background "green"
     :foreground "black")
    (t
     :inverse-video t))
  "Face for indicating `xx-general-holidays'.")

