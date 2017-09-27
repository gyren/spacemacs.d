;;; xx-fonts.el --- 设置字体。

;; {%org-mode%}
;; (xx//set-font "normal" "prog") xx-current-profile xx-current-fontsize
;; (xx//set-font "tiny") (xx//set-font "small") (xx//set-font "normal")
;; (xx//set-font "large") (xx//set-font "huge")
;; | aaaaaaaaaaaaaaaaaa | AAAAAAAAAAAAAAAAAA |
;; | ~!@#$%^&*()+-=,.;: | { }[ ]< >` '"?\ /_ |
;; | 仿佛兮若轻云之蔽月 | 飘飘兮若流风之回雪 |
;; | 《》「」“”‘’？ | ，。；：！～￥—×…|
;; | 𠄀𠄁𠄂𠄄𠄅𠄆𠄇𠄈𠄉 | 𠄀𠄁𠄂𠄄𠄅𠄆𠄇𠄈𠄉 |
;;
;; {%/org-mode%}

(eval-when-compile
  (require 'cl))

;; When use `font-spec' function, if the `size' is an integer, means in pixels,
;; float in points. So, both in same time will cause confuse. You'd better use
;; integer.
(defconst xx-font-profiles
  '(("prog" (("fonts" . ("Source Code Pro" "Microsoft Yahei" "SimSun-ExtB"))
             ("tiny" . (10 12 12))
             ("small" . (12 14 14))
             ("normal" . (14 16 16))
             ("large" . (16 20 20))
             ("huge" . (18 22 22))))
    ("read" (("fonts" . ("Bitstream Vera Sans Mono" "KaiTi" "SimSun-ExtB"))
             ("tiny" . (10 12 12))
             ("small" . (12 14 14))
             ("normal" . (14 16 16))
             ("large" . (16 20 20))
             ("huge" . (18 22 22)))))
  "Define sets of fonts name and respectively size.
Fontsets including `EN' `CN' `ExtB' fonts.
The default profile is the first one.
And the structure of all profiles must be
same in order to rescale font correctly")

(defvar xx-font-size "normal"
  "The value must be one of something defined in `xx-font-profiles'.
This variable could be modified according to resolution by function `xx//set-fontsize-by-resolution'.")

(defcustom xx-set-fontsize-by-resolution t
  "If non-nil, emacs will startup with proper font size according to screen resolution.")

(defvar xx-current-profile nil)
(defvar xx-current-fontsize nil)

(defun xx//get-fonts-profile ()
  "Get name of profiles."
  (let (name)
    (dolist (profile xx-font-profiles)
             (push (car profile) name))
    (reverse name)))

(defun xx//get-the-profile-fontlist (profile)
  "Get the fontlist including fontnames and fontsize of the profile"
  (if (member profile (xx//get-fonts-profile))
      (cadr (assoc profile xx-font-profiles))
    (message "The profile `%s' is not exist, you should define it in `xx-font-profiles' firstly."
             profile)))

(defun xx//get-the-profile-font-sizes (profile)
  "Get the profile's font sizes defined in `xx-font-profiles'."
  (let* ((font-sizes (cdr (xx//get-the-profile-fontlist profile)))
         sizes)
    (dolist (size font-sizes)
      (push (car size) sizes))
    (reverse sizes)))

(defun xx//set-font (&optional size profile)
  "Set the fontset with the profile.
If the profile is not given,use the first profile in `xx-font-profiles'."
  (let* ((size (or size xx-current-fontsize xx-font-size))
         (profile (or profile xx-current-profile (car (xx//get-fonts-profile))))
         (fontlist (xx//get-the-profile-fontlist profile))
         (fonts (cdr (assoc "fonts" fontlist)))
         (rsize (cdr (assoc size fontlist)))
         (en-fontname (nth 0 fonts))
         (zh-fontname (nth 1 fonts))
         (extb-fontname (nth 2 fonts))
         (en-size (nth 0 rsize))
         (zh-size (nth 1 rsize))
         (extb-size (nth 2 rsize))
         (en-main-fontspec (font-spec :name en-fontname
                                      :size en-size
                                      :weight 'normal
                                      :slant 'normal))
         (en-bold-fontspec (font-spec :name en-fontname
                                      :size en-size
                                      :weight 'bold
                                      :slant 'normal))
         (en-italic-fontspec (font-spec :name en-fontname
                                        :size en-size
                                        :weight 'normal
                                        :slant 'italic))
         (en-bold-italic-fontspec (font-spec :name en-fontname
                                             :size en-size
                                             :weight 'bold
                                             :slant 'italic))
         (en-symbol-fontspec (font-spec :name en-fontname
                                        :size en-size
                                        :weight 'normal
                                        :slant 'normal))
         (zh-main-fontspec (font-spec :name zh-fontname
                                      :size zh-size
                                      :weight 'normal
                                      :slant 'normal))
         (zh-symbol-fontspec (font-spec :name zh-fontname
                                        :size zh-size
                                        :weight 'normal
                                        :slant 'normal))
         (zh-extb-fontspec (font-spec :name extb-fontname
                                      :size extb-size
                                      :weight 'normal
                                      :slant 'normal)))
    ;; set English font.
    (set-face-attribute 'default nil :font en-main-fontspec)
    (set-face-font 'bold en-bold-fontspec)
    (set-face-font 'italic en-italic-fontspec)
    (set-face-font 'bold-italic en-bold-italic-fontspec)
    ;; 设置中文字体，不能使用 `unicode' 字符集，否则将覆盖上述英文字体的设置。
    (dolist (charset '(kana han cjk-misc bopomofo gb18030))
      (set-fontset-font t charset zh-main-fontspec))
    ;; 设置 Ext-B 字体。
    (set-fontset-font t nil zh-extb-fontspec nil 'prepend)
    (setq xx-current-profile profile
          xx-current-fontsize size)
    (message "Your font profile is `%s' and size is `%s'." profile size)
    ))

(defun xx//set-fontsize-by-resolution ()
  "Set font relative size according to resolution.
`HR' is the abbreviation of `horizontal resolution'.
0 - 1024 -- 1440 -- 1920 -- 2560 -- ∞
tiny | small | normal | large | huge"
  (let* ((HR-list '(0 1024 1440 1920 2560))
         (l (length HR-list))
         (rsize-list (xx//get-the-profile-font-sizes (car (xx//get-fonts-profile))))
         (n 0))
    (while (and (< (nth n HR-list) (frame-pixel-width))
                (< n l))
      (setq n (+ n 1)))
    (setq xx-font-size (nth (- n 1) rsize-list))))

(defun xx//rescale-fontsize (step)
  "Rescale font size.
TODO & FIXME"
  (let* ((size-list (xx//get-the-profile-font-sizes xx-current-profile))
         (l (length size-list)))
    (when (< step 0)
      (setq size-list (reverse size-list)))
    (setq next-size
          (cadr (member xx-current-fontsize size-list)))
    (when next-size
      (xx//set-font next-size))))

(defun xx/increase-fontsize ()
  "Increase font size."
  (interactive)
  (xx//rescale-fontsize +1))

(defun xx/decrease-fontsize ()
  "Decrease font size."
  (interactive)
  (xx//rescale-fontsize -1))

(defun xx/change-font-profile (profile)
  "Change font profile interactively.
TODO & FIXME"
  (interactive ; "sYou current font profile is `', which profile would you select?")
   (list (read-string (format "The current font profile is `%s', which profile would you select: " xx-current-profile)
                (cadr (member xx-current-profile (xx//get-fonts-profile))) ;;TODO & FIXME
                )))
  (message "You changed font profile from `%s' to `%s'." xx-current-profile profile)
  (xx//set-font nil profile)
  (setq xx-current-profile profile))

(defun xx//font-exist-p (font)
  (and (x-list-fonts font) t))

(provide 'xx-fonts)
