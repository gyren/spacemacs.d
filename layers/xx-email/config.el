(setq user-full-name "周盼"
      user-mail-address "zhou.pan@foxmail.com"
      message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      ;; mail-host-address ""
      ;; mail-specify-envelope-from t
      ;; mail-envelope-from 'header
      message-sendmail-envelope-from 'header)
(setq mm-text-html-renderer 'shr)
