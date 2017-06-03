(defun xx-dired/open-file-by-xdg-open ()
  "Try to run `xdg-open' to open the file on this line."
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit))))
    (if (and (spacemacs/system-is-linux) (executable-find "xdg-open"))
        (call-process "xdg-open" nil 0 nil file)
      (message "`xdg-open' is not existed in you system."))))

(defun xx-dired/dired-sort-dirs-before-files ()
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (set-buffer-modified-p nil))

(defun xx-dired/dired-sort-by-size ()
  "sort by Size"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "S")))

(defun xx-dired/dired-sort-by-extension ()
  "sort by eXtension"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "X")))

(defun xx-dired/dired-sort-by-time ()
  "sort by Time"
  (interactive)
  (dired-sort-other (concat dired-listing-switches "t")))

(defun xx-dired/dired-sort-by-name ()
  "sort by Name"
  (interactive)
  (dired-sort-other dired-listing-switches))
