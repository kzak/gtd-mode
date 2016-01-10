;;; gtd-mode.el --- Getting Things Done on Emacs

;; Copyright (C) 2015  kzak

;; Author: kzak
;; URL: https://github.com/kazuakit/gtd-mode
;; Keywords: Getting Things Done
;; Version: 0.3.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is a major mode for Getting Things Done on Emacs.

;;; Code:
(require 'cl-lib)
(require 'outline)

;;; Variables
(defcustom gtd-prefix-todo
  "□"
  "String to indicate an opened item (= a task to do)"
  :type 'string
  :group 'gtd-mode)

(defcustom gtd-prefix-done
  "■"
  "String to indicate a closed item (= a task done)"
  :type 'string
  :group 'gtd-mode)

(defcustom gtd-postfix-important
  "★"
  "String to indicate an important item"
  :type 'string
  :group 'gtd-mode)

(defcustom gtd-prefix-title1
  "#"
  "String to indicate title1 (= 1 means top-level)"
  :type 'string
  :group 'gtd-mode)

(defvar gtd-regexp-title1
  (format "^[\s\t]*%s[\s\t]+\\(.+\\)$" gtd-prefix-title1)
  "Regexp to determin a line of title")

(defcustom gtd-prefix-title2
  "##"
  "String to indicate title1 (= 1 means top-level)"
  :type 'string
  :group 'gtd-mode)

(defvar gtd-regexp-title2
  (format "^[\s\t]*%s[\s\t]+\\(.+\\)$" gtd-prefix-title2)
  "Regexp to determin a line of title")

(defvar gtd-regexp-itemize-target
  (format "^\\([\s\t]*\\)[%s%s]?" gtd-prefix-todo gtd-prefix-done))

(defvar gtd-regexp-log-target
  (format "^[\s\t]*%s\\(.+\\)?" gtd-prefix-done))

(defvar gtd-summary-buffer-name
  "*gtd-log-summary*")

;;; Functions / Commands
(defun gtd-make-item (prefix)
  (interactive)
  (save-excursion
    (if (use-region-p)
        (gtd-make-item-on-region
         (region-beginning) (line-end-position) prefix)
      (gtd-make-item-on-region
       (line-beginning-position) (line-end-position) prefix))))

(defun gtd-make-item-on-region (beg end prefix)
  (interactive)
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (while (and (< (point) end)
                (re-search-forward gtd-regexp-itemize-target end t))
      (replace-match (format "\\1%s" prefix)))))

(defun gtd-open-item ()
  (interactive)
  (gtd-make-item gtd-prefix-todo))

(defun gtd-close-item ()
  (interactive)
  (gtd-make-item gtd-prefix-done))

(defun gtd-log-file-name()
  (format "%s.log" (buffer-file-name)))

(defun gtd-log-item (n)
  (interactive "nWorking hours (ex. 1.5) : ")
  (save-excursion
    (let* ((bol (line-beginning-position))
           (eol (line-end-position))
           (line (buffer-substring-no-properties bol eol)))
      (if (string-match gtd-prefix-done line)
          (progn
            (gtd-write-file (gtd-make-log-msg line n) (gtd-log-file-name))
            (message (format "Logged in %s" (gtd-log-file-name)))
            (beginning-of-line)
            (kill-line))
        (message (format "'%s' isn't closed item. cannnot logged." line))))))

(defun gtd-make-log-msg (line time)
  (let ((t1 (gtd-get-title-string gtd-regexp-title1))
        (t2 (gtd-get-title-string gtd-regexp-title2)))
    (string-match gtd-regexp-log-target line)
    (format "%s,%s,%s,%s,%s\n"
            (format-time-string "%Y,%m,%d")
            time                        ; time spend
            t1                          ; title
            t2                          ; sub-title
            (match-string 1 line))))    ; description

(defun gtd-write-file (s file-name)
  (with-temp-buffer
    (insert s)
    (write-region (point-min) (point-max) file-name t)))

(defun gtd-find-log-file ()
  (interactive)
  (find-file (gtd-log-file-name))
  (gtd-mode))

(defun gtd-get-title-string (regexp-title)
  (interactive)
  (save-excursion
    (if (re-search-backward regexp-title nil t)
        (match-string 1))))

(defun gtd-outline-toggle-subtree ()
  "Show or hide the current subtree depending on its current state."
  (interactive)
  (save-excursion
    (outline-back-to-heading)
    (if (not (outline-invisible-p (line-end-position)))
        (hide-subtree)
      (show-subtree))))

(defun gtd-mark-as-important ()
  (interactive)
  (save-excursion
    (end-of-line)
    (insert (format " %s" gtd-postfix-important))))

(defun gtd-regexp-start-with (str)
  (format "^[\s\t]*%s.*$" str))

(defun gtd-regexp-included (str)
  (format "^.*%s.*$" str))

(defun gtd-regexp-surrounded (str)
  (format "%s[^%s]+\\%s" str str str))

;;; for .gtd.log
(defun gtd-log-kill-buffer ()
  (interactive)
  (save-excursion
    (if (get-buffer gtd-summary-buffer-name)
        (kill-buffer gtd-summary-buffer-name))
    (kill-buffer-and-window)))

(defun gtd-log-summary ()
  (interactive)
  (save-excursion
    (let* ((lines (gtd-log-read-lines (buffer-file-name)))
           (title-times (gtd-log-parse-lines lines))
           (title-subtotal-hash (gtd-log-title-subtotal-hash title-times))
           (fmt-params (gtd-log-fmt-params title-subtotal-hash))
           (summary-elems (gtd-log-summary-elems title-subtotal-hash))
           (summary-lines (gtd-log-summary-lines summary-elems fmt-params))
           (sorted-summary-lines (sort summary-lines 'string<))
           (log-summary-buffer (get-buffer-create gtd-summary-buffer-name)))

      (set-buffer log-summary-buffer)
      (erase-buffer)

      ;; insert the title of summary table
      (insert (gtd-log-table-header fmt-params))
      (insert (gtd-log-hr fmt-params))

      ;; insert bodies of summary table
      (dolist (summary-line sorted-summary-lines)
        (insert summary-line))
      (insert (gtd-log-hr fmt-params))

      ;; insert the total line of summary table
      (insert (format (gtd-log-fmt-body fmt-params)
                      ""
                      (gethash "__total_time__" title-subtotal-hash)
                      100.0))

      (display-buffer (current-buffer)))))

(defun gtd-log-read-lines (file-name)
  (split-string
   (trim (with-temp-buffer
           (insert-file-contents file-name)
           (buffer-string)))
   "\n"))

(defun gtd-log-parse-lines (lines)
  (mapcar (lambda (l)
            (let ((elms (split-string l ",")))
              (list
               (format "%s::%s" (nth 4 elms) (nth 5 elms)) ;title::sub-title
               (nth 3 elms))))                             ; time
          lines))

(defun gtd-log-title-subtotal-hash (title-times)
  (let ((title-subtotal-hash (make-hash-table :test 'equal)))
    (dolist (title-time title-times)
      (let ((title (car title-time))
            (time (float (string-to-number (car (cdr title-time))))))
        ;; Title Length
        (if (< (gethash "__max_title_length__" title-subtotal-hash 0) (length title))
            (puthash "__max_title_length__" (length title) title-subtotal-hash))
        ;; Summation by title
        (puthash title
                 (+ time
                    (gethash title title-subtotal-hash 0.0))
                 title-subtotal-hash)
        ;; Summation of time
        (puthash "__total_time__"
                 (+ time
                    (gethash "__total_time__" title-subtotal-hash 0.0))
                 title-subtotal-hash)))
    (puthash "__max_time_length__"
             (length (number-to-string (gethash "__total_time__" title-subtotal-hash)))
             title-subtotal-hash)
    title-subtotal-hash))

(defun gtd-log-fmt-params (title-subtotal-hash)
  (let ((h (make-hash-table :test 'equal)))
    (puthash "len-title" (gethash "__max_title_length__" title-subtotal-hash) h)
    (puthash "len-time" (gethash "__max_time_length__" title-subtotal-hash) h)
    h))

(defun gtd-log-summary-elems (title-subtotal-hash)
  (let ((summary-elems '()))
    (maphash (lambda (title subtotal)
               (let ((percent
                      (* 100 (/ subtotal (gethash "__total_time__" title-subtotal-hash)))))
                 (if (string-match "^__" title)
                     nil
                   (setq summary-elems
                         (cons (list title subtotal percent) summary-elems)))))
             title-subtotal-hash)
    summary-elems))

(defun gtd-log-summary-lines (summary-elems fmt-params)
  (mapcar (lambda (elem)
            (format (gtd-log-fmt-body fmt-params) (nth 0 elem) (nth 1 elem) (nth 2 elem)))
          summary-elems))

(defun gtd-log-fmt-body (fmt-params)
  (format "%%-%ds  %%%d.1f  %%5.1f\n"
          (gethash "len-title" fmt-params)
          (gethash "len-time" fmt-params)))

(defun gtd-log-fmt-header (fmt-params)
  (format "%%-%ds  %%%ds  %%5s\n"
          (gethash "len-title" fmt-params)
          (gethash "len-time" fmt-params)))

(defun gtd-log-table-header (fmt-params)
  (format (gtd-log-fmt-header fmt-params)
          "task" "hr" "%"))

(defun gtd-log-hr (fmt-params)
  (format (gtd-log-fmt-header fmt-params)
            (make-string (gethash "len-title" fmt-params)  ?-)
            (make-string (gethash "len-time" fmt-params)  ?-)
            (make-string 5 ?-)))

(defun trim-right (s)
  (if (string-match "[\s\t\n\r]+$" s)
      (replace-match "" t t s)
    s))

(defun trim-left (s)
  (if (string-match "^[\s\t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun trim (s)
  (trim-left (trim-right s)))

;;; Hook
(defvar gtd-mode-hook nil)

;;; Keymap
(defvar gtd-mode-map
  (let ((map (make-sparse-keymap)))
    ;;  for .gtd
    (define-key map (kbd "C-c o") 'gtd-open-item)
    (define-key map (kbd "C-c c") 'gtd-close-item)
    (define-key map (kbd "C-c l") 'gtd-log-item)
    (define-key map (kbd "C-c f") 'gtd-find-log-file)
    (define-key map (kbd "C-c m") 'gtd-mark-as-important)
    (define-key map (kbd "C-t") 'gtd-outline-toggle-subtree)

    ;; for .gtd.log
    (define-key map (kbd "C-c q") 'gtd-log-kill-buffer)
    (define-key map (kbd "C-c s") 'gtd-log-summary)

    (define-key map (kbd "TAB") 'tab-to-tab-stop)
    map)
  "Keymap for GTD major mode")

;;; Face
;; M-x list-faces-display
(defvar gtd-font-lock-keywords
  (list
   (cons (gtd-regexp-start-with gtd-prefix-title2) font-lock-keyword-face)
   (cons (gtd-regexp-start-with gtd-prefix-title1) font-lock-builtin-face)
   (cons (gtd-regexp-start-with gtd-prefix-done) font-lock-comment-face)
   (cons (gtd-regexp-included gtd-postfix-important) font-lock-warning-face)
   (cons (gtd-regexp-surrounded "*") font-lock-warning-face)))

;;; Indentation
;; http://www.pement.org/emacs_tabs.htm
(defun gtd-indent-line ()
  (interactive)
  (let ((indent-prev 0)
        (indent-cur (current-indentation)))

    (save-excursion
      (forward-line -1)
      (setq indent-prev (current-indentation)))

    (when (< indent-cur indent-prev)
      (indent-line-to indent-prev))))

;;; Mode
(defun gtd-mode ()
  (interactive)
  (kill-all-local-variables)

  ;; Keymap
  (use-local-map gtd-mode-map)

  ;; font-lock
  (set (make-local-variable 'font-lock-defaults)
       '(gtd-font-lock-keywords))

  ;; Indent
  (set (make-local-variable 'tab-width) 2)
  (set (make-local-variable 'tab-stop-list) '(2 4 6 8 10 12))
  (set (make-local-variable 'indent-line-function) 'gtd-indent-line)

  (setq major-mode 'gtd-mode)
  (setq mode-name "GTD")

  ;; minor-mode
  (setq outline-regexp "[*#\^L]+")
  (outline-minor-mode t)

  ;; Run hooks
  (run-hooks 'gtd-mode-hook))

(provide 'gtd-mode)

;;; gtd-mode.el ends here
