;;; gtd-mode-log.el --- GTD Log Viewer

;; Copyright (C) 2015  kzak

;; Author: kzak
;; URL: https://github.com/kazuakit/gtd-mode
;; Keywords: Getting Things Done Log Viewer
;; Version: 0.1.0

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

;; This package is a major mode for Getting Things Done Log Viewer.

;;; Code:
(defvar gtd-summary-buffer-name
  "*gtd-log-summary*")

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

;;; Keymap
(defvar gtd-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c q") 'gtd-log-kill-buffer)
    (define-key map (kbd "C-c s") 'gtd-log-summary)
    map)
  "Keymap for GTD-LOG major mode")

;;; Mode
(defun gtd-log-mode ()
  (interactive)
  (kill-all-local-variables)

  ;; Keymap
  (use-local-map gtd-log-mode-map)

  (setq major-mode 'gtd-log-mode)
  (setq mode-name "GTD LOG")

  ;; Run hooks
  (run-hooks 'gtd-log-mode-hook))

(provide 'gtd-log-mode)
