;;; gtd-mode.el --- Getting Things Done on Emacs

;; Copyright (C) 2015  kzak

;; Author: kzak
;; URL: https://github.com/kazuakit/gtd-mode
;; Keywords: Getting Things Done
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

;; This package is a major mode for Getting Things Done on Emacs.

;;; Code:
(require 'cl-lib)
(require 'outline)
(require 'gtd-log-mode)

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
  (gtd-log-mode))

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

;;; Hook
(defvar gtd-mode-hook nil)

;;; Keymap
(defvar gtd-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'gtd-open-item)
    (define-key map (kbd "C-c c") 'gtd-close-item)
    (define-key map (kbd "C-c l") 'gtd-log-item)
    (define-key map (kbd "C-c f") 'gtd-find-log-file)
    (define-key map (kbd "C-t") 'gtd-outline-toggle-subtree)
    (define-key map (kbd "TAB") 'tab-to-tab-stop)
    map)
  "Keymap for GTD major mode")

;;; Face
;; M-x list-faces-display
(defvar gtd-font-lock-keywords
  '(("^[\s\t]*##.*$" . font-lock-keyword-face)
    ("^[\s\t]*#.*$" . font-lock-builtin-face)
    ("^[\s\t]*■.*$" . font-lock-comment-face)))

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
