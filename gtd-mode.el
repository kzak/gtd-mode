;;; gtd-mode.el --- Getting Things Done on Emacs

;; Copyright (C) 2015  kzak

;; Author: kzak
;; URL: https://github.com/kazuakit/gtd-mode
;; Keywords: Getting Things Done
;; Version: 0.0.1

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

;; This package is a major mode for task management, Getting Things Done, on Emacs.

;;; Code:

;;; Variables
(defvar item-opened "□")

(defvar item-closed "■")

(defvar item-regexp
	(format
	 "^\\([ \\|　\\|	]*\\)[%s\\|%s]?"
	 item-opened item-closed))

;;; Functions / Commands
(defun make-item (beg-of-item)
	(interactive)
	(save-excursion
		(if (use-region-p)
				(make-item-on-region
				 (region-beginning) (line-end-position) beg-of-item)
			(make-item-on-region
			 (line-beginning-position) (line-end-position) beg-of-item))))

(defun make-item-on-region (beg end beg-of-item)
	(interactive)
	(save-excursion
		(goto-char beg)
		(beginning-of-line)
		(while (and (< (point) end)
								(re-search-forward item-regexp end t))
			(replace-match (format "\\1%s" beg-of-item)))))

(defun open-item ()
	(interactive)
	(make-item item-opened))

(defun close-item ()
	(interactive)
	(make-item item-closed))

;;; Hook
(defvar gtd-mode-hook nil)

;;; Keymap
(defvar gtd-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map (kbd "C-c o") 'open-item)
		(define-key map (kbd "C-c c") 'close-item)
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

	;; Run hooks
	(run-hooks 'gtd-mode-hook))

(provide 'gtd-mode)

;;; gtd-mode.el ends here
