;;; ansilove.el --- Ansilove integration -*- lexical-binding: t -*-


;; This file is part of emacs-ansilove.

;; emacs-ansilove is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, version 3.

;; emacs-ansilove is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with emacs-ansilove.  If not, see <https://www.gnu.org/licenses/>.

;; Copyright (c) 2022, Maciej Barć <xgqt@riseup.net>
;; Licensed under the GNU GPL v3 License
;; SPDX-License-Identifier: GPL-3.0-only


;; Author: Maciej Barć <xgqt@riseup.net>
;; Homepage: https://gitlab.com/xgqt/emacs-ansilove/
;; Version: 0.0.0
;; Package-Requires: ((emacs "26.1"))



;;; Commentary:


;; Use ansilove from GNU Emacs.

;; This package provides some integration with the ansilove tool,
;; which is a ANSI and ASCII art to PNG converter.

;; ansilove repository: https://github.com/ansilove/ansilove/

;; To test this library out open one of files from ansilove's examples
;; (https://github.com/ansilove/ansilove/tree/master/examples/)
;; and call `ansilove' (M-x ansilove).



;;; Code:


;; Custom variables

(defgroup ansilove nil
  "Ansilove integration."
  :group 'external
  :group 'image
  :group 'text)

(defcustom ansilove--ansilove-executable "ansilove"
  "Path or name to the \"ansilove\" executable."
  :safe 'stringp
  :type 'file
  :group 'ansilove)

(defcustom ansilove--temporary-directory
  (file-name-as-directory
   (expand-file-name (concat "." user-full-name "_Emacs_ansilove")
                     temporary-file-directory))
  "Temporary directory path used for file conversion via \"anilove\"."
  :safe 'stringp
  :type 'file
  :group 'ansilove)


;; Helper functions

(defun ansilove--init-temporary-directory ()
  "Ensure ANSILOVE--TEMPORARY-DIRECTORY is writable."
  (when (not (file-exists-p ansilove--temporary-directory))
    (with-temp-buffer
      (make-directory ansilove--temporary-directory))))

;; TODO: Completion-read of conversion method
;;       before calling `ansilove--convert-file-to-png'.

(defun ansilove--convert-file-to-png (input-file output-file)
  "Wrapper for calling ANSILOVE--ANSILOVE-EXECUTABLE.
Calls ANSILOVE--ANSILOVE-EXECUTABLE given INPUT-FILE as input and
OUTPUT-FILE as output."
  (let ((output-buffer (get-buffer-create "Ansilove-Process"))
        (error-buffer (get-buffer-create "Ansilove-Error")))
    (shell-command (format "%s -o %s %s"
                           ansilove--ansilove-executable
                           output-file
                           input-file)
                   output-buffer
                   error-buffer)))

(defun ansilove--buffer-to-png (buffer)
  "Give BUFFER contents to \"ansilove\".
If BUFFER is a file take the BUFFER's file as input,
else save buffer to a temporary file and
feed that file to `ansilove--convert-file-to-png'.
Returns a path to a file in ANSILOVE--TEMPORARY-DIRECTORY."
  (ansilove--init-temporary-directory)
  (let* ((buffer-file-path (buffer-file-name buffer))
         (temporary-name
          (concat "ansilove_" (number-to-string (abs (random)))))
         (temporary-output
          (expand-file-name (concat temporary-name ".png")
                            ansilove--temporary-directory)))
    (cond
     (buffer-file-path
      (ansilove--convert-file-to-png buffer-file-path temporary-output))
     (t
      (let* ((temporary-input-name (concat temporary-name ".txt"))
             (temporary-input
              (expand-file-name temporary-input-name
                                ansilove--temporary-directory)))
        (with-current-buffer buffer
          (write-file temporary-input))
        (ansilove--convert-file-to-png temporary-input temporary-output)
        (kill-buffer temporary-input-name)
        (delete-file temporary-input))))
    temporary-output))

(defun ansilove--convert-and-disply-now ()
  "Convert current buffer with \"ansilove\" and display the results."
  (find-file (ansilove--buffer-to-png (current-buffer))))


;; Main provided features

;; TODO: Add a special mode: before buffer is closed, delete the file it holds.

;;;###autoload
(defun ansilove ()
  "Display current file as PNG image using the \"ansilove\" tool."
  (interactive)
  (ansilove--init-temporary-directory)
  (ansilove--convert-and-disply-now))


(provide 'ansilove)



;;; ansilove.el ends here
