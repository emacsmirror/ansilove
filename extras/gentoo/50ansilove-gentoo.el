(add-to-list 'load-path "@SITELISP@")
(autoload 'ansilove "ansilove"
  "Display current buffer as a PNG image." t)
(autoload 'ansilove-mode "ansilove"
  "Major mode for ANSI image files." t)
(defvar ansilove-supported-file-extensions
  '("adf" "ans" "bin" "idf" "pcb" "tnd" "xb")
  "List of file extensions supported by \"ansilove\".")
(mapc (lambda (ext)
        (add-to-list 'auto-mode-alist
                     `(,(format "\\.%s\\'" ext) . ansilove-mode)))
      ansilove-supported-file-extensions)
