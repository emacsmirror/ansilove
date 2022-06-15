(add-to-list 'load-path "@SITELISP@")
(autoload 'ansilove "ansilove"
  "Display current buffer as a PNG image." t)
(autoload 'ansilove-mode "ansilove"
  "Major mode for ANSI image files." t)
(mapc (lambda (ext)
        (add-to-list 'auto-mode-alist
                     `(,(format "\\.%s\\'" ext) . ansilove-mode)))
      '("adf" "ans" "bin" "idf" "pcb" "tnd" "xb"))
