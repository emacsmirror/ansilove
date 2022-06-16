# Emacs-Ansilove


## About

Display buffers as PNG images using ansilove.

This package provides some integration with the ansilove tool,
which is a ANSI and ASCII art to PNG converter.

ansilove repository: https://github.com/ansilove/ansilove/

There are three non-ELisp dependencies of this library:
- ansilove
  to convert files to PNG images,
- Emacs built with ImageMagick support
  to dispaly PNG images created by ansilove,
- ImageMagick with PNG fiel support
  to display PNG files.

To test this library out open one of files from ansilove's examples
(https://github.com/ansilove/ansilove/tree/master/examples/)
and call `ansilove` (M-x ansilove).


## License

Copyright (c) 2022, Maciej Barć <xgqt@riseup.net>
Licensed under the GNU GPL v3 License

SPDX-License-Identifier: GPL-3.0-only
