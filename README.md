# Emacs-Ansilove

<p align="center">
  <a href="https://melpa.org/#/ansilove">
    <img src="https://melpa.org/packages/ansilove-badge.svg">
  </a>
  <a href="https://stable.melpa.org/#/ansilove">
    <img src="https://stable.melpa.org/packages/ansilove-badge.svg">
  </a>
  <a href="https://archive.softwareheritage.org/browse/origin/?origin_url=https://gitlab.com/xgqt/emacs-ansilove">
    <img src="https://archive.softwareheritage.org/badge/origin/https://gitlab.com/xgqt/emacs-ansilove/">
  </a>
  <a href="https://gitlab.com/xgqt/emacs-ansilove/pipelines">
    <img src="https://gitlab.com/xgqt/emacs-ansilove/badges/master/pipeline.svg">
  </a>
</p>

Display buffers as PNG images using ansilove.

<p align="center">
  <img src="logo.png" width="250" height="250">
</p>

## About

Display buffers as PNG images using ansilove.

This package provides some integration with the ansilove tool,
which is a ANSI and ASCII art to PNG converter.

ansilove repository: https://github.com/ansilove/ansilove/

There are three non-ELisp dependencies of this library:
- ansilove
  to convert files to PNG images,
- Emacs built with ImageMagick support
  to display PNG images created by ansilove,
- ImageMagick with PNG file support
  to display PNG files.

To test this library out open one of files from ansilove's examples
(https://github.com/ansilove/ansilove/tree/master/examples/)
and call `ansilove' (M-x ansilove).

## License

    This file is part of xgqt-elisp-lib-ansilove - Ansilove support for GNU Emacs.
    Copyright (c) 2024-2025, Maciej Barć <xgqt@riseup.net>
    Licensed under the GNU GPL v2 License

    xgqt-elisp-lib-ansilove is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    xgqt-elisp-lib-ansilove is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with xgqt-elisp-lib-ansilove.  If not, see <https://www.gnu.org/licenses/>.
