;;; early-init.el --- Early Init File -*- lexical-binding: t -*-

;; Copyright (c) 2020-2022  Protesilaos Stavrou <info@protesilaos.com>

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs



;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prior to Emacs 27, the `init.el' was supposed to handle the
;; initialisation of the package manager, by means of calling
;; `package-initialize'.  Starting with Emacs 27, the default
;; behaviour is to start the package manager before loading the init
;; file.
;;
;; See my dotfiles: https://git.sr.ht/~protesilaos/dotfiles

;;; Code:

(setq frame-resize-pixelwise t)

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

(dolist (var '(default-frame-alist initial-frame-alist))
  (add-to-list var '(width . (text-pixels . 1920)))
  ;; The height should be 1080, but the panel and the window's
  ;; deocrations reduce the effective value.  If I set 1080 here, Emacs
  ;; maximises the frame regardless of the width value, which I do not
  ;; want.
  (add-to-list var '(height . (text-pixels . 1080))))

;; Initialise installed packages
(setq package-enable-at-startup t)

(defvar package-quickstart)

;; Allow loading from the package cache
(setq package-quickstart t)

;; Init hooks

;; Disable GUI elements
(display-time-mode 1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 8)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-visual-line-mode t)

(global-display-line-numbers-mode 1)

(column-number-mode)
(menu-bar--display-line-numbers-mode-relative)

(setq global-prettify-symbols-mode t) ; Glyph support

(setq-default
 delete-by-moving-to-trash t
 tab-width 4
 window-combination-resize t ; When a new window is created, take space from all existing windows
 x-stretch-cursor t)
(setq load-prefer-newer noninteractive)
(setq use-short-answers t)
(setq undo-limit 80000000 ; Set undo limit to 8MB
	  evil-want-fine-undo t ; Granular changes in insert-mode
	  uniquify-buffer-name-style 'forward ; uniquify buffer names
	  truncate-lines nil
	  indent-tabs-mode t)
(setq user-full-name "Ganfina Brice-Loic"
	  user-mail-address "ganfinab@gmail.com")
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq inhibit-startup-message t)
(setq create-lockfiles nil)

(setq inhibit-splash-screen t)
(setq use-dialog-box t)                 ; only for mouse events
(setq use-file-dialog nil)

(setq inhibit-startup-echo-area-message user-login-name) ; read the docstring
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)

(setq native-comp-async-report-warnings-errors 'silent) ; emacs28 with native compilation

;;; early-init.el ends here
