;;; tui.el --- Text-based UI framework modeled after React       -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2020 Erik Anderson

;; Author: Erik Anderson <erik@ebpa.link>
;; Homepage: https://github.com/ebpa/tui.el
;; Keywords: maint
;; Version: 0.0.4
;; Package-Requires: ((emacs "26.1") (cl-lib "0.6.1") (s "1.12.0") (dash "2.18.0"))

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

;; An experimental text-based UI framework modeled after React.

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;; Components
(require 'tui-components)

;; Layout
(require 'tui-absolute-container "layout/tui-absolute-container.el")

(require 'tui-demo "demo/tui-demo.el")
(require 'tui-dev)
(require 'tui-errors)
(require 'tui-defun)
(require 'tui-inspect)
(require 'tui-reconciler)
(require 'tui-shared-size)
(require 'tui-snippets)
(require 'tui-tabstops)
(require 'tui-type-helpers)
(require 'tui-util)
(require 'tui-util-ui)

(provide 'tui)
;;; tui.el ends here

