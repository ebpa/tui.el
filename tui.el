;;; tui.el --- Text-based UI framework modeled after React       -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Erik Anderson

;; Author: Erik Anderson <erik@ebpa.link>
;; Homepage: https://github.com/ebpa/tui.el
;; Keywords: maint
;; Version: 0.0.1
;; Package-Requires: ((emacs "25") (cl-lib "0.6.1") (s "1.12.0") (dash "2.12.0") (dash-functional "1.2.0"))

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

;; Components
(require 'tui-button "components/tui-button.el")
(require 'tui-buffer "components/tui-buffer.el")
(require 'tui-div "components/tui-div.el")
(require 'tui-expander "components/tui-expander.el")
(require 'tui-fixed-width "components/tui-fixed-width.el")
(require 'tui-heading "components/tui-heading.el")
(require 'tui-icon "components/tui-icon.el")
(require 'tui-line "components/tui-line.el")
(require 'tui-link "components/tui-link.el")
(require 'tui-ol "components/tui-ol.el")
(require 'tui-span "components/tui-span.el")
(require 'tui-timer "components/tui-timer.el")
(require 'tui-listview "containers/tui-listview.el")

;; Layout
(require 'tui-absolute "layout/tui-absolute.el")

(require 'tui-demo "demo/tui-demo.el")
(require 'tui-dev)
(require 'tui-reconciler)
(require 'tui-shared-size)
(require 'tui-tabstops)
(require 'tui-util)

(provide 'tui)

;;; tui.el ends here
 
