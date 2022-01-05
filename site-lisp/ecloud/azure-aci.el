;;; azure-aci.el --- Azure Container Instance.  -*- lexical-binding: t; -*-

;; Copyright (C) 2018 The Ecloud Contributors

;; Author: Ramanathan Sivagurunathan <ramzthecoder+ecloud@gmail.com>

;; Version: 0.0.1

;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (magit "2.13.0") (ht "2.2") (s "1.12.0") (pcache "0.4.2"))

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
;; Contains code to handle azure aci.

;;; Code:

(require 'magit)
(require 'ecloud-model)
(require 'ecloud-state)
(require 'ecloud-view)
(require 'ecloud-mode)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-aci--list-command
  '("az" "container" "list")
  "Azure cli for getting aci list.")

(defvar azure-aci-list-view-display-params
  '(name osType resourceGroup location)
  "List of attributes to display in list view.")

;; Model for Azure Container Registry
(ecloud-define-resource-model azure aci)

;; View for Azure Container Registry
(ecloud-setup-resource-view azure aci)

(ecloud-define-cautious-action azure-aci-delete
                               ("az" "container" "delete" "--name" name "--resource-group" resourceGroup "--yes")
                               ("Do you want to delete the container group %s" name ))

(magit-define-popup azure-aci-popup
  "Popup console for aci commands."
  'ecloud
  :actions
  '((?d "Delete" azure-aci-delete))
  :max-action-columns 3)

(defvar magit-azure-aci-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "h" 'azure-aci-popup)
    map)
  "Keymap for the `azure-aci' section.")

(provide 'azure-aci)
;;; azure-aci.el ends here
