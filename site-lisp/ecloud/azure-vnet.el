;;; azure-vnet.el --- Azure Vnet.  -*- lexical-binding: t; -*-

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
;; Contains code to handle azure vnet

;;; Code:

(require 'ecloud-model)
(require 'ecloud-state)
(require 'ecloud-view)
(require 'ecloud-mode)
(require 'magit)
(require 'subr-x)
(require 'eieio)
(eval-when-compile (require 'cl))

(defvar azure-vnet--list-command
  '("az" "network" "vnet" "list")
  "Azure cli for getting vnet list.")

(defvar azure-vnet-list-view-display-params
  '(name address-list location)
  "List of attributes to display in list view.")


(defvar azure-subnet-list-view-display-params
  '(name addressPrefix)
  "List of attributes to display in list view.")

;; Model for Azure Vnet, subnet
(ecloud-define-resource-model azure vnet)
(ecloud-define-resource-model azure subnet)

;; View for Azure Vnet
(ecloud-setup-resource-view azure vnet)

;;;; Hooks
(defcustom azure-vnet-parser-hook
  '(azure-vnet--parse-address-space
    azure-vnet--parse-subnets)
  "Hook to run for parsing address space."
  :group 'ecloud-azure
  :type 'hook)

(defcustom azure-vnet-detailed-view-hook
  '(azure-vnet-add-detailed-view-section)
  "Hook to run for adding a detailed view."
  :group 'ecloud-azure
  :type 'hook)

;;;; Parsing
(defun azure-vnet--parse-address-space (robj)
  "Function to parse the address space for the vnet in the response `ROBJ."
  (-let* (((&alist 'addressSpace (&alist 'addressPrefixes address-list))
           (oref robj attributes))
          (address-list-str (string-join address-list ","))
          )
    (oset robj :attributes (append (oref robj attributes)
                                   `((address-list . ,address-list-str))))
    ))

(defun azure-vnet--parse-subnets (robj)
  "Function to parse subnets for the vnet `ROBJ."
  (-let* (((&alist 'subnets subnets) (oref robj attributes))
          (ts (ecloud-get-resource-type-modified-ts "azure" "vnet"))
          (parsed-data (ecloud-parse-resource-data subnets "azure-subnet" ts t)))
    (--map (progn
             (ecloud-resource-add-has robj "azure-subnet" (ecloud-resource-id it))
             (ecloud-resource-add-belongs-to it "azure-vnet" (ecloud-resource-id robj)))
           parsed-data)))

;;;; Detailed View
(defun azure-vnet-add-detailed-view-section (robj)
  "Function to add detailed view section for the vnet.`ROBJ is the object for the vnet."
  (ecloud-insert-list-views 'azure '(subnet) robj))

;;;; Actions
(ecloud-define-cautious-action azure-vnet-delete-vnet
                               ("az" "network" "vnet" "delete" "--ids" id "--output" "json")
                               ("Do you want to delete vnet %s" name ))

(defvar magit-azure-vnet-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "p" 'azure-overview-print-section)
    (define-key map "d" 'azure-vnet-delete-vnet)
    (define-key map [tab] 'magit-section-cycle)
    map)
  "Keymap for the `azure-vnet-section'.")

(provide 'azure-vnet)
;;; azure-vnet.el ends here
