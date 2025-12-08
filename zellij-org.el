;;; zellij-org.el --- Org-mode integration for zellij.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kyeong Soo Choi

;; Author: Kyeong Soo Choi
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (org "9.0"))
;; Keywords: terminals, tools, processes, org
;; URL: https://github.com/mandoo180/zellij.el

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides Org-mode integration for zellij.el.
;;
;; Features:
;; - Execute source blocks in Zellij panes
;; - Property-based targeting (ZELLIJ_SESSION, ZELLIJ_TAB, ZELLIJ_PANE)
;; - Header argument support (:zellij-session, :zellij-tab, :zellij-pane)
;;
;; Usage:
;;
;;   (require 'zellij-org)
;;
;;   ;; In your org file:
;;   #+PROPERTY: ZELLIJ_TAB 8
;;   #+PROPERTY: ZELLIJ_PANE 0
;;
;;   #+begin_src python
;;   print("Hello from Emacs!")
;;   #+end_src
;;
;;   ;; Execute with: M-x zellij-org-babel-execute
;;
;; Or use header arguments:
;;
;;   #+begin_src python :zellij-tab 8 :zellij-pane 0
;;   print("Hello!")
;;   #+end_src

;;; Code:

(require 'zellij)
(require 'org)

;;;; Property-Based Targeting

(defun zellij-org--get-target-from-properties ()
  "Get Zellij target from org properties.
Checks for: ZELLIJ_SESSION, ZELLIJ_TAB, ZELLIJ_PANE at point, subtree, or file level.
Returns: (SESSION TAB PANE-OFFSET) or nil."
  (let ((session (org-entry-get nil "ZELLIJ_SESSION" t))
        (tab (org-entry-get nil "ZELLIJ_TAB" t))
        (pane (org-entry-get nil "ZELLIJ_PANE" t)))
    (when (and tab pane)
      (list session
            ;; If tab is numeric, format as "Tab #N" to match Zellij's naming
            (if (string-match-p "^[0-9]+$" tab)
                (format "Tab #%s" tab)
              tab)
            (string-to-number pane)))))

;;;; Source Block Execution

(defun zellij-org-babel-execute ()
  "Execute current org source block in Zellij pane.
Uses header arguments or org properties to determine target."
  (interactive)
  (unless (org-in-src-block-p)
    (user-error "Not in a source block"))
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info))
         (params (nth 2 info))
         ;; Check for :zellij-* header args
         (session (alist-get :zellij-session params))
         (tab (alist-get :zellij-tab params))
         (pane (alist-get :zellij-pane params))
         ;; Parse tab - convert numeric strings to "Tab #N" format
         (tab-parsed (when tab
                      (if (numberp tab)
                          (format "Tab #%d" tab)
                        (if (string-match-p "^[0-9]+$" tab)
                            (format "Tab #%s" tab)
                          tab))))
         (pane-parsed (when pane
                       (if (numberp pane) pane
                         (string-to-number pane))))
         ;; Determine target
         (target (or (when (and tab-parsed pane-parsed)
                      (list session tab-parsed pane-parsed))
                    (zellij-org--get-target-from-properties)
                    zellij-target-pane)))
    (unless target
      (user-error "No Zellij target configured. Set properties or use M-x zellij-set-target-pane"))
    (zellij--send-to-target body target)
    (message "Sent to Zellij: %s"
             (or zellij-target-pane-description
                 (format "Tab %s, Pane %d" (nth 1 target) (nth 2 target))))))

(defun zellij-org-send-block ()
  "Send current org source block to Zellij without executing.
Sends the block content as text without adding newline."
  (interactive)
  (unless (org-in-src-block-p)
    (user-error "Not in a source block"))
  (let* ((info (org-babel-get-src-block-info))
         (body (nth 1 info))
         (params (nth 2 info))
         (session (alist-get :zellij-session params))
         (tab (alist-get :zellij-tab params))
         (pane (alist-get :zellij-pane params))
         (tab-parsed (when tab
                      (if (numberp tab)
                          (format "Tab #%d" tab)
                        (if (string-match-p "^[0-9]+$" tab)
                            (format "Tab #%s" tab)
                          tab))))
         (pane-parsed (when pane
                       (if (numberp pane) pane
                         (string-to-number pane))))
         (target (or (when (and tab-parsed pane-parsed)
                      (list session tab-parsed pane-parsed))
                    (zellij-org--get-target-from-properties)
                    zellij-target-pane)))
    (unless target
      (user-error "No Zellij target configured"))
    ;; Send without formatting/execution
    (let ((zellij-format-code-blocks nil))
      (zellij--send-to-target body target))
    (message "Sent block to Zellij")))

;;;; Keybindings

(defvar zellij-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c z e") #'zellij-org-babel-execute)
    (define-key map (kbd "C-c z b") #'zellij-org-send-block)
    map)
  "Keymap for Zellij Org-mode integration.")

;;;###autoload
(define-minor-mode zellij-org-mode
  "Minor mode for Zellij Org-mode integration.

\\{zellij-org-mode-map}"
  :lighter " Z-Org"
  :keymap zellij-org-mode-map)

;;;###autoload
(defun zellij-org-setup ()
  "Set up Zellij Org-mode integration.
Enables `zellij-org-mode' in org-mode buffers."
  (add-hook 'org-mode-hook #'zellij-org-mode))

(provide 'zellij-org)

;;; zellij-org.el ends here
