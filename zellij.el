;;; zellij.el --- Zellij terminal multiplexer integration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Kyeong Soo Choi

;; Author: Kyeong Soo Choi
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: terminals, tools, processes
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

;; This package provides integration between Emacs and Zellij terminal multiplexer.
;;
;; Features:
;; - Send text/commands to specific Zellij panes
;; - Auto-detect AI CLI tools (Claude Code, OpenCode, Aider, Cursor)
;; - Smart code block formatting for AI interactions
;; - Create new panes with commands
;; - Project-aware pane management
;; - Buffer-local target pane tracking
;; - Org-mode source block integration
;; - Mode-line indicator for current target
;;
;; Quick Start:
;;
;;   (require 'zellij)
;;   (global-zellij-mode 1)
;;
;;   ;; Set target pane (with AI CLI auto-detection)
;;   M-x zellij-set-target-pane
;;
;;   ;; Send code to target
;;   C-c z s (zellij-send-region-or-buffer)
;;
;; See README.org for detailed documentation.

;;; Code:

(require 'cl-lib)
(require 'project)

;;;; Customization

(defgroup zellij nil
  "Zellij terminal multiplexer integration."
  :group 'processes
  :prefix "zellij-")

(defcustom zellij-executable "zellij"
  "Path to zellij executable."
  :type 'string
  :group 'zellij)

(defcustom zellij-default-session nil
  "Default session name.
If nil, uses current session from $ZELLIJ environment variable."
  :type '(choice (const :tag "Current session" nil)
                 (string :tag "Session name"))
  :group 'zellij)

(defcustom zellij-default-pane-direction "right"
  "Default direction for new panes."
  :type '(choice (const "right")
                 (const "down")
                 (const "floating"))
  :group 'zellij)

(defcustom zellij-use-project-root t
  "When non-nil, use project root as CWD for new panes.
Falls back to `default-directory' if not in a project."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-prompt-for-pane-name t
  "When non-nil, prompt for pane name when creating new panes."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-default-pane-name-function #'zellij--default-pane-name
  "Function to generate default pane name.
Called with COMMAND as argument, should return string or nil."
  :type 'function
  :group 'zellij)

(defcustom zellij-echo-commands nil
  "When non-nil, echo sent commands to echo area."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-log-buffer-name "*Zellij Log*"
  "Name of the Zellij log buffer."
  :type 'string
  :group 'zellij)

(defcustom zellij-enable-logging t
  "When non-nil, log all Zellij commands to log buffer."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-format-code-blocks t
  "When non-nil, format code with triple backticks for AI CLIs."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-auto-detect-language t
  "When non-nil, auto-detect language for code block formatting."
  :type 'boolean
  :group 'zellij)

(defcustom zellij-ai-cli-patterns
  '(("claude" . "Claude Code")
    ("opencode" . "OpenCode")
    ("aider" . "Aider")
    ("cursor" . "Cursor")
    ("copilot" . "GitHub Copilot")
    ("continue" . "Continue")
    ("cody" . "Sourcegraph Cody"))
  "Alist of (PROCESS-NAME . DISPLAY-NAME) for AI CLI detection.
The order determines priority for auto-detection."
  :type '(alist :key-type string :value-type string)
  :group 'zellij)

(defcustom zellij-pane-targeting-strategy 'navigate
  "Strategy for targeting panes.
- navigate: Navigate to tab then cycle panes (reliable, slower)
- named: Use pane names for targeting (faster if panes are named)"
  :type '(choice (const :tag "Navigation-based" navigate)
                 (const :tag "Name-based" named))
  :group 'zellij)

;;;; Buffer-Local Variables

(defvar-local zellij-target-pane nil
  "Buffer-local target pane: (SESSION TAB PANE-OFFSET).
SESSION: string or nil (current)
TAB: number (index) or string (name)
PANE-OFFSET: number (0 = first pane, 1 = second, etc.)")

(defvar-local zellij-target-pane-description nil
  "Human-readable description of target pane for mode-line.")

;;;; Utility Functions

(defun zellij-available-p ()
  "Return non-nil if Zellij is available."
  (executable-find zellij-executable))

(defun zellij--current-session ()
  "Get current Zellij session from $ZELLIJ environment variable.
Returns nil if not in a Zellij session."
  (getenv "ZELLIJ"))

(defun zellij--effective-session (&optional session)
  "Return effective session name.
Priority: SESSION > `zellij-default-session' > current session."
  (or session
      zellij-default-session
      (zellij--current-session)))

(defun zellij--log (format-string &rest args)
  "Log message to Zellij log buffer if logging is enabled."
  (when zellij-enable-logging
    (let ((msg (apply #'format format-string args))
          (buf (get-buffer-create zellij-log-buffer-name)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert (format "[%s] %s\n"
                       (format-time-string "%Y-%m-%d %H:%M:%S")
                       msg))))))

(defun zellij--error (format-string &rest args)
  "Display error message and log it."
  (let ((msg (apply #'format format-string args)))
    (zellij--log "ERROR: %s" msg)
    (user-error "%s" msg)))

(defun zellij--call (args)
  "Execute zellij with ARGS, return (EXIT-CODE . OUTPUT).
ARGS should be a list of strings."
  (with-temp-buffer
    (let* ((exit-code (apply #'call-process
                            zellij-executable
                            nil t nil
                            args))
           (output (buffer-string)))
      (zellij--log "Command: %s %s" zellij-executable (string-join args " "))
      (zellij--log "Exit code: %d" exit-code)
      (when (> (length output) 0)
        (zellij--log "Output: %s" (string-trim output)))
      (cons exit-code output))))

(defun zellij--call-async (args &optional callback)
  "Execute zellij with ARGS asynchronously.
Call CALLBACK with output when done."
  (let ((proc (apply #'start-process
                    "zellij-async"
                    nil
                    zellij-executable
                    args)))
    (when callback
      (set-process-sentinel
       proc
       (lambda (process event)
         (when (string= event "finished\n")
           (funcall callback (with-current-buffer (process-buffer process)
                              (buffer-string)))))))
    (zellij--log "Async command: %s %s" zellij-executable (string-join args " "))
    proc))

(defun zellij--get-working-directory ()
  "Return appropriate working directory for new panes."
  (if (and zellij-use-project-root (project-current))
      (project-root (project-current))
    default-directory))

(defun zellij--default-pane-name (command)
  "Generate default pane name from COMMAND."
  (when command
    (let ((cmd (car (split-string command))))
      (file-name-nondirectory cmd))))

(defun zellij--get-language-for-mode ()
  "Get language identifier for current major mode."
  (pcase major-mode
    ('emacs-lisp-mode "elisp")
    ('lisp-interaction-mode "elisp")
    ('python-mode "python")
    ('python-ts-mode "python")
    ('ruby-mode "ruby")
    ('ruby-ts-mode "ruby")
    ('javascript-mode "javascript")
    ('js-mode "javascript")
    ('js-ts-mode "javascript")
    ('typescript-mode "typescript")
    ('typescript-ts-mode "typescript")
    ('tsx-ts-mode "tsx")
    ('sh-mode "bash")
    ('bash-ts-mode "bash")
    ('shell-script-mode "bash")
    ('c-mode "c")
    ('c-ts-mode "c")
    ('c++-mode "cpp")
    ('c++-ts-mode "cpp")
    ('rust-mode "rust")
    ('rust-ts-mode "rust")
    ('go-mode "go")
    ('go-ts-mode "go")
    ('java-mode "java")
    ('java-ts-mode "java")
    ('org-mode "org")
    ('markdown-mode "markdown")
    ('json-mode "json")
    ('json-ts-mode "json")
    ('yaml-mode "yaml")
    ('yaml-ts-mode "yaml")
    (_ nil)))

;;;; Session Management

(defun zellij-list-sessions ()
  "Return list of session names."
  (let* ((result (zellij--call '("list-sessions")))
         (output (cdr result)))
    (when (zerop (car result))
      (let ((lines (split-string output "\n" t)))
        (mapcar (lambda (line)
                  ;; Parse: "session-name [Created ...] (status)"
                  (when (string-match "^\\([^ ]+\\)" line)
                    (match-string 1 line)))
                lines)))))

(defun zellij-select-session ()
  "Interactively select a Zellij session."
  (interactive)
  (let ((sessions (zellij-list-sessions)))
    (if sessions
        (completing-read "Session: " sessions nil t)
      (zellij--error "No Zellij sessions found"))))

(defun zellij-session-exists-p (session)
  "Return non-nil if SESSION exists."
  (member session (zellij-list-sessions)))

;;;; Command Sending

(defun zellij-write-chars (text &optional session)
  "Send TEXT to focused pane in SESSION."
  (let ((args (append
               (when session (list "--session" session))
               (list "action" "write-chars" text))))
    (zellij--call args)))

(defun zellij-write-bytes (bytes &optional session)
  "Send BYTES (list of numbers) to focused pane in SESSION."
  (let ((args (append
               (when session (list "--session" session))
               (list "action" "write")
               (mapcar #'number-to-string bytes))))
    (zellij--call args)))

(defun zellij-send-command (command &optional session)
  "Send COMMAND to focused pane and execute (adds newline)."
  (interactive "sCommand: ")
  (zellij-write-chars command session)
  (zellij-write-bytes '(10) session)  ; newline
  (when zellij-echo-commands
    (message "Sent to Zellij: %s" command)))

(defun zellij-send-text (text &optional session)
  "Send TEXT to focused pane without executing."
  (interactive "sText: ")
  (zellij-write-chars text session)
  (when zellij-echo-commands
    (message "Sent text to Zellij")))

(defun zellij-send-lines (lines &optional session)
  "Send LINES (list of strings) to focused pane."
  (dolist (line lines)
    (zellij-write-chars line session)
    (zellij-write-bytes '(10) session)))

;;;; Navigation

(defun zellij-go-to-tab (tab &optional session)
  "Go to TAB (number or string) in SESSION."
  (let ((args (append
               (when session (list "--session" session))
               (list "action")
               (if (numberp tab)
                   (list "go-to-tab" (number-to-string tab))
                 (list "go-to-tab-name" tab)))))
    (zellij--call args)))

(defun zellij-next-tab (&optional session)
  "Go to next tab in SESSION."
  (interactive)
  (zellij--call (append
                (when session (list "--session" session))
                '("action" "go-to-next-tab"))))

(defun zellij-previous-tab (&optional session)
  "Go to previous tab in SESSION."
  (interactive)
  (zellij--call (append
                (when session (list "--session" session))
                '("action" "go-to-previous-tab"))))

(defun zellij-focus-next-pane (&optional session)
  "Focus next pane in SESSION."
  (interactive)
  (zellij--call (append
                (when session (list "--session" session))
                '("action" "focus-next-pane"))))

(defun zellij-focus-previous-pane (&optional session)
  "Focus previous pane in SESSION."
  (interactive)
  (zellij--call (append
                (when session (list "--session" session))
                '("action" "focus-previous-pane"))))

(defun zellij--navigate-to-pane (tab pane-offset &optional session)
  "Navigate to TAB and focus pane at PANE-OFFSET.
Returns non-nil on success."
  (zellij--log "Navigating to tab %s, pane offset %d" tab pane-offset)
  ;; Go to tab
  (let ((tab-result (zellij-go-to-tab tab session)))
    (when (zerop (car tab-result))
      ;; Focus pane by cycling
      (dotimes (_ pane-offset)
        (zellij-focus-next-pane session))
      t)))

;;;; Layout Parsing

(defun zellij--parse-layout (&optional session)
  "Parse current Zellij layout and return structured data.
Returns: ((tab-index tab-name focused-p (pane-info...))...)"
  (let* ((result (zellij--call (append
                                (when session (list "--session" session))
                                '("action" "dump-layout"))))
         (output (cdr result)))
    (when (zerop (car result))
      (zellij--parse-layout-string output))))

(defun zellij--parse-layout-string (layout-str)
  "Parse LAYOUT-STR and extract tab/pane information."
  (let ((tabs '())
        (tab-index 0))
    (with-temp-buffer
      (insert layout-str)
      (goto-char (point-min))
      ;; Find all tab blocks
      (while (re-search-forward "tab name=\"\\([^\"]+\\)\"\\(.*\\){" nil t)
        (let* ((tab-name (match-string 1))
               (tab-attrs (match-string 2))
               (focused-p (string-match-p "focus=true" tab-attrs))
               (tab-start (point))
               (tab-end (save-excursion
                         (backward-char 1)
                         (forward-sexp)
                         (point)))
               (panes (zellij--parse-panes-in-region tab-start tab-end)))
          (push (list tab-index tab-name focused-p panes) tabs)
          (cl-incf tab-index))))
    (nreverse tabs)))

(defun zellij--parse-panes-in-region (start end)
  "Parse panes in region between START and END.
Returns list of pane info: (command args focus-p)."
  (let ((panes '()))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "pane\\(.*\\){" end t)
        (let* ((pane-attrs (match-string 1))
               (focused-p (string-match-p "focus=true" pane-attrs))
               (command nil)
               (args nil))
          ;; Check if this is a plugin pane (skip it)
          (unless (string-match-p "plugin" pane-attrs)
            ;; Look for command
            (when (string-match "command=\"\\([^\"]+\\)\"" pane-attrs)
              (setq command (match-string 1 pane-attrs)))
            ;; Look for args
            (save-excursion
              (when (re-search-forward "args \"\\([^\"]+\\)\"" end t)
                (setq args (match-string 1))))
            ;; Only add non-plugin panes
            (when (or command (not (string-match-p "plugin" pane-attrs)))
              (push (list command args focused-p) panes))))))
    (nreverse panes)))

;;;; AI CLI Detection

(defun zellij--detect-ai-panes (&optional session)
  "Detect panes running AI CLI tools in SESSION.
Returns: ((tab-index pane-offset ai-name)...)."
  (let ((layout (zellij--parse-layout session))
        (ai-panes '()))
    (dolist (tab layout)
      (let ((tab-index (nth 0 tab))
            (panes (nth 3 tab))
            (pane-offset 0))
        (dolist (pane panes)
          (let ((command (nth 0 pane))
                (args (nth 1 pane)))
            ;; Check command and args against AI patterns
            (when-let ((ai-name (zellij--match-ai-pattern command args)))
              (push (list tab-index pane-offset ai-name) ai-panes)))
          (cl-incf pane-offset))))
    (nreverse ai-panes)))

(defun zellij--match-ai-pattern (command args)
  "Match COMMAND and ARGS against AI CLI patterns.
Returns display name if matched, nil otherwise."
  (cl-loop for (pattern . display-name) in zellij-ai-cli-patterns
           when (or (and command (string-match-p pattern command))
                   (and args (string-match-p pattern args)))
           return display-name))

(defun zellij--get-ai-pane-suggestions (&optional session)
  "Get list of AI panes for completing-read.
Returns: ((description . target)...)."
  (let ((ai-panes (zellij--detect-ai-panes session)))
    (mapcar (lambda (pane)
              (let ((tab-index (nth 0 pane))
                    (pane-offset (nth 1 pane))
                    (ai-name (nth 2 pane)))
                (cons (format "%s (Tab #%d, Pane %d)"
                             ai-name tab-index pane-offset)
                      (list session tab-index pane-offset ai-name))))
            ai-panes)))

(defun zellij--should-format-as-code (target)
  "Return non-nil if TARGET is an AI CLI pane."
  (when (and zellij-format-code-blocks target)
    (let* ((tab (nth 1 target))
           (pane (nth 2 target))
           (ai-panes (zellij--detect-ai-panes (nth 0 target))))
      (cl-some (lambda (ai-pane)
                 (and (= (nth 0 ai-pane) tab)
                      (= (nth 1 ai-pane) pane)))
               ai-panes))))

(defun zellij--format-as-code-block (text &optional language)
  "Format TEXT as markdown code block with LANGUAGE."
  (let ((lang (or language
                  (when zellij-auto-detect-language
                    (zellij--get-language-for-mode))
                  "")))
    (format "```%s\n%s\n```" lang text)))

;;;; High-Level Operations

(defun zellij-send-to-pane (tab pane-offset command &optional session)
  "Send COMMAND to specific TAB and PANE-OFFSET in SESSION."
  (interactive
   (list (read-string "Tab (number or name): ")
         (read-number "Pane offset: " 0)
         (read-string "Command: ")))
  (let ((effective-session (zellij--effective-session session))
        (tab-num (if (string-match-p "^[0-9]+$" tab)
                    (string-to-number tab)
                  tab)))
    (when (zellij--navigate-to-pane tab-num pane-offset effective-session)
      (zellij-send-command command effective-session)
      (message "Sent to tab %s, pane %d" tab pane-offset))))

(defun zellij-new-pane (&optional command direction name cwd session)
  "Create new pane in SESSION.
COMMAND: optional command to run
DIRECTION: right/down/floating (default from `zellij-default-pane-direction')
NAME: optional pane name
CWD: working directory (default from `zellij--get-working-directory')"
  (interactive)
  (let* ((dir (or direction zellij-default-pane-direction))
         (pane-cwd (or cwd (zellij--get-working-directory)))
         (pane-name (or name
                       (when zellij-prompt-for-pane-name
                         (read-string "Pane name (optional): "
                                     (funcall zellij-default-pane-name-function
                                             command)))))
         (args (append
                (when session (list "--session" session))
                (list "action" "new-pane")
                (when dir (list "--direction" dir))
                (when pane-name (list "--name" pane-name))
                (when pane-cwd (list "--cwd" pane-cwd))
                (when command (list "--" command)))))
    (let ((result (zellij--call args)))
      (if (zerop (car result))
          (message "Created new pane%s"
                   (if pane-name (format ": %s" pane-name) ""))
        (zellij--error "Failed to create pane: %s" (cdr result))))))

(defun zellij-new-pane-with-command ()
  "Interactively create new pane and optionally run command."
  (interactive)
  (let* ((command (read-string "Command (leave empty for shell): "))
         (direction (completing-read "Direction: "
                                    '("right" "down" "floating")
                                    nil t
                                    zellij-default-pane-direction))
         (cmd (if (string-empty-p command) nil command)))
    (zellij-new-pane cmd direction)))

;;;; Target Pane Management

(defun zellij--get-target (&optional specified-target)
  "Return target pane to use.
Priority: SPECIFIED-TARGET > buffer-local > current pane."
  (or specified-target
      zellij-target-pane
      ;; Default: current session, current tab, current pane
      (list nil nil 0)))

(defun zellij--set-target-manual ()
  "Manually set target pane by prompting for session/tab/pane."
  (let* ((sessions (zellij-list-sessions))
         (session (when (> (length sessions) 1)
                   (completing-read "Session: " sessions nil t)))
         (tab (read-string "Tab (number or name): "))
         (pane-offset (read-number "Pane offset (0 for first): " 0))
         (tab-num (if (string-match-p "^[0-9]+$" tab)
                     (string-to-number tab)
                   tab)))
    (setq zellij-target-pane (list session tab-num pane-offset)
          zellij-target-pane-description (format "Tab %s, Pane %d"
                                                 tab pane-offset))))

(defun zellij-set-target-pane ()
  "Set buffer-local target pane with AI CLI auto-detection."
  (interactive)
  (let ((ai-panes (zellij--get-ai-pane-suggestions)))
    (if ai-panes
        ;; Offer AI panes as suggestions
        (let* ((choices (append ai-panes
                               '(("Manual selection..." . manual))))
               (choice (completing-read "Target pane: "
                                       (mapcar #'car choices)
                                       nil t)))
          (if (string= choice "Manual selection...")
              (zellij--set-target-manual)
            ;; Use selected AI pane
            (let ((target (cdr (assoc choice choices))))
              (setq zellij-target-pane (list (nth 0 target)
                                            (nth 1 target)
                                            (nth 2 target))
                    zellij-target-pane-description (nth 3 target))
              (message "Target set to: %s" zellij-target-pane-description))))
      ;; No AI panes found, use manual selection
      (zellij--set-target-manual))
    (force-mode-line-update)))

(defun zellij-clear-target-pane ()
  "Clear buffer-local target pane setting."
  (interactive)
  (setq zellij-target-pane nil
        zellij-target-pane-description nil)
  (force-mode-line-update)
  (message "Zellij target pane cleared"))

(defun zellij-show-target ()
  "Display currently configured target pane."
  (interactive)
  (if zellij-target-pane
      (message "Target: %s" (or zellij-target-pane-description
                               (format "%s" zellij-target-pane)))
    (message "No target set (using current pane)")))

;;;; Region/Buffer Sending

(defun zellij--send-to-target (text target)
  "Send TEXT to TARGET pane with optional formatting."
  (let* ((session (nth 0 target))
         (tab (nth 1 target))
         (pane-offset (nth 2 target))
         (formatted-text (if (zellij--should-format-as-code target)
                            (zellij--format-as-code-block text)
                          text)))
    (when (zellij--navigate-to-pane tab pane-offset session)
      (zellij-send-command formatted-text session))))

(defun zellij-send-region (start end &optional target)
  "Send region between START and END to TARGET or buffer-local target."
  (interactive "r")
  (let* ((text (buffer-substring-no-properties start end))
         (effective-target (zellij--get-target target)))
    (zellij--send-to-target text effective-target)
    (when zellij-echo-commands
      (message "Sent region to Zellij%s"
               (if zellij-target-pane-description
                   (format " (%s)" zellij-target-pane-description)
                 "")))))

(defun zellij-send-buffer (&optional target)
  "Send entire buffer to TARGET or buffer-local target."
  (interactive)
  (zellij-send-region (point-min) (point-max) target))

(defun zellij-send-region-or-buffer ()
  "Send region if active, otherwise send buffer."
  (interactive)
  (if (use-region-p)
      (zellij-send-region (region-beginning) (region-end))
    (zellij-send-buffer)))

(defun zellij-send-line ()
  "Send current line to target pane."
  (interactive)
  (zellij-send-region (line-beginning-position) (line-end-position)))

(defun zellij-send-paragraph ()
  "Send current paragraph to target pane."
  (interactive)
  (save-excursion
    (let ((start (progn (backward-paragraph) (point)))
          (end (progn (forward-paragraph) (point))))
      (zellij-send-region start end))))

;;;; Interactive Commands

(defun zellij-send-to-pane-interactive ()
  "Interactively send command to specific pane."
  (interactive)
  (let* ((ai-panes (zellij--get-ai-pane-suggestions))
         (choices (append ai-panes
                         '(("Manual selection..." . manual))))
         (choice (completing-read "Target pane: "
                                 (mapcar #'car choices)
                                 nil t))
         (target (if (string= choice "Manual selection...")
                    (progn
                      (zellij--set-target-manual)
                      zellij-target-pane)
                  (let ((t-info (cdr (assoc choice choices))))
                    (list (nth 0 t-info) (nth 1 t-info) (nth 2 t-info)))))
         (command (read-string "Command: ")))
    (zellij--send-to-target command target)))

(defun zellij-send-command-to-target (command)
  "Send COMMAND to buffer-local target or prompt for target."
  (interactive "sCommand: ")
  (if zellij-target-pane
      (zellij--send-to-target command zellij-target-pane)
    (zellij-send-to-pane-interactive)))

(defun zellij-send-to-ai (text)
  "Send TEXT to first detected AI CLI pane."
  (interactive (list (if (use-region-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (read-string "Text to send: "))))
  (if-let ((ai-panes (zellij--detect-ai-panes))
           (first-ai (car ai-panes)))
      (let ((target (list nil (nth 0 first-ai) (nth 1 first-ai))))
        (zellij--send-to-target text target)
        (message "Sent to %s" (nth 2 first-ai)))
    (zellij--error "No AI CLI panes detected")))

(defun zellij-toggle-code-formatting ()
  "Toggle code block formatting for sends."
  (interactive)
  (setq zellij-format-code-blocks (not zellij-format-code-blocks))
  (message "Zellij code formatting: %s"
           (if zellij-format-code-blocks "enabled" "disabled")))

;;;; Log Buffer

(defun zellij-show-log ()
  "Display Zellij log buffer."
  (interactive)
  (let ((buf (get-buffer-create zellij-log-buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'special-mode)
        (special-mode)))
    (display-buffer buf)))

(defun zellij-clear-log ()
  "Clear Zellij log buffer."
  (interactive)
  (when-let ((buf (get-buffer zellij-log-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)))))

;;;; Mode-Line Integration

(defvar zellij-mode-line-format
  '(:eval (when (and zellij-mode zellij-target-pane)
            (propertize (format " [Z:%s]"
                               (or zellij-target-pane-description
                                   (format "%s" zellij-target-pane)))
                       'face 'mode-line-emphasis
                       'help-echo "Zellij target pane")))
  "Mode-line format for Zellij target indicator.")

(defun zellij--setup-mode-line ()
  "Add Zellij indicator to mode-line."
  (unless (member zellij-mode-line-format mode-line-misc-info)
    (push zellij-mode-line-format mode-line-misc-info)))

(defun zellij--teardown-mode-line ()
  "Remove Zellij indicator from mode-line."
  (setq mode-line-misc-info
        (remove zellij-mode-line-format mode-line-misc-info)))

;;;; Minor Mode

(defvar zellij-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Sending
    (define-key map (kbd "C-c z s") #'zellij-send-region-or-buffer)
    (define-key map (kbd "C-c z l") #'zellij-send-line)
    (define-key map (kbd "C-c z p") #'zellij-send-paragraph)
    (define-key map (kbd "C-c z c") #'zellij-send-command-to-target)
    (define-key map (kbd "C-c z a") #'zellij-send-to-ai)

    ;; Target management
    (define-key map (kbd "C-c z t") #'zellij-set-target-pane)
    (define-key map (kbd "C-c z T") #'zellij-clear-target-pane)
    (define-key map (kbd "C-c z ?") #'zellij-show-target)

    ;; Pane creation
    (define-key map (kbd "C-c z n") #'zellij-new-pane-with-command)

    ;; Interactive
    (define-key map (kbd "C-c z g") #'zellij-send-to-pane-interactive)

    ;; Formatting
    (define-key map (kbd "C-c z f") #'zellij-toggle-code-formatting)

    ;; Log
    (define-key map (kbd "C-c z L") #'zellij-show-log)

    map)
  "Keymap for `zellij-mode'.")

;;;###autoload
(define-minor-mode zellij-mode
  "Minor mode for Zellij terminal multiplexer integration.

\\{zellij-mode-map}"
  :lighter (:eval (if zellij-target-pane " Zellij" " zellij"))
  :keymap zellij-mode-map
  (if zellij-mode
      (zellij--setup-mode-line)
    (zellij--teardown-mode-line)))

;;;###autoload
(define-globalized-minor-mode global-zellij-mode
  zellij-mode
  (lambda ()
    (when (zellij-available-p)
      (zellij-mode 1)))
  :group 'zellij)

(provide 'zellij)

;;; zellij.el ends here
