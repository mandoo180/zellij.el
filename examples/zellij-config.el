;;; zellij-config.el --- Example configuration for zellij.el -*- lexical-binding: t; -*-

;; This file contains example configurations for zellij.el.
;; Copy and adapt these to your init.el or config.el.

;;; Commentary:

;; This file demonstrates various ways to configure and use zellij.el.

;;; Code:

;;;; Basic Configuration

(use-package zellij
  :load-path "~/path/to/zellij.el"
  :custom
  ;; AI CLI integration (code formatting for AI tools)
  (zellij-format-code-blocks t)
  (zellij-auto-detect-language t)

  ;; Pane creation preferences
  (zellij-default-pane-direction "right")
  (zellij-use-project-root t)
  (zellij-prompt-for-pane-name t)

  ;; UI preferences
  (zellij-enable-logging t)
  (zellij-echo-commands nil)

  ;; AI CLI detection priority (Claude Code first, OpenCode second)
  (zellij-ai-cli-patterns
   '(("claude" . "Claude Code")
     ("opencode" . "OpenCode")
     ("aider" . "Aider")
     ("cursor" . "Cursor")
     ("copilot" . "GitHub Copilot")
     ("continue" . "Continue")
     ("cody" . "Sourcegraph Cody")))

  :config
  (global-zellij-mode 1)

  ;; Optional: Add mode-line indicator to custom position
  (add-to-list 'mode-line-misc-info zellij-mode-line-format))

;;;; Org-Mode Integration

(use-package zellij-org
  :after (zellij org)
  :config
  ;; Auto-enable in org-mode buffers
  (zellij-org-setup)

  ;; Optional: Custom keybinding for org-babel execution
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c z e") #'zellij-org-babel-execute)
    (define-key org-mode-map (kbd "C-c z b") #'zellij-org-send-block)))

;;;; Custom Functions

;; Example: Ask Claude Code a question about selected code
(defun my/ask-claude ()
  "Send region or buffer to Claude Code with a question."
  (interactive)
  (let ((code (if (use-region-p)
                 (buffer-substring-no-properties
                  (region-beginning) (region-end))
               (buffer-string)))
        (question (read-string "Ask Claude: ")))
    (zellij-send-to-ai (format "%s\n\n%s"
                              question
                              (zellij--format-as-code-block code)))))

;; Example: Create a new pane and run project tests
(defun my/zellij-run-tests ()
  "Create new pane and run project tests."
  (interactive)
  (let ((test-cmd (or (project-compile-command)
                     "make test")))
    (zellij-new-pane test-cmd "down" "Tests")))

;; Example: Send to specific AI by name
(defun my/send-to-opencode ()
  "Send region or buffer to OpenCode (if detected)."
  (interactive)
  (let* ((ai-panes (zellij--detect-ai-panes))
         (opencode-pane (cl-find-if
                        (lambda (p) (string= (nth 2 p) "OpenCode"))
                        ai-panes)))
    (if opencode-pane
        (let ((text (if (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))
                     (buffer-string)))
              (target (list nil (nth 0 opencode-pane) (nth 1 opencode-pane))))
          (zellij--send-to-target text target)
          (message "Sent to OpenCode"))
      (user-error "OpenCode not detected in Zellij"))))

;; Example: Custom pane name generator with emojis
(defun my/pane-name-with-emoji (command)
  "Generate pane name with emoji based on COMMAND."
  (when command
    (let ((cmd (car (split-string command))))
      (pcase cmd
        ("npm" "📦 npm")
        ("python" "🐍 python")
        ("ruby" "💎 ruby")
        ("node" "🟢 node")
        ("make" "🔨 make")
        ("cargo" "🦀 cargo")
        ("go" "🐹 go")
        (_ (concat "🚀 " (file-name-nondirectory cmd)))))))

;; Example: Project-specific Zellij setup
(defun my/setup-project-zellij ()
  "Set up Zellij panes for current project."
  (interactive)
  (let ((project-root (project-root (project-current t))))
    (message "Setting up Zellij for project: %s" project-root)
    ;; Create dev server pane
    (zellij-new-pane "npm run dev" "right" "Dev Server")
    (sit-for 0.5)
    ;; Create test watcher pane
    (zellij-new-pane "npm run test:watch" "down" "Tests")
    (sit-for 0.5)
    ;; Go back to main pane
    (zellij-focus-previous-pane)
    (message "Project setup complete!")))

;;;; Custom Keybindings

;; Global keybindings (outside of zellij-mode)
(global-set-key (kbd "C-c a c") #'my/ask-claude)
(global-set-key (kbd "C-c a o") #'my/send-to-opencode)

;; Additional zellij-mode keybindings
(with-eval-after-load 'zellij
  (define-key zellij-mode-map (kbd "C-c z r") #'my/zellij-run-tests)
  (define-key zellij-mode-map (kbd "C-c z P") #'my/setup-project-zellij))

;;;; Advanced Configuration

;; Example: Use specific session for certain projects
(defun my/set-project-zellij-session ()
  "Set Zellij session based on project name."
  (when-let ((project (project-current)))
    (let* ((project-name (file-name-nondirectory
                         (directory-file-name (project-root project))))
           (session-name (concat "proj-" project-name)))
      (when (zellij-session-exists-p session-name)
        (setq-local zellij-default-session session-name)
        (message "Using Zellij session: %s" session-name)))))

;; Auto-detect project session when opening files
(add-hook 'find-file-hook #'my/set-project-zellij-session)

;; Example: Custom language detection for exotic modes
(defun my/get-language-for-mode ()
  "Custom language detection with fallback."
  (or (pcase major-mode
        ('nix-mode "nix")
        ('dockerfile-mode "dockerfile")
        ('terraform-mode "hcl")
        ('gleam-mode "gleam")
        ('zig-mode "zig")
        (_ nil))
      (zellij--get-language-for-mode)))

;; Override default language detection
(advice-add 'zellij--get-language-for-mode :override #'my/get-language-for-mode)

;; Example: Custom pane name function
(setq zellij-default-pane-name-function #'my/pane-name-with-emoji)

;; Example: Disable formatting for specific modes
(defun my/disable-formatting-for-text-modes ()
  "Disable code formatting in text modes."
  (when (derived-mode-p 'text-mode)
    (setq-local zellij-format-code-blocks nil)))

(add-hook 'text-mode-hook #'my/disable-formatting-for-text-modes)

;;;; Integration with Other Packages

;; Example: Integration with projectile
(with-eval-after-load 'projectile
  (defun my/zellij-new-pane-projectile-root (command)
    "Create new pane in projectile root."
    (interactive "sCommand: ")
    (let ((default-directory (projectile-project-root)))
      (zellij-new-pane command))))

;; Example: Integration with magit
(with-eval-after-load 'magit
  (defun my/send-git-diff-to-ai ()
    "Send current git diff to AI for review."
    (interactive)
    (let ((diff (with-temp-buffer
                 (magit-git-insert "diff" "HEAD")
                 (buffer-string))))
      (zellij-send-to-ai
       (format "Please review this git diff:\n\n```diff\n%s\n```" diff)))))

;; Example: Mode-specific auto-targeting
(defun my/auto-target-for-python ()
  "Automatically target Python REPL pane for Python files."
  (when (and (eq major-mode 'python-mode)
            (not zellij-target-pane))
    ;; Look for a pane named "Python" or running python
    (let ((ai-panes (zellij--detect-ai-panes)))
      (when ai-panes
        ;; Use first AI pane as default
        (let ((pane (car ai-panes)))
          (setq-local zellij-target-pane
                     (list nil (nth 0 pane) (nth 1 pane))
                     zellij-target-pane-description
                     (nth 2 pane)))))))

(add-hook 'python-mode-hook #'my/auto-target-for-python)

;;;; Hydra Menu (if you use hydra)

(with-eval-after-load 'hydra
  (defhydra hydra-zellij (:color blue :hint nil)
    "
^Send^             ^Target^           ^Pane^             ^Misc^
^^^^^^^^-----------------------------------------------------------------
_s_: region/buffer  _t_: set target    _n_: new pane      _f_: toggle format
_l_: line          _T_: clear target  _g_: interactive   _L_: show log
_p_: paragraph     _?_: show target   ^ ^                _q_: quit
_a_: send to AI    ^ ^                ^ ^                ^ ^
"
    ("s" zellij-send-region-or-buffer)
    ("l" zellij-send-line)
    ("p" zellij-send-paragraph)
    ("a" zellij-send-to-ai)
    ("t" zellij-set-target-pane)
    ("T" zellij-clear-target-pane)
    ("?" zellij-show-target)
    ("n" zellij-new-pane-with-command)
    ("g" zellij-send-to-pane-interactive)
    ("f" zellij-toggle-code-formatting)
    ("L" zellij-show-log)
    ("q" nil "quit"))

  ;; Bind hydra to a key
  (global-set-key (kbd "C-c z z") #'hydra-zellij/body))

;;;; Minimal Configuration

;; If you just want the basics:

;; (use-package zellij
;;   :load-path "~/path/to/zellij.el"
;;   :config
;;   (global-zellij-mode 1))

;;; zellij-config.el ends here
