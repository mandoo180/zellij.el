# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

zellij.el is an Emacs package that integrates with the Zellij terminal multiplexer. Its primary purpose is sending code from Emacs to Zellij panes, with special support for AI CLI tools (Claude Code, OpenCode, Aider, etc.) including automatic detection and code block formatting.

## Architecture

### Core Files

- **zellij.el** - Main package providing:
  - Zellij command execution via `zellij--call` (synchronous) and `zellij--call-async`
  - Layout parsing to detect tabs/panes (`zellij--parse-layout`)
  - AI CLI detection by matching process names against `zellij-ai-cli-patterns`
  - Buffer-local target pane tracking (`zellij-target-pane`)
  - Navigation-based pane targeting (go to tab, cycle panes)
  - Code block formatting with language detection from major-mode

- **zellij-org.el** - Org-mode integration:
  - Execute source blocks in Zellij panes
  - Property-based targeting (ZELLIJ_TAB, ZELLIJ_PANE, ZELLIJ_SESSION)
  - Header argument support (:zellij-tab, :zellij-pane, :zellij-session)

### Key Design Patterns

- Buffer-local variables (`zellij-target-pane`) allow per-buffer targeting
- AI detection uses pattern matching on Zellij layout dump output
- Code formatting wraps text in markdown fenced code blocks for AI CLIs
- Mode-line integration shows current target via `zellij-mode-line-format`
- Auto-targeting: first send auto-detects and sets current focused pane
- Pane validation: checks pane existence before sending and auto-clears stale targets
- Layout parser filters out plugin panes (tab-bar, status-bar) and container panes

## Development

### Testing Manually

1. Load the package: `M-x load-file RET zellij.el RET`
2. Enable mode: `M-x global-zellij-mode`
3. Check Zellij availability: `M-x zellij-available-p`
4. View debug log: `M-x zellij-show-log`

### Key Commands for Testing

```elisp
;; Check if Zellij is detected
(zellij-available-p)

;; Parse current layout
(zellij--parse-layout)

;; Detect AI panes
(zellij--detect-ai-panes)

;; Test language detection
(zellij--get-language-for-mode)
```

### Requirements

- Emacs 27.1+
- Zellij 0.40.0+
- Org-mode 9.0+ (for zellij-org.el)

## Keybinding Prefix

All keybindings use `C-c z` prefix:
- `C-c z s` - send region/buffer
- `C-c z t` - set target pane
- `C-c z a` - send to AI (auto-detect)
