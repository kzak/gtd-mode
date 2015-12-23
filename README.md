# gtd-mode

This program is major mode for Emacs.
Provided very simple Getting Things Done todo management.

## Installation

Download this package and deploy to the Emacs's load-path.

```
git clone https://github.com/kazuakit/gtd-mode.git
cd gtd-mode
cp gtd-mode.el /path/to/elisp-load-path
```

And configure your init.el like below.

```elisp
(require 'gtd-mode)
(add-to-list 'auto-mode-alist '("\\.todo$"  . gtd-mode))
```

## Keybinds

| Key   | Command    | Description                                           |
| ----- | ---------- | ----------------------------------------------------- |
| C-c o | open-item  | Itemize current line (Insert □ in beginning of line) |
| C-c c | close-item | Close item (flip □ to ■)                            |

## Screen Capture

Assumed that editting the text file, named filename.todo.

![Screen Capture](https://raw.githubusercontent.com/kazuakit/gtd-mode/master/docs/gtd-mode.gif)
