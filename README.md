# gtd-mode

This program is a major mode for Emacs.
To-do management in Getting Things Done paradigm is provided.

## Installation

Download this package and deploy to the Emacs's load-path.

```
cd ~/.emacs.d
git clone https://github.com/kazuakit/gtd-mode.git
```

Add the lines below in ~/.emacs.d/init.el.

```
(add-to-list 'load-path "~/.emacs.d/gtd-mode")
(add-to-list 'auto-mode-alist '("\\.todo$"  . gtd-mode))
(require 'gtd-mode)
```

## Keybinds

### In gtd-mode.

| Key   | Command           | Description                                           |
| ----- | ----------------- | ----------------------------------------------------- |
| C-c o | gtd-open-item  		| Itemize current line (or region).											|
| C-c c | gtd-close-item 		| Close item of current line (or region).								|
| C-c l | gtd-log-item 			| Write log of current line.    												|
| C-c f | gtd-find-log-file | Open the log file in gtd-log-mode.								 		|

### In gtd-log-mode.

| Key   | Command              | Description                                           |
| ----- | -------------------- | ----------------------------------------------------- |
| s 		| gtd-log-summary  		 | Show log summary																			 |
| q 		| gtd-log-kill-buffer  | Close the buffer of log file.												 |


## Screen Capture

Assumed that editing the text file, named filename.todo.

![Screen Capture](https://raw.githubusercontent.com/kazuakit/gtd-mode/master/docs/gtd-mode.gif)
