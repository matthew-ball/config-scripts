Configuration Scripts
=====================
This is a minimal collection of configuration files and a script to configure a "default" environment.

This configuration project assumes the following system and applications:
 * [Ubuntu 17.10](https://www.ubuntu.com/)
 * [GNU Emacs 25.2.2](https://www.gnu.org/software/emacs/)
 * [GNU Bash 4.3.46](https://www.gnu.org/software/bash/)

# Setup Script
The setup script is pretty straight forward; it simply links the files from the project directory to the respective file location in the home directory.

```
 ln -s ~/.config-scripts/bash_aliases ~/.bash_aliases
 ln -s ~/.config-scripts/emacs-dir/ ~/.emacs.d
```

The script is designed to be as unobtrusive as possible. It therefore does not ask the user about removing files (if they exist) before it runs the symbolic linking. This is perhaps counter-intuitive to some, so be forewarned.

# Configurations
The following descriptions assume the previously mentioned linking process has been completed.

## Bash
The __~/bashrc__ file (default from an Ubuntu 17.10 install) sources alias definitions from the file __/.config-scripts/bash_aliases__.

There is an alias for launching screen (primarily to reconnect to existing screen sessions).

There is an alias to launch htop and show only the resources used by $USER.

The remaining aliases are focused around the [apt](https://wiki.debian.org/Apt) package manager; generally, they simplify the syntax and call the original command with **sudo**.

## TODO Remove bashrc from project

## TODO remove Screenrc from project

## Emacs
The __/.config-scripts/emacs-dir/init.el__ file loads the following features and customisations during initialisation which affects the startup time significantly.

The default, vanilla, emacs is somewhat lacking; features are enabled which serve little to no purpose, and features that would benefit the user are disabled. This configuration attempts to implement a set of "better defaults" for emacs.

The following features are disabled:
 * menu-bar-mode
 * tool-bar-mode
 * scroll-bar-mode
 * tooltip-mode

The following features are enabled:
 * file-name-shadow-mode
 * delete-selection-mode
 * show-paren-mode
 * electric-pair-mode
 * column-number-mode
 * global-prettify-symbols-mode
 * global-visual-line-mode
 * midnight-mode
 * recentf-mode
 * savehist-mode
 * save-place-mode
 * desktop-save-mode

The following packages are available from [melpa](http://melpa.milkbox.net/packages/):
 * magit
 * gist
 * markdown-mode
 * undo-tree
 * browse-kill-ring
 * yasnippet
 * auto-complete
 * diminish

When emacs is launched and **package-archive-contents** is nil, these packages will be automatically installed.

Since there are quite a few extra minor modes enabled, the mode-line can easily get cluttered, it becomes convenient to "diminish" these minor modes; keeping visible only what is important, in this case, only the major mode.

Most of the configuration here is focused on [org-mode](http://orgmode.org/).
