Configuration Scripts
=====================
This is a minimal collection of configuration files and a script to configure a "default" environment.

This project uses the following applications:
 * [Ubuntu 16.10]()
 * [GNU Emacs 25.1.2]()
 * [GNU Bash 4.3.46]()
 * [Screen 4.04.00]()
 * [htop 2.0.2]()

# Setup Script
The setup script is pretty straight forward:

It symbolically links the files from the project directory to the respective file location in the home directory.

```
 ln -s ~/.config-scripts/bashrc ~/.bashrc
 ln -s ~/.config-scripts/bash_aliases ~/.bash_aliases
 ln -s ~/.config-scripts/screenrc ~/.screenrc
 ln -s ~/.config-scripts/emacs-dir/ ~/.emacs.d
```

The script is designed to be as unobtrusive as possible. It therefore does not ask about removing files (if they exist) before it runs the symbolic linking. This is perhaps counter-intuitive to some, so be forewarned.

# Bash
As this project uses the default __~/.bashrc__ file from an Ubuntu 16.10 install, I will not comment further upon it, nor document it in this project documentation, except to mention that this file adds alias definitions from the file __~/.bash_aliases__ (if it exists). The main purpose of this bash configuration is the aliases.

## TODO Remove bashrc from project

# Screen
Simple settings for screen.

# Emacs
A default __~/.emacs.d/init.el__ file (which pulls in everything required) and the directory __~/.emacs.d/snippets/__ for [yasnippet]().
