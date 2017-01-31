Configuration Scripts
=====================
This is a minimal collection of configuration files and a script to configure a "default" environment.

This project uses the following applications:
 * [Ubuntu 16.10](https://www.ubuntu.com/)
 * [GNU Emacs 25.1.2](https://www.gnu.org/software/emacs/)
 * [GNU Bash 4.3.46](https://www.gnu.org/software/bash/)
 * [Screen 4.04.00](https://www.gnu.org/software/screen/)
 * [htop 2.0.2](http://hisham.hm/htop/)

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
Explain the settings for screen.

# Emacs
Explain the __~/.emacs.d/init.el__ file.
