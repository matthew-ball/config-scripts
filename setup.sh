#!/bin/bash

version="0.01"
branch="master"
source="https://github.com/matthew-ball/config-scripts.git"
target="~/.config-scripts/"

declare -a source_links=("bashrc" "bash_aliases" "emacs-dir/" "screenrc")
declare -a destination_links=("~/.bashrc" "~/.bash_aliases" "~/.emacs.d" "~/.screenrc")

echo "=========================="
echo "Configuration Scripts $version"
echo "=========================="

function symlink_files {
	for ((i = 0; i < ${#source_links[@]}; i++));
	do
		echo "[info] linking ${source_links[$i]} -> ${destination_links[$i]}"
		echo "- [info] checking if ${destination_links[$i]} exists"
		if [ -f ${destination_links[$i]} -o -d ${destination_links[$i]} ]; then
			echo "- [info] ... file does exist"
		else
			echo "- [info] ... file does not exist"
		fi
	done
}

function print {
	echo "[info] to use setup.sh:"
	echo "-l, --link-files    :: symbolic link files."
	exit
}

if [ -z $1 ]; then
	print
else
	case $1 in
		-l|--link-files    ) symlink_files ;;
		*                  ) print
	esac
fi
