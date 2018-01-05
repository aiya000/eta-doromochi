#!/bin/bash
if [[ ! -d $HOME/.config/doromochi ]] ; then
    mkdir -p "$HOME/.config/doromochi" || exit 1
fi

echo 'copying resources, please wait...'
cp -r images sounds ~/.config/doromochi && etlas install
