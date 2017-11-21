#!/bin/bash
if [[ ! -d $HOME/.config/doromochi ]] ; then
    mkdir -p $HOME/.config/doromochi || exit 1
fi

cp -r images ~/.config/doromochi/images && etlas install
