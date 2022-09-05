#!/bin/bash
cp ./.bash_profile ~/.bash_profile
cp ./.tmux.conf ~/.tmux.conf
cp ./.vimrc ~/.vimrc

# install light for brightness control
sudo pacman -S light
sudo chmod +s /usr/bin/light
