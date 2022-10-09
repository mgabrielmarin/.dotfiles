#!/bin/bash
cp ./.bash_profile ~/.bash_profile
cp ./.tmux.conf ~/.tmux.conf
cp ./.vimrc ~/.vimrc

# install light for brightness control(pacman...)
sudo pacman -S light
sudo chmod +s /usr/bin/light

# add macrandomization with NetworkManager
sudo cp ./conf-files/00-macrandomize.conf ~/etc/NetworkManager/conf.d/
sudo systemctl NetworkManager # Restart NetworkManager service
