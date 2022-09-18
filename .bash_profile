alias ls='ls --color'
export PS1='\[\e[35m\]\u@\h:\[\e[0m\]\[\e[32m\]\w\[\e[0m\] \$ '
export EDITOR='vim'
#export BROWSER="firefox"
export BROWSER="brave"
export TERMINAL="alacritty"

if [[ "$(tty)" = "/dev/tty1" ]]; then
  startx
fi

