alias ls='ls --color'
export PS1='\[\e[35m\]\u@\h:\[\e[0m\]\[\e[32m\]\w\[\e[0m\] \$ '

# Default programs
export EDITOR='vim'
export TERMINAL="st"
export BROWSER="brave"

# Path adding
export PATH="$HOME/.luarocks/bin:$PATH"
export XINITRC=

#if [[ "$(tty)" = "/dev/tty1" ]]; then
#  startx
#fi

[ "$(tty)" = "/dev/tty1" ] && ! pidfof -s Xorg >/dev/null 2>&1 && exec startx "$XINITRC"

