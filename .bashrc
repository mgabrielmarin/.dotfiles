#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Prompt
PS1='[\u@\h \W]\$ '

# vi mode
set -o vi

# History
# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Aliases

# ls
alias ls='ls --color=auto'
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# dir
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

# diff
alias diff='diff --color=auto'

# Completion
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

# Defaults
export EDITOR="vi"
export TERMINAL="gnome-terminal"
export TERMINAL_PROG="gnome-terminal"
export BROWSER="chrome"

# Paths
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:/usr/local/bin"
export PATH="$PATH:/opt/nvim-linux-x86_64/bin"
export PATH="$PATH:/usr/local/go/bin"
export PATH="$PATH:$HOME/.local/scripts"
export PATH="$PATH:$HOME/.cargo/bin"

# Environment variables

# Lynx
export LYNX_CFG="$HOME/.config/lynx/lynx.cfg"

# Add fzf to bash
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Binds
bind -x '"\C-f": tmux-sessionizer'
