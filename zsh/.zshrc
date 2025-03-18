# .zshrc
# Mainly for MacOS

# Set vim mode
set -o vi

# Defaults
EDITOR='vi'

# Options
force_color_prompt=yes

# Paths
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/scripts"

# Binds
bindkey -s ^f "tmux-sessionizer\n"

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
