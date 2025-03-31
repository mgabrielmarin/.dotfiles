# .zshrc
# Mainly for MacOS

# Set vim mode
set -o vi

# Completion
autoload -U compinit
compinit -D

# Defaults
EDITOR='vi'

# Options
force_color_prompt=yes

# Paths
export PATH="$PATH:$HOME/.local/bin"
export PATH="$PATH:$HOME/.local/scripts"
export PATH="$PATH:$HOME/go/bin"
export PATH="$PATH:$HOME/.dotnet/tools"

# Env Vars
# export DOTNET_ROOT="/opt/homebrew/Cellar/dotnet/9.0.3/libexec"
export DOTNET_ROOT="/opt/homebrew/Cellar/dotnet@8/8.0.14_1/libexec"


# Aliases

# Binds
bindkey -s ^f "tmux-sessionizer\n"

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
