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

# GNU
# GNU gsed in favor of BSD sed needs to be installed in order for emacs man command to work
export PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"

# Guile
export GUILE_LOAD_PATH="/opt/homebrew/share/guile/site/3.0"
export GUILE_LOAD_COMPILED_PATH="/opt/homebrew/lib/guile/3.0/site-ccache"
export GUILE_SYSTEM_EXTENSIONS_PATH="/opt/homebrew/lib/guile/3.0/extensions"

# Env Vars
export DOTNET_ROOT="/opt/homebrew/Cellar/dotnet/9.0.3/libexec"
# export DOTNET_ROOT="/opt/homebrew/Cellar/dotnet@8/8.0.14_1/libexec"

# Openjdk
export CPPFLAGS="-I/opt/homebrew/opt/openjdk/include"

# C & C++
export CPATH=/opt/homebrew/include:$CPATH
export LIBRARY_PATH=/opt/homebrew/lib:$LIBRARY_PATH
# Use this cautiously (it mau cause problems)
# export DYLD_LIBRARY_PATH=/opt/homebrew/lib:$DYLD_LIBRARY_PATH

# Ruby
# If you need to have ruby first in your PATH:
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
#By default, binaries installed by gem will be placed into:
export PATH="/opt/homebrew/lib/ruby/gems/3.4.0/bin:$PATH"
#For compilers to find ruby you may need to set:
export LDFLAGS="-L/opt/homebrew/opt/ruby/lib"
export CPPFLAGS="-I/opt/homebrew/opt/ruby/include"
#For pkg-config to find ruby you may need to set:
export PKG_CONFIG_PATH="/opt/homebrew/opt/ruby/lib/pkgconfig"

# OpenSSL for Perl
export OPENSSL_PREFIX=$(brew --prefix openssl)
export OPENSSL_INCLUDE="$OPENSSL_PREFIX/include"
export OPENSSL_LIB="$OPENSSL_PREFIX/lib"
export PERL_MM_USE_DEFAULT=1
export PERL5LIB=""

# Set up fzf key bindings and fuzzy completion
source <(fzf --zsh)
