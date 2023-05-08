# eval "$(starship init zsh)"
# eval "$(direnv hook zsh)"

# autoload -U compinit && compinit
bindkey -e

# Aliases
alias cleanup="doas nix-collect-garbage --delete-older-than 7d"
alias bloat="nix path-info -Sh /run/current-system"
alias ytmp3="yt-dlp -x --continue --add-metadata --embed-thumbnail --audio-format mp3 --audio-quality 0 --metadata-from-title='%(artist)s - %(title)s' --prefer-ffmpeg -o '%(title)s.%(ext)s' "
alias cat="bat --style=plain"
alias grep='ripgrep'
alias du='du-dust'
alias ps='procs'
alias m="mkdir -p"
alias ls="exa -h --git --icons --color=auto --group-directories-first -s extension"
alias l="ls -lF --time-style=long-iso --icons"
alias la="exa -lah --tree"
alias tree="exa --tree --icons --tree"
alias http="python3 -m http.server"
alias burn="pkill -9"
alias diff="diff --color=auto"
alias kys="doas shutdown now"
alias killall="pkill"
alias ".1"="cd .."
alias ".2"="cd ../.."
alias ".3"="cd ../../.."
alias c="clear"
alias v="nvim"
alias emd="pkill emacs; emacs --daemon"
alias e="emacsclient -t"
alias cp="cp -iv"
alias mv="mv -iv"
alias rm="rm -vI"
alias bc="bc -ql"
alias mkd="mkdir -pv"
alias ytfzf="ytfzf -D"
alias hyprcaps="hyprctl keyword input:kb_options caps:caps"
alias gc="git clone --depth=1"
alias sudo="doas"

# Functions
function ytdl() {
    yt-dlp --embed-metadata --embed-subs -f 22 "$1"
}

function fcd() {
    cd "$(find -type d | fzf)"
}

export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export PATH="$PATH:/home/i/d-git/d-bin/bin:$HOME/.local/bin/d"
export EDITOR="emacsclient -nw -a 'nvim'"
export VISUAL=$EDITOR
export STARDICT_DATA_DIR="$HOME/.local/share/stardict"

export GRIM_DEFAULT_DIR="$HOME/pics/sshots/"
