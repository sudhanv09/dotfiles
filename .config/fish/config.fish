if status is-interactive
    # Commands to run in interactive sessions can go here
end

alias ls="eza -G --icons"
alias rsync="rsync -raP --info=progress2 --info=name0"
alias dust="dust -rbd 1"
alias mvfo="mv */* . && rmdir */"
alias lg="lazygit"
alias lzd="lazydocker"

alias mon="bash ~/.screenlayout/monitor.sh"
alias tv="bash ~/.screenlayout/tv.sh"


zoxide init fish | source
jj util completion fish | source

set -gx EDITOR /usr/bin/nvim

# pnpm
set -gx PNPM_HOME "/home/zeus/.local/share/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

