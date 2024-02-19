# Dotfiles

## Setup

### Essential packages

``` bash
zypper in neovim ripgrep fish zoxide dust fcitx5 fcitx5-rime fcitx5-chewing lazygit tmux calibre rofi awesome mpv eza kitty

```

### Optionals

``` bash
# Install rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Install pnpm
curl -fsSL https://get.pnpm.io/install.sh | sh -


cargo install --locked yazi-fm
go install github.com/charmbracelet/glow@latest

sudo zypper in stow opi fzf fd aria2 kdeconnect-kde gh docker go nodejs

# Get all codecs in opensuse
opi codecs

```

### Configure

Ensure stow is installed.

``` bash
git clone https://github.com/sudhanv09/dotfiles
cd dotfiles
stow .

```

Install Fish shell and setup fisher and tide prompt.

```
curl -sL https://raw.githubusercontent.com/jorgebucaran/fisher/main/functions/fisher.fish | source && fisher install jorgebucaran/fisher
fisher install IlanCosman/tide@v6
```

Login to tmux and Press leader + I to install all plugins
``` bash
git clone https://github.com/tmux-plugins/tpm ~/.config/tmux/plugins/tpm
```

