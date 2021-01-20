# Build using nixos-generators. For example:
#
# For example, if building an installer iso on an x86_64-linux system:
#
#   nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
#     -f install-iso -c configuration.nix
#
# If building an installer sd image for aarch64 on an x86_64-linux system,
# set 'boot.binfmt.emulatedSystems = [ "aarch64-linux" ];' and run:
#
#   nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
#     -f sd-aarch64-installer --system aarch64-linux -c configuration.nix

{ pkgs, lib, ... }:

let
  my-vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = {
      packages.vim-package-group.start = with pkgs.vimPlugins; [ vim-nix vim-surround ];
      customRC = ''
        scriptencoding utf-8
        set ffs=unix
        set encoding=utf-8
        filetype plugin indent on
        syntax on

        set hidden autoread noconfirm
        set noerrorbells visualbell t_vb=
        set lazyredraw
        set splitbelow splitright
        set number relativenumber
        set list listchars=tab:>-,extends:>,precedes:<
        set laststatus=2 showmode cmdheight=1 showcmd
        set statusline=[%n]\ %f%<\ %m%y%h%w%r\ \ %(0x%B\ %b%)%=%p%%\ \ %(%l/%L%)%(\ \|\ %c%V%)%(\ %)
        set wildmenu wildmode=longest:list,full
        set virtualedit=all
        set scrolloff=0
        set nojoinspaces backspace=indent,eol,start whichwrap+=<,>,h,l,[,]
        set nrformats=bin,hex
        set cpoptions+=y
        set autoindent smarttab tabstop=4 expandtab softtabstop=4 shiftwidth=4
        set cinoptions+=:0L0g0j1J1
        set textwidth=80 nowrap
        set formatoptions=croqjln
        set magic noignorecase smartcase showmatch incsearch hlsearch
        set timeout timeoutlen=3000 ttimeout ttimeoutlen=0

        nnoremap Q @q
        xnoremap <silent> Q :normal @q<CR>
        xnoremap <silent> . :normal .<CR>

        colorscheme elflord
      '';
    };
  };
in {
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
  };
  users.users.root.password = "nixos";
  environment = {
    variables.EDITOR = "vim";
    systemPackages = with pkgs; [
      git
      tmux
      my-vim
    ];
  };
  programs.bash.interactiveShellInit = ''
    set -o vi
  '';
}
