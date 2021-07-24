nix_prompt_info() {
    if [[ -n $IN_NIX_SHELL ]]; then
        echo -n "(%{$fg_bold[yellow]%}nix%{$reset_color%})"
    fi
}
