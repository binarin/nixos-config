#!/usr/bin/env zsh

fzf-atuin-history-widget() {
    local selected num
    setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2>/dev/null

    selected=$(
        export FZF_DEFAULT_COMMAND='atuin search --print0 --reverse --format="$format_string" --filter-mode="${FZF_HEADER_LABEL:-$initial_filter_mode}"'
        export FZF_DEFAULT_OPTS="${FZF_DEFAULT_OPTS:-}${FZF_DEFAULT_OPTS:+ }--color preview-bg:#2b2b2b,preview-fg:#989890"
        export format_string="{relativetime}\t{command}\t{time}\t{duration}\t{user}\t{host}\t{exit}\t{directory}"
        export initial_filter_mode=host
        export zsh_user_color="\033[34m"
        export zsh_host_color="\033[31m"
        export zsh_directory_color="\033[36m"
        export reset_color="\033[0m"
        export exit_success_color="\033[32m"
        export exit_failure_color="\033[31m"
        eval $FZF_DEFAULT_COMMAND | \
        fzf \
            --read0 \
            --query="${LBUFFER}" \
            --no-multi-line \
            --no-sort \
            --delimiter "\t" \
            --scheme=history \
            --nth=2 \
            --with-nth=1,2 \
            --accept-nth=2 \
            --no-multi \
            --header=$initial_filter_mode \
            --header-label=$initial_filter_mode \
            --header-first \
            --preview-window top,2 \
            --preview='
            echo "$zsh_user_color"{5}"$reset_color@$zsh_host_color"{6}"$reset_color:$zsh_directory_color"{8}"$reset_color"
            echo -n "["{3}"] run for "{4}", exited with "
            if [[ {7} -gt 0 ]]; then
              echo -n "$exit_failure_color"
            else
              echo -n "$exit_success_color"
            fi
            echo {7}"$reset_color"

            ' \
            --bind 'ctrl-r:transform:
            _ub() {
              echo -n "change-header($1)+change-header-label($1)+reload:$FZF_DEFAULT_COMMAND"
            }
            case "$FZF_HEADER_LABEL" in
              host) _ub directory;;
              directory) _ub session;;
              session) _ub global;;
              global) _ub host;;
            esac
            '
            )
    local ret=$?
    echo "$selected"
    if [ -n "$selected" ]; then
        # the += lets it insert at current pos instead of replacing
        LBUFFER="${selected}"
    fi
    zle reset-prompt
    return $ret
}

atuin-setup() {
    if ! which atuin &> /dev/null; then return 1; fi

    export ATUIN_NOBIND="true"
    eval "$(atuin init zsh)"

    zle -N fzf-atuin-history-widget
    bindkey '^R' fzf-atuin-history-widget
}

atuin-setup
