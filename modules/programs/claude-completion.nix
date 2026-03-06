{ ... }:
{
  flake.homeModules.claude-completion =
    { pkgs, lib, ... }:
    let
      claudeCompletion = pkgs.writeTextDir "share/zsh/site-functions/_claude" ''
        #compdef claude

        # Zsh completion for Claude Code CLI
        # Generated for nixos-config

        _claude_models() {
          local models=(
            'sonnet:Claude Sonnet (latest)'
            'opus:Claude Opus (latest)'
            'haiku:Claude Haiku (latest)'
            'claude-sonnet-4-5-20250929:Claude Sonnet 4.5'
            'claude-opus-4-5-20251101:Claude Opus 4.5'
          )
          _describe -t models 'model' models
        }

        _claude_permission_modes() {
          local modes=(
            'acceptEdits:Accept all edit operations'
            'bypassPermissions:Bypass all permission checks'
            'default:Default permission mode'
            'delegate:Delegate permissions'
            'dontAsk:Do not ask for permissions'
            'plan:Plan mode'
          )
          _describe -t modes 'permission mode' modes
        }

        _claude_output_formats() {
          local formats=(
            'text:Plain text output (default)'
            'json:JSON output (single result)'
            'stream-json:Realtime streaming JSON'
          )
          _describe -t formats 'output format' formats
        }

        _claude_input_formats() {
          local formats=(
            'text:Plain text input (default)'
            'stream-json:Realtime streaming input'
          )
          _describe -t formats 'input format' formats
        }

        _claude_mcp() {
          local -a subcmds
          subcmds=(
            'add:Add an MCP server to Claude Code'
            'add-from-claude-desktop:Import MCP servers from Claude Desktop'
            'add-json:Add an MCP server with a JSON string'
            'get:Get details about an MCP server'
            'help:Display help for command'
            'list:List configured MCP servers'
            'remove:Remove an MCP server'
            'reset-project-choices:Reset all approved and rejected project-scoped servers'
            'serve:Start the Claude Code MCP server'
          )

          if (( CURRENT == 2 )); then
            _describe -t commands 'mcp command' subcmds
          else
            case $words[2] in
              add)
                _arguments \
                  '--transport[Transport type]:transport:(stdio http sse)' \
                  '-e[Environment variable]:env:' \
                  '--env[Environment variable]:env:' \
                  '-s[Scope]:scope:(user project)' \
                  '--scope[Scope]:scope:(user project)' \
                  '--header[HTTP header]:header:' \
                  '*:args:'
                ;;
              add-json)
                _arguments \
                  '-s[Scope]:scope:(user project)' \
                  '--scope[Scope]:scope:(user project)' \
                  ':name:' \
                  ':json:'
                ;;
              get|remove)
                _arguments ':name:'
                ;;
              serve)
                _arguments \
                  '--permission-prompt-tool[Permission prompt tool]' \
                  '--read-approval-tool[Read approval tool]' \
                  '--write-approval-tool[Write approval tool]' \
                  '--mcp-approval-tool[MCP approval tool]' \
                  '--bash-approval-tool[Bash approval tool]'
                ;;
              *)
                ;;
            esac
          fi
        }

        _claude_plugin() {
          local -a subcmds
          subcmds=(
            'disable:Disable an enabled plugin'
            'enable:Enable a disabled plugin'
            'help:Display help for command'
            'install:Install a plugin from available marketplaces'
            'list:List installed plugins'
            'marketplace:Manage Claude Code marketplaces'
            'uninstall:Uninstall an installed plugin'
            'update:Update a plugin to the latest version'
            'validate:Validate a plugin or marketplace manifest'
          )

          if (( CURRENT == 2 )); then
            _describe -t commands 'plugin command' subcmds
          else
            case $words[2] in
              install|i|enable|disable|uninstall|remove|update)
                _arguments ':plugin:'
                ;;
              validate)
                _arguments ':path:_files'
                ;;
              marketplace)
                local -a mkt_subcmds
                mkt_subcmds=(
                  'add:Add a marketplace'
                  'list:List marketplaces'
                  'remove:Remove a marketplace'
                )
                if (( CURRENT == 3 )); then
                  _describe -t commands 'marketplace command' mkt_subcmds
                fi
                ;;
              *)
                ;;
            esac
          fi
        }

        _claude_install() {
          _arguments \
            '--force[Force installation even if already installed]' \
            '-h[Display help]' \
            '--help[Display help]' \
            ':target:(stable latest)'
        }

        _claude() {
          local -a subcmds
          subcmds=(
            'doctor:Check the health of your Claude Code auto-updater'
            'install:Install Claude Code native build'
            'mcp:Configure and manage MCP servers'
            'plugin:Manage Claude Code plugins'
            'setup-token:Set up a long-lived authentication token'
            'update:Check for updates and install if available'
          )

          local -a main_opts
          main_opts=(
            '--add-dir[Additional directories to allow tool access to]:directories:_files -/'
            '--agent[Agent for the current session]:agent:'
            '--agents[JSON object defining custom agents]:json:'
            '--allow-dangerously-skip-permissions[Enable bypassing all permission checks as an option]'
            '--allowedTools[Comma or space-separated list of tool names to allow]:tools:'
            '--allowed-tools[Comma or space-separated list of tool names to allow]:tools:'
            '--append-system-prompt[Append a system prompt to the default system prompt]:prompt:'
            '--betas[Beta headers to include in API requests]:betas:'
            '--chrome[Enable Claude in Chrome integration]'
            '-c[Continue the most recent conversation in the current directory]'
            '--continue[Continue the most recent conversation in the current directory]'
            '--dangerously-skip-permissions[Bypass all permission checks]'
            '-d[Enable debug mode with optional category filtering]:filter:'
            '--debug[Enable debug mode with optional category filtering]:filter:'
            '--debug-file[Write debug logs to a specific file path]:path:_files'
            '--disable-slash-commands[Disable all skills]'
            '--disallowedTools[Comma or space-separated list of tool names to deny]:tools:'
            '--disallowed-tools[Comma or space-separated list of tool names to deny]:tools:'
            '--fallback-model[Enable automatic fallback to specified model]:model:_claude_models'
            '--file[File resources to download at startup]:specs:'
            '--fork-session[Create a new session ID instead of reusing the original]'
            '--from-pr[Resume a session linked to a PR]:value:'
            '-h[Display help for command]'
            '--help[Display help for command]'
            '--ide[Automatically connect to IDE on startup]'
            '--include-partial-messages[Include partial message chunks as they arrive]'
            '--input-format[Input format]:format:_claude_input_formats'
            '--json-schema[JSON Schema for structured output validation]:schema:'
            '--max-budget-usd[Maximum dollar amount to spend on API calls]:amount:'
            '--mcp-config[Load MCP servers from JSON files or strings]:configs:_files'
            '--mcp-debug[Enable MCP debug mode (deprecated)]'
            '--model[Model for the current session]:model:_claude_models'
            '--no-chrome[Disable Claude in Chrome integration]'
            '--no-session-persistence[Disable session persistence]'
            '--output-format[Output format]:format:_claude_output_formats'
            '--permission-mode[Permission mode to use for the session]:mode:_claude_permission_modes'
            '--plugin-dir[Load plugins from directories]:paths:_files -/'
            '-p[Print response and exit]'
            '--print[Print response and exit]'
            '--replay-user-messages[Re-emit user messages from stdin back on stdout]'
            '-r[Resume a conversation by session ID]:value:'
            '--resume[Resume a conversation by session ID]:value:'
            '--session-id[Use a specific session ID for the conversation]:uuid:'
            '--setting-sources[Comma-separated list of setting sources to load]:sources:'
            '--settings[Path to a settings JSON file or a JSON string]:file-or-json:_files'
            '--strict-mcp-config[Only use MCP servers from --mcp-config]'
            '--system-prompt[System prompt to use for the session]:prompt:'
            '--tools[Specify the list of available tools]:tools:'
            '--verbose[Override verbose mode setting from config]'
            '-v[Output the version number]'
            '--version[Output the version number]'
          )

          _arguments -C \
            "''${main_opts[@]}" \
            '1:command:->command' \
            '*::arg:->args'

          case $state in
            command)
              _describe -t commands 'claude command' subcmds
              ;;
            args)
              case $words[1] in
                mcp)
                  _claude_mcp
                  ;;
                plugin)
                  _claude_plugin
                  ;;
                install)
                  _claude_install
                  ;;
                doctor|update|setup-token)
                  _arguments '-h[Display help]' '--help[Display help]'
                  ;;
              esac
              ;;
          esac
        }

        _claude "$@"
      '';
    in
    {
      key = "nixos-config.modules.home.claude-completion";

      home.packages = [ claudeCompletion ];

      programs.zsh.initContent = lib.mkAfter ''
        # Add claude completion to fpath
        fpath+=(${claudeCompletion}/share/zsh/site-functions)
      '';
    };
}
