;; -*- lexical-binding: t; -*-

(autoload 'gptel-make-openai "gptel-openai")
(autoload 'gptel-make-deepseek "gptel-openai-extras")
(autoload 'gptel-make-gemini "gptel-gemini")

(use-package gptel
  :ensure nil
  :defer t
  :commands (gptel-make-openai)
  :config

  (gptel-make-openai "llm-runner"
    :stream t
    :protocol "https"
    :host "llm-runner.lynx-lizard.ts.net"
    ;; nix eval .#nixosConfigurations.llm-runner.config.llama-models.configurations --json 2>/dev/null | jq 'keys|join(" ")' -r
    :models '(gemma4 qwen3-coder-30b qwen3.5-9b))

  (gptel-make-gemini "Gemini"
    :key 'gptel-api-key-from-auth-source
    :stream t)

  (setf gptel-backend (gptel-make-deepseek "DeepSeek"
                        :stream t
                        :key 'gptel-api-key-from-auth-source
                        :models '(deepseek-v4-pro deepseek-v4-flash))
        gptel-model 'deepseek-v4-pro)
  (setf gptel-default-mode 'org-mode)
  (add-hook 'gptel-mode-hook 'gptel-highlight-mode))

(provide 'b-gptel)
