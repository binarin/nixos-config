;; -*- lexical-binding: t; -*-
(use-package hledger-mode
  :ensure nil
  :mode "\\.journal"
  :bind (:map hledger-mode-map
              ("<Forward>" . hledger-forward-entry)
              ("<Back>" . hledger-backward-entry)
              ("RET" . newline))
  :custom ((hledger-currency-string "€")))


(provide 'b-ledger)
