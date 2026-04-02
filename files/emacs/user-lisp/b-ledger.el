;; -*- lexical-binding: t; -*-
(use-package hledger-mode
  :ensure nil
  :mode "\\.journal"
  :bind (:map hledger-mode-map
              ("<Forward>" . hledger-forward-entry)
              ("<Back>" . hledger-backward-entry)))


(provide 'b-ledger)
