;; -*- lexical-binding: t; -*-

(context-menu-mode t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package zenburn-theme
  :ensure t)
(load-theme 'zenburn t)
(add-to-list 'default-frame-alist '(font . "IosevkaTerm Nerd Font-16"))



(provide 'b-visual)
