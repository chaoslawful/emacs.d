(evil-mode 1)
; @see https://github.com/timcharper/evil-surround
(require 'surround)
(global-surround-mode 1)
(evil-set-initial-state 'org-mode 'emacs)
(evil-set-initial-state 'inf-ruby-mode 'emacs)
(evil-set-initial-state 'yari-mode 'emacs)
(provide 'init-evil)
