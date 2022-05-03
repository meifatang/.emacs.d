(require 'package)

(setq package-selected-packages '(0x0
				  amx
				  ansible
				  applescript-mode
				  auctex
				  auto-dark
				  bluetooth
				  bongo
				  browse-at-remote
				  browse-kill-ring
				  chronos
				  company
				  counsel
				  csv-mode
				  docker
				  docker-cli
				  docker-compose-mode
				  dockerfile-mode
				  easy-hugo
				  edbi
				  ein
				  ;;elfeed
				  elixir-mode
				  elpl
				  elpy
				  emamux
				  erlang
				  esqlite
				  ess
				  exec-path-from-shell
				  expand-region
				  ffmpeg-player
				  flycheck
				  forge
				  format-all
				  gif-screencast
				  gist
				  gnuplot
				  go-mode
				  google-this
				  guix
				  haskell-mode
				  hass
				  helpful
				  hledger-mode
				  ido-completing-read+
				  image+
				  ivy
				  ivy-rich
				  jenkins
				  ;;jenkinsfile-mode
				  julia-mode
				  k8s-mode
				  kubedoc
				  kubernetes
				  leetcode
				  logview
				  lsp-mode
				  lua-mode
				  magit
				  meow
				  modus-themes
				  nginx-mode
				  nix-buffer
				  nix-env-install
				  nix-mode
				  nndiscourse
				  ob-async
				  ob-browser
				  ob-dart
				  ob-elixir
				  ob-go
				  ob-graphql
				  ob-http
				  ob-ipython
				  ob-mongo
				  ob-restclient
				  ob-rust
				  ob-tmux
				  ob-typescript
				  ob-uart
				  org-bullets
				  org-contrib
				  ;;org-modern
				  org-noter
				  org-noter-pdftools
				  org-pdftools
				  org-pomodoro
				  org-ref
				  org-roam
				  org-roam-ui
				  ;;org-superstar
				  org-tree-slide
				  ;;org2ctex
				  ox-pandoc
				  pass
				  pcache
				  pdf-tools
				  pinentry
				  projectile
				  rainbow-delimiters
				  restart-emacs
				  restclient
				  rime
				  robot-mode
				  rust-mode
				  shell-pop
				  slack
				  slime
				  smart-hungry-delete
				  ssh
				  sudo-edit
				  system-packages
				  systemd
				  telega
				  terraform-mode
				  toml-mode
				  tsc
				  ;;undo-tree
				  uuidgen
				  vagrant
				  valign
				  vterm
				  which-key
				  whois
				  wolfram
				  yaml-mode
				  yasnippet
				  yasnippet-snippets
				  yatemplate))

(setq package-archives '(("org"   . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("gnu"   . "https://elpa.gnu.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(dolist (package package-selected-packages)
  (unless (package-installed-p package)
    (package-install package)))

(provide 'felix-package)
