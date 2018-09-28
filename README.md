# Installation:

```
cargo install evcxr
cargo install evcxr_repl
```

I highly recommend using the straight package manager

```

(straight-use-package
 '(evcxr
   :type git
   :host github
   :repo "serialdev/evcxr-mode"
   :config
   (add-hook 'rust-mode-hook #'evcxr-minor-mode)
))
```

Alternatively pull the repo and add to your init file
```
git clone https://github.com/SerialDev/evcxr-mode
```
!TODO: Will be added to melpa once all basic functionality works

# Current functionality:

```
C-c C-p [Start repl]
C-c C-c [Eval buffer]
C-c C-l [Eval line]
C-c C-r [eval region]
C-c C-t [Type Check under point]
C-c C-o [Toggle optimization]
C-c C-v [Display bound variables]
C-c C-e [Explain last Error]
C-c C-s [Clear State & keep compilation cache]
C-c C-t [Type Check under point]
```

## Gotchas
Make sure that your cargo bin is accessible to emacs
```
(setq exec-path (append exec-path '("/home/usr/.cargo/bin")))
```

## Rust Repl mode based on:

Ecvxr: https://github.com/google/evcxr

Heavily based on Rusti.el
