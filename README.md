# Installation:

```
cargo install evcxr
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
```



## Rust Repl mode based on:

Ecvxr: https://github.com/google/evcxr

Heavily based on Rusti.el
