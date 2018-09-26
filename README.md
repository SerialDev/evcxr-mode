Installation:  
I highly recommend using the straight package manager  

(straight-use-package
 '(evil
   :type git
   :host github
   :repo "emacs-evil/evil"
   :config
   (add-hook 'rust-mode-hook #'evcxr-minor-mode)
))


Current functionality:  

```
C-c c [Eval buffer]  
C-c l [Eval line]  
C-c r [eval region]  
```



Rust Repl mode based on:  

Ecvxr: https://github.com/google/evcxr  

Heavily based on Rusti.el
