This is my first configuration from scratch. I wanted to understand how to do Elisp and get something that I can modify to my likings.

I want to keep my configuration the simpliest as I can, I don't search to optimize it, just to keep an sane amount of packages (the fact to see over 150 packages in Centaur or Doom was a little overwhelming).

This config needs to be placed in `.emacs.d` and does not require an online connection.

I will not place screenshots since my layout is pretty much just Emacs with the lemon system monitor (when using EXWM) and without the ugly GUI elements from default Emacs.

In this configuration I use keybindings like "M-é" because I use a french keyboard and to enter number keys I need to hold shift so to split the windows I would need to do "M-Shift-é" that send "M-2".

I use Devil key bindings that I find very fun to use, and had replaced ", m" by ", ," for "M-" because I think it is simplier to double tap the coma than to reach the "m" key or than to use a different key for Meta (like using ";").

The only thing this config depends on are the tools to build vterm-module (on debian):

```
apt install build-essential cmake libtool libvterm-dev
```
