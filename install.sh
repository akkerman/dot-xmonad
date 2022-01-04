cabal install --package-env=$HOME/.config/xmonad --lib xmonad xmonad-contrib xmonad-dbus dbus utf8-string
cabal install --package-env=$HOME/.config/xmonad xmonad --overwrite-policy=always
cabal install --package-env=$HOME/.config/xmonad xmonad-dbus --overwrite-policy=always
