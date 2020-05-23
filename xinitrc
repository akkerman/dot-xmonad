export _JAVA_AWT_WM_NONREPARENTING=1
export GTK_THEME=Adwaita:dark

xmonad --recompile

/home/akkerman/.screenlayout/default          # xrandr
# /home/akkerman/.fehbg                       # background
# /home/akkerman/.config/compton/launch.sh &  # transparency / fade
/home/akkerman/.config/polybar/launch.sh &    # dock / statusbar

# tray programs
/usr/bin/pasystray &      # sound
/usr/sbin/dropbox start & # file sharing
/usr/sbin/flameshot &     # screen shots
/usr/bin/synergyc octotron & # share mouse & keyboard
blueman-applet &

# utils
/usr/sbin/dunst &
/usr/sbin/unclutter -idle 2 &

# xmodmap -e 'keycode 66 = Control_L'
# xmodmap -e 'clear Lock'
# xmodmap -e 'add Control = Control_L'

exec xmonad
