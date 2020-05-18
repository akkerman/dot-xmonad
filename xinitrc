export _JAVA_AWT_WM_NONREPARENTING=1
export GTK_THEME=Adwaita:dark

xmonad --recompile

/home/akkerman/.screenlayout/default
/home/akkerman/.fehbg
/* /home/akkerman/.config/compton/launch.sh & */
/home/akkerman/.config/polybar/launch.sh &
/usr/bin/pasystray &
/usr/sbin/dropbox start &
/usr/sbin/flameshot &
/usr/bin/synergy &
/usr/sbin/dunst &
/usr/sbin/unclutter -idle 2 &

xmodmap -e 'keycode 66 = Control_L' 
xmodmap -e 'clear Lock'             
xmodmap -e 'add Control = Control_L'

exec xmonad
