# Disable access control
xhost +SI:localuser:$USER

# Themes, etc
#gnome-settings-daemon &

# Fallback cursor
xsetroot -cursor_name left_ptr

# Keyboard repeat rate
xset r rate 200 60

export EDITOR="emacsclient"
export VISUAL="emacsclient"

xrdb -load ~/.Xresources
xinput --set-prop 10 "libinput Natural Scrolling Enabled" 1 &
setxkbmap us -variant colemak -option ctrl:nocaps -option altwin:meta_alt &
xbanish &
conky &
#keynav &
#xinput --disable 12 &
redshift -b 1.0:0.8 -l 51.5074:0.1278 -t 5500:3700 &

# Start Emacs
#exec dbus-launch --exit-with-session emacs
exec openbox
