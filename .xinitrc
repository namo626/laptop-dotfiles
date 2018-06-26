xinput --set-prop 'Elan Touchpad' 'libinput Accel Speed' 0.6 &
# redshift -b 0.8:0.6 &
setxkbmap -option caps:escape
setxkbmap -layout us,th -option grp:lctrl_lshift_toggle
xscreensaver &
xbindkeys
compton &
udiskie --tray &
dropbox start &
megasync &
xss-lock -- xscreensaver-command -lock &

exec xmonad
