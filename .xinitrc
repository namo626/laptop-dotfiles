xinput --set-prop 'SynPS/2 Synaptics TouchPad' 'libinput Accel Speed' 0.7 &
# redshift -b 0.8:0.6 &
setxkbmap -option caps:escape
setxkbmap -layout us,th -option grp:lctrl_lshift_toggle
xbindkeys
compton &
udiskie --tray &
#dropbox start &
~/.dropbox-dist/dropboxd &
megasync &
stalonetray &
xscreensaver &
xss-lock -- slock &
feh --bg-scale ~/Pictures/sky.jpg &
xset b off &

exec xmonad
