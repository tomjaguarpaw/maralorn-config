#!/bin/sh

#      bind = [
#        "$mod, Prior, workspace, -1"
#        "$mod, Next, workspace, +1"
#        "SUPER_SHIFT, Prior, movetoworkspace, -1"
#        "SUPER_SHIFT, Next, movetoworkspace, +1"
#        ", Print, execr, screenshot"
#      ];
#  };

swaybg -m fill -i ~/.config/wallpaper &

riverctl map normal Super N spawn "(makoctl mode -r show; eww close overlay) || (eww open overlay; makoctl mode -a show)"

riverctl map normal Super Return spawn foot
riverctl map normal Super Space spawn "foot -a hotkeys my-hotkeys"
riverctl float-filter-add app-id hotkeys
riverctl keyboard-layout -variant neo -options altwin:swap_lalt_lwin de

riverctl map normal Super Q close
riverctl map normal None Print spawn screenshot
riverctl map normal Super Z zoom

riverctl map normal Super Right focus-view next
riverctl map normal Super Left focus-view previous

riverctl map normal Super+Shift Right swap next
riverctl map normal Super+Shift Left swap previous

riverctl map normal Super Period focus-output next
riverctl map normal Super Comma focus-output previous

riverctl map normal Super+Shift Period send-to-output next
riverctl map normal Super+Shift Comma send-to-output previous

# Super + Left Mouse Button to move views
riverctl map-pointer normal Super BTN_LEFT move-view

# Super + Right Mouse Button to resize views
riverctl map-pointer normal Super BTN_RIGHT resize-view

# Super + Middle Mouse Button to toggle float
riverctl map-pointer normal Super BTN_MIDDLE toggle-float

for i in $(seq 1 9)
do
    tags=$((1 << (i - 1)))

    # Super+[1-9] to focus tag [0-8]
    riverctl map normal Super "$i" set-focused-tags "$tags"

    # Super+Shift+[1-9] to tag focused view with tag [0-8]
    riverctl map normal Super+Shift "$i" set-view-tags "$tags"

    # Super+Control+[1-9] to toggle focus of tag [0-8]
    riverctl map normal Super+Control "$i" toggle-focused-tags "$tags"

    # Super+Shift+Control+[1-9] to toggle tag [0-8] of focused view
    riverctl map normal Super+Shift+Control "$i" toggle-view-tags "$tags"
done

# Super+0 to focus all tags
# Super+Shift+0 to tag focused view with all tags
all_tags=$(((1 << 32) - 1))
riverctl map normal Super 0 set-focused-tags "$all_tags"
riverctl map normal Super+Shift 0 set-view-tags "$all_tags"

# Super+Space to toggle float
riverctl map normal Super T toggle-float

# Super+F to toggle fullscreen
riverctl map normal Super F toggle-fullscreen


# Declare a passthrough mode. This mode has only a single mapping to return to
# normal mode. This makes it useful for testing a nested wayland compositor
#riverctl declare-mode passthrough

# Super+F11 to enter passthrough mode
#riverctl map normal Super F11 enter-mode passthrough

# Super+F11 to return to normal mode
#riverctl map passthrough Super F11 enter-mode normal

# Various media key mapping examples for both normal and locked mode which do
# not have a modifier
for mode in normal locked
do
    # Eject the optical drive (well if you still have one that is)
    #riverctl map $mode None XF86Eject spawn 'eject -T'

    # Control pulse audio volume with pamixer (https://github.com/cdemoulins/pamixer)
    riverctl map $mode None XF86AudioRaiseVolume  spawn 'pamixer -i 5'
    riverctl map $mode None XF86AudioLowerVolume  spawn 'pamixer -d 5'
    riverctl map $mode None XF86AudioMute         spawn 'pamixer --toggle-mute'

    # Control MPRIS aware media players with playerctl (https://github.com/altdesktop/playerctl)
    riverctl map $mode None XF86AudioMedia spawn 'playerctl play-pause'
    riverctl map $mode None XF86AudioPlay  spawn 'playerctl play-pause'
    riverctl map $mode None XF86AudioPrev  spawn 'playerctl previous'
    riverctl map $mode None XF86AudioNext  spawn 'playerctl next'

    # Control screen backlight brightness with light (https://github.com/haikarainen/light)
    riverctl map $mode None XF86MonBrightnessUp   spawn 'light -A 5'
    riverctl map $mode None XF86MonBrightnessDown spawn 'light -U 5'
done

# Set background and border color
riverctl border-color-focused 0xffffff
riverctl border-color-unfocused 0xffffff00
riverctl border-width 1

# Set keyboard repeat rate
#riverctl set-repeat 50 300

# Make all views with an app-id that starts with "float" and title "foo" start floating.
#riverctl rule-add float -app-id 'float*' -title 'foo'

# Make all views with app-id "bar" and any title use client-side decorations
#riverctl rule-add csd -app-id "bar"
dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY XDG_CURRENT_DESKTOP XDG_SESSION_TYPE NIXOS_OZONE_WL

systemctl --user start river-session.target

river-tag-overlay &

riverctl default-layout riverguile
#river-luatile &
riverguile &

riverctl spawn unlock-keys

kanshictl reload
