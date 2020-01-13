# -*- coding: utf-8 -*-
# Requires Weechat >= 0.3.7, openssl
# Released under GNU GPL v3

import weechat

weechat.register("sortlatest", "maralorn", "0.1", "GPL3",
                 "sortlatest: Put buffer with new message at position 2.", "",
                 "")

# Hook privmsg/hilights
weechat.hook_print("", "notify_message", "", 1, "move_up", "")
weechat.hook_print("", "notify_private", "", 1, "move_up", "")


# Functions
def move_up(data, bufferp, uber_empty, tagsn, isdisplayed, ishilight, prefix,
            message):
    weechat.command(bufferp, "/buffer move 2".format(bufferp))
    return weechat.WEECHAT_RC_OK
