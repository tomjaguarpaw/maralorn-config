#!/usr/bin/env python

import subprocess
import os
import sys

def call_task(args):
    return subprocess.Popen(["task", "rc.verbose=nothing"] + args, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()[0].strip()

runtime_dir = os.getenv("XDG_RUNTIME_DIR")
if not os.path.exists(runtime_dir):
   os.makedirs(runtime_dir)
filename = os.path.join(runtime_dir, "taskfilter")
if not os.path.exists(filename):
    with open(filename, 'w') as filterfile:
        filterfile.write("inboxall")

lastresult = ""
while True:
    with open(filename) as filterfile:
        filter = filterfile.read().strip()
    tasklist = call_task(["default"] + filter.split())
    rofi_list_cmd = ['rofi', '-dmenu', '-p', 'task', '-mesg', '\n'.join([lastresult, "filter: {}".format(filter)]).strip()]
    rofi = subprocess.Popen(rofi_list_cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    entered_command = rofi.communicate(input=tasklist)[0].decode('utf8').strip()
    if entered_command == "":
        sys.exit(0)
    task_result = subprocess.Popen(["task"] + entered_command.split(), stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    lastresult = '\n'.join([task_result[0].decode('utf8'), task_result[1].decode('utf8')]).strip()
