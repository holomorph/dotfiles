#!/usr/bin/python
# ~/.mutt/gmail.py

import re
import os
import subprocess
import sys

mapping = { 'INBOX':              'inbox'
          , '[Gmail]/All Mail':   'archive'
          , '[Gmail]/Drafts':     'drafts'
          , '[Gmail]/Important':  'important'
          , '[Gmail]/Sent Mail':  'sent'
          , '[Gmail]/Spam':       'spam'
          , '[Gmail]/Starred':    'starred'
          , '[Gmail]/Trash':      'trash'
          }

r_mapping = { val: key for key, val in mapping.items() }

def nt_remote(folder):
    try:
        return mapping[folder]
    except:
        return re.sub(' ', '_', folder).lower()

def nt_local(folder):
    try:
        return r_mapping[folder]
    except:
        return re.sub('_', ' ', folder).capitalize()

# folderfilter = exclude(['Label', 'Label', ... ])
def exclude(excludes):
    def inner(folder):
        try:
            excludes.index(folder)
            return False
        except:
            return True
    return inner

def username(account):
	account = os.path.basename(account)
	path = os.path.expanduser("~/.mutt/%s.gpg" % account)
	args = ["gpg", "--quiet", "--no-tty", "--batch", "--decrypt", path]
	try:
		return re.compile('\w+@\w+\.\w+').findall(subprocess.check_output(args))[0]
	except subprocess.CalledProcessError:
		return ""

def password(account):
	account = os.path.basename(account)
	path = os.path.expanduser("~/.mutt/%s.gpg" % account)
	args = ["gpg", "--quiet", "--no-tty", "--batch", "--decrypt", path]
	try:
		return re.compile('password=\"([ -~]*)\"').findall(subprocess.check_output(args))[0]
	except subprocess.CalledProcessError:
		return ""
