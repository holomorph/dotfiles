"""Translate between Gmail box names and Maildir directory names.
"""

import re

mapping = { 'INBOX':              'inbox'
#         , '[Gmail]/All Mail':   'archive'
          , '[Gmail]/Drafts':     'drafts'
#         , '[Gmail]/Important':  'important'
          , '[Gmail]/Sent Mail':  'sent'
          , '[Gmail]/Spam':       'spam'
#         , '[Gmail]/Starred':    'starred'
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
