# get-ssh-agent-gnome-keyring-daemon

On Ubuntu 21.10 my Emacs running keychain-refresh-environment doesn't interact well with the gnome-keyring-daemon so it starts a fresh agent and doesn't get the right ```SSH_AGENT_SOCK``` and ```SSH_AUTH_SOCK```.

This very fragile little bit of emacs code looks for a socket in the place my gnome-keyring-daemon stores it, and if it exists calls pgrep to try and get the PID of the agent associated with it.

All you really need us something like this:

```
(use-package get-ssh-agent-gnome-keyring-daemon
 :config
 (get-ssh-agent-gnome-keyring-daemon))
```

It will attempts to look for a socket in ```/run/user/<uid>/keyring/.ssh``` then get the PID for the ssh-agent associated.

This was written because on Ubuntu 21.10 my keychain-refresh-environment doesn't work, so now for my portable config I have:

```
(use-package get-ssh-agent-gnome-keyring-daemon
 :config
 (get-ssh-agent-gnome-keyring-daemon))

(use-package keychain-environment
 :ensure t
 :config
 (if (not get-ssh-agent-gnome-keyring-daemon)
     (keychain-refresh-environment)))
```

Which means if it gets it from the gnome-keyring-daemon it doesn't
try keychain-refresh-environment.

If your socket is in another location you can call:

```
(get-ssh-agent-gnome-keyring-daemon "/path/to/your/socket")
```

Just keep in mind this is going to be put into a pgrep shell command and I can't guarentee this is totally secure.  You have been warned.




