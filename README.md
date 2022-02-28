# get-ssh-agent-gnome-keyring-daemon

On Ubuntu 21.10 my Emacs running keychain-refresh-environment doesn't interact well with the gnome-keyring-daemon so it starts a fresh agent and doesn't get the right ```SSH_AGENT_SOCK``` and ```SSH_AUTH_SOCK```.

This very fragile little bit of emacs code looks for a socket in the place my gnome-keyring-daemon stores it (by default ```/run/user/<uid>/keyring/.ssh```) and if it exists then emacs will call ```pgrep``` to try and get the PID of the agent associated with it.

All you really need us something like this:

```
(use-package get-ssh-agent-gnome-keyring-daemon
 :config
 (get-ssh-agent-gnome-keyring-daemon))
```

If it succeeds then it will return ```t```, but also set the variable ```get-ssh-agent-gnome-keyring-daemon``` to ```t```.  If it fails it will return ```nil``` and ensure that ```get-ssh-agent-gnome-keyring-daemon``` is also ```nil```, which is also its default value.


## Interactions with keychain-environment

If you already use the normally excellent keychain-environment in some places but want to make your config portable then you can try something like this:

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

So if there is already a running gnome-keyring-daemon it doesn't
try keychain-refresh-environment (which will probably start another ssh-agent)

## Configuring socket location and pgrep command

The function accepts two named arguments ```:socket``` and ```:pgrep-template``` which can be used to override the default methods used in the function.  The ```:socket``` has any single quotes removed from it.  The ```:pgrep-template``` should be a ```format``` string with a single ```%s``` escape in it, which is where your socket path will be put.

You can use one, or both, of these as you see fit.

```
(get-ssh-agent-gnome-keyring-daemon 
    :socket "/path/to/your/socket/.ssh"
    :pgrep-template "your-pgrep 'ssh-agent %s'")
```

Just keep in mind this is going to be put into a shell command and I can't guarentee this is totally secure.  You have been warned.
