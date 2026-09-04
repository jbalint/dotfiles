# SSH agent: systemd-managed global agent

This machine runs one SSH agent per user session under the systemd user manager.
It listens on a fixed socket so all shells and systemd-started programs can share
it, instead of spawning a new agent per shell.

## Layout

| File | Purpose |
|------|---------|
| `systemd/ssh-agent.socket` | Owns the listening socket path |
| `systemd/ssh-agent.service` | Runs `ssh-agent -D` (socket-activated) |
| `environment.d/10-ssh-agent.conf` | Sets `SSH_AUTH_SOCK` for systemd user services |
| `configs/zshrc` (ssh-agent block) | Shell fallback + interactive auto-add |

These live in the dotfiles repo and are symlinked into `~/.config/...`.

## How it works

- The `.socket` unit binds `$XDG_RUNTIME_DIR/ssh-agent.socket` (i.e.
  `/run/user/<uid>/ssh-agent.socket`) and owns the path.
- The `.service` is socket-activated: on first connection, systemd spawns
  `ssh-agent -D` and hands it the already-bound fd via `LISTEN_FDS`/`LISTEN_PID`.
  That is why the service does **not** pass `-a <path>` — the socket unit owns the
  path, so the agent is told not to bind its own.
- `environment.d` injects `SSH_AUTH_SOCK` into the environment the user manager
  passes to every user service it starts.
- Interactive shells read `.zshrc`, which applies this precedence:
  1. If `SSH_AUTH_SOCK` is already set (e.g. agent forwarding from an SSH client),
     use it — giving access to the client's keys.
  2. Otherwise fall back to the global systemd socket.
  3. On interactive login, if the socket falls back to the global agent and it holds
     no keys, auto-run `ssh-add ~/.ssh/id_rsa` (one passphrase prompt, then it persists
     for the lifetime of the service).

## Interacting with the global agent

The global agent socket is `$XDG_RUNTIME_DIR/ssh-agent.socket`, typically
`/run/user/1000/ssh-agent.socket`. Point `SSH_AUTH_SOCK` at it explicitly to drive it,
independent of what the current shell's forwarded/fallback value is.

Run these to check or modify the **global** agent:

```sh
# List keys loaded in the global agent
SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket" ssh-add -l

# Add a key (passphrase prompt; persists until the service stops/reboots)
SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket" ssh-add ~/.ssh/id_rsa

# Add every default key in ~/.ssh
SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket" ssh-add

# Remove all keys from the global agent
SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket" ssh-add -D

# Remove one key
SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket" ssh-add -d ~/.ssh/id_rsa
```

To drive whatever agent the *current* shell resolves to, omit the
`SSH_AUTH_SOCK=...` prefix (plain `ssh-add -l`, etc.).

## Managing the service

```sh
systemctl --user start   ssh-agent.socket   # start listening
systemctl --user stop    ssh-agent.socket   # stop
systemctl --user restart ssh-agent.service  # restart the agent (clears loaded keys)
systemctl --user status  ssh-agent.socket ssh-agent.service
```

After `restart`, loaded keys are cleared — run `ssh-add` again to repopulate.

## Notes and caveats

- **Agent forwarding**: this server is reached over SSH, and forwarding exposes the
  *client's* keys via a per-connection forwarded socket. The `.zshrc` logic honors
  that when present and falls back to the global agent otherwise, so both work.
  Keys that exist only on the client will not be in the global agent unless you add
  them to this box explicitly.
- **`environment.d` only reaches systemd user services**, not sshd-spawned login
  shells — hence the `.zshrc` fallback block.
- **Passphrase key after reboot**: user services start on first login (linger is off),
  so keys must be re-added after reboot; the interactive auto-add in `.zshrc` handles
  `id_rsa` automatically at first login.