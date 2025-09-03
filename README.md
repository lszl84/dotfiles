# Stow

## IMPORTANT for mytools use `doas stow -t /usr/local mytools`

These contain small tools to be used inside `vi`'s non-interactive shell, to they must be accessible globally.
# Wayland Environment

- `labwc` for window management and lightdm login
- `swaybg` for the background, but I'm using my own `dynamic-background.sh` script that generates the background by mixing in `fastfetch` output
- `kanshi` for monitor and scaling management
- `mako` for notifications
- `swayidle` for locking the screen
  - `swaylock` does the actual locking
  - then after a delay, I simply call `systemctl suspend`. I don't turn off the screens with `wlopm` because this has problems with waking up the screens sometimes
- `thunar` works well as a file manager, without Xwayland,
- to make `emacs` work properly on Wayland, a package named sth like `emacs-pgtk` or whatever, needs to be installed
- `fuzzer` for launcher menu, bound to Super-Space
- `foot` as a nice and light terminal. Start `foot --server` on autostart and then use `footclient` for terminals
- `wl-recorder` for screen recording. Supports geometry. NOTE: with the 2x `kanshi` scaling, the geometry should be halved, i.e. '1920x1080' for 4K region,
- `slurp` for screen region selector

# Autologin

```bash
mkdir -p /etc/systemd/system/getty@tty1.service.d
```

Add the file `/etc/systemd/system/getty@tty1.service.d/override.conf`:

```
[Service]
ExecStart=
ExecStart=-/sbin/agetty --autologin luke --noclear %I 38400 linux
```

Then in `~/.bashrc`:

```
[ `tty` == /dev/tty1 ] && /home/luke/Developer/labwc/build/labwc && exit
```

Source: https://unix.stackexchange.com/questions/401759/automatically-login-on-debian-9-2-1-command-line

# Bluetooth Wakeup

I got it set up on Fedora Server but it does not seem to work on Debian hmmm. Kernel issue?

Arch linux recommends this, but does not work for me on Debian:

```
luke@debian:~$ cat /etc/udev/rules.d/*
ACTION=="add", SUBSYSTEM=="usb", DRIVERS=="usb", \
    ATTR{bDeviceClass}=="e0", \
    ATTR{bDeviceProtocol}=="01", \
    ATTR{bDeviceSubClass}=="01", \
ATTR{power/wakeup}="enabled"
luke@debian:~$ ls /etc/udev/rules.d/*
/etc/udev/rules.d/91-bluetooth-wakeup.rules
```

TODO:
- volume settings?
- sound output selection?
- brightness controls, also for the studio display

# Misc

## For LoFree keyboard - F keys will send F commands instead of brightness etc
```
echo 'options hid_apple fnmode=2' | sudo tee /etc/modprobe.d/hid_apple.conf
```

# dotfiles

Great! Since you’ve cloned your dotfiles repo to `~/.dotfiles`, here’s a **step-by-step guide** to manage your XFCE config (or other dotfiles) with **GNU Stow**:

---

### **1. Organize Your XFCE Config for Stow**
Stow works by creating symlinks from your repo to their target locations (e.g., `~/.config`).  
Structure your repo like this (example for XFCE):
```bash
~/.dotfiles/
  └── xfce/               # Stow "package" name (arbitrary)
      └── .config/
          └── xfce4/      # Mirror the actual config path
              ├── xfconf/
              ├── panel/
              └── ...
```

#### **Steps:**
1. **Copy your live XFCE configs** into the repo:
   ```bash
   mkdir -p ~/.dotfiles/xfce/.config/xfce4
   rsync -av ~/.config/xfce4/ ~/.dotfiles/xfce/.config/xfce4/
   ```
   - This preserves the directory structure.

2. **Repeat for other files** (if needed):
   ```bash
   mkdir -p ~/.dotfiles/xfce/.local/share/xfce4
   rsync -av ~/.local/share/xfce4/ ~/.dotfiles/xfce/.local/share/xfce4/
   ```

---

### **2. Run Stow to Create Symlinks**
From your repo root, run:
```bash
cd ~/.dotfiles
stow --verbose --target=$HOME xfce
```
- `--verbose`: Shows which symlinks are created.
- `--target=$HOME`: Points symlinks to your home directory (e.g., `~/.config/xfce4` → `~/.dotfiles/xfce/.config/xfce4`).
- `xfce`: The package name (folder inside `~/.dotfiles`).

#### **Expected Output:**
```
LINK: .config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml -> ../../../../../.dotfiles/xfce/.config/xfce4/xfconf/xfce-perchannel-xml/xfce4-keyboard-shortcuts.xml
...
```

---

### **3. Verify Symlinks**
Check that symlinks were created correctly:
```bash
ls -la ~/.config/xfce4/xfconf/  # Should show symlinks pointing to ~/.dotfiles/
```

---

### **4. Update Your Repo (When Configs Change)**
1. **Pull changes** from live configs into your repo:
   ```bash
   rsync -av --delete ~/.config/xfce4/ ~/.dotfiles/xfce/.config/xfce4/
   ```
2. **Commit and push**:
   ```bash
   cd ~/.dotfiles
   git add .
   git commit -m "Update xfce config"
   git push
   ```

---

### **5. On a New Machine**
1. Clone your repo and run Stow:
   ```bash
   git clone git@github.com:yourusername/dotfiles.git ~/.dotfiles
   cd ~/.dotfiles
   stow --verbose --target=$HOME xfce
   ```
   - XFCE will now use the configs from your repo.

---

### **Key Notes:**
1. **Stow Packages**: Each folder (e.g., `xfce`, `zsh`, `nvim`) inside `~/.dotfiles` is a "package" Stow can manage separately.
2. **Conflict Handling**: If files already exist, Stow will warn you. Delete or backup conflicting files first.
3. **Unsymlink**: To remove symlinks (e.g., for debugging):
   ```bash
   stow --verbose --target=$HOME --delete xfce
   ```

---

### **Example: Adding More Configs (e.g., Bash, Vim)**
```bash
~/.dotfiles/
  ├── bash/
  │   └── .bashrc
  ├── nvim/
  │   └── .config/nvim/
  └── xfce/
      └── .config/xfce4/
```
Run Stow for all packages:
```bash
stow --verbose --target=$HOME bash nvim xfce
```

---

### **Troubleshooting**
- **"File exists" errors**: Backup and remove the conflicting file first.
- **Broken symlinks**: Run `stow --delete` and try again.
- **Stow won’t overwrite files**: Use `--adopt` to merge (dangerous) or manually resolve.

Let me know if you hit snags!

