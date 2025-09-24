## Installing vaapi on Arch

```
sudo pacman -S intel-media-driver libva-intel-driver
sudo pacman -S libva libva-utils
sudo pacman -S intel-media-sdk

```
## Will this help with farting sound?

```
sudo pacman -S linux-zen linux-zen-headers realtime-privileges pipewire pipewire-pulse
sudo usermod -a -G realtime $USER

sudo pacman -S sof-firmware
```


```
... quiet i916.enable_dc=0
/boot/loader/entries/linux-zen.conf
```

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

# Running Windows 11 with `qemu` on Alpine Linux host

Installing apks:

```sh
doas apk add qemu-system-x86_64 qemu-img
doas apk add  qemu-modules libvirt libvirt-qemu
doas apk add ovmf edk2
doas apk add swtpm
```

Module, group and rc setup:

```sh
echo tun |doas tee /etc/modules
doas modprobe tun
doas addgroup luke kvm
doas adduser luke qemu
doas rc-update add libvirtd
doas rc-update add libvirt-guests
```

TODO: **It looks like these rc services are not really needed for Windows guest to work (?)**

Download Windows 11 ISO to `~/Machines/`: `/home/luke/Machines/Win11_24H2_English_x64.iso`

Download Virtio drivers ISO - it will be mounted as CD during Windows installation. This will
be needed for hard disk and network to work when installing Windows.

```sh
mkdir -p ~/Machines/windows-vm
wget https://fedorapeople.org/groups/virt/virtio-win/direct-downloads/stable-virtio/virtio-win.iso
```

Create the disk

```sh
cd ~/Machines/windows-vm
qemu-img create -f qcow2 windows11.qcow2 64G
```

Create the script to run the VM:

```sh
#!/bin/sh

# Create TPM directory
mkdir -p /tmp/tpm

# Start software TPM
swtpm socket --tpmstate dir=/tmp/tpm --ctrl type=unixio,path=/tmp/tpm/swtpm-sock &

qemu-system-x86_64 \
  -enable-kvm \
  -cpu host \
  -smp 8 \
  -m 8G \
  -drive file=windows11.qcow2,if=virtio,format=qcow2 \
  -cdrom ~/Machines/Win11_24H2_English_x64.iso \
  -drive file=virtio-win.iso,media=cdrom \
  -boot order=d \
  -bios /usr/share/OVMF/OVMF.fd \
  -device virtio-vga \
  -device virtio-net,netdev=net0 \
  -netdev user,id=net0 \
  -usb -device usb-tablet \
  -device virtio-rng-pci \
  -chardev socket,id=chrtpm,path=/tmp/tpm/swtpm-sock \
  -tpmdev emulator,id=tpm0,chardev=chrtpm \
  -device tpm-tis,tpmdev=tpm0

# Cleanup
pkill swtpm
```

Run the machine. When prompted, hit enter to boot the Windows installer from CD:

```sh
chmod +x run.sh
./run.sh
```

During installation, you will need to load drivers from `virtio-win.iso` which 
is mounted as the 2nd CDROM:
1. Drivers for the hard disk: `viostor -> w11 -> amd64`
2. Network drivers: `NetKVM -> w11 -> amd64`

After installation it's recommended to install the Display Dirivers for better
resolution and performance. 

Right click on the start button -> Device Manager. Find Display adapters and change
the driver for the device: use the folder `viogpudo -> w11 -> amd64`.

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

