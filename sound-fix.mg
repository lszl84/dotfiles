Good plan! Let's implement the permanent configuration so it takes effect after reboot. Here's how to do it step by step:

## 1. Create the modprobe configuration file

```bash
# Create the modprobe configuration file
sudo nano /etc/modprobe.d/sof-fix.conf
```

Add these lines to the file:
```
options snd_sof_intel_hda_common sof_use_tplg_nhlt=1
options snd_sof_pci tplg_filename=sof-hda-generic-ace1-4ch.tplg
```

Or use echo commands:
```bash
echo "options snd_sof_intel_hda_common sof_use_tplg_nhlt=1" | sudo tee /etc/modprobe.d/sof-fix.conf
echo "options snd_sof_pci tplg_filename=sof-hda-generic-ace1-4ch.tplg" | sudo tee -a /etc/modprobe.d/sof-fix.conf
```

## 2. Create the firmware symlink

```bash
# Create the symlink for firmware
sudo ln -s /lib/firmware/intel/sof-ace-tplg /lib/firmware/intel/sof-ipc4-tplg
```

## 3. Verify the files were created correctly

```bash
# Check the modprobe file
cat /etc/modprobe.d/sof-fix.conf

# Check the symlink
ls -la /lib/firmware/intel/sof-ipc4-tplg
```

## 4. Reboot and test

```bash
sudo reboot
```

## 5. After reboot, configure PulseAudio/PipeWire

Once rebooted:
```bash
# Open pavucontrol
pavucontrol
```

Go to the **Configuration** tab and for "Meteor Lake-P HD Audio Controller":
- Change from the headphone profile to the speaker profile

## 6. Test the sound

```bash
# Test all speakers
speaker-test -t wav -c 6

# Or play a test sound
aplay /usr/share/sounds/alsa/Front_Left.wav
aplay /usr/share/sounds/alsa/Front_Right.wav
```

## Optional: Check if modules loaded with correct parameters

```bash
# Verify the parameters were applied after reboot
cat /sys/module/snd_sof_intel_hda_common/parameters/sof_use_tplg_nhlt
cat /sys/module/snd_sof_pci/parameters/tplg_filename
```

This approach is much cleaner since you won't have to fight with processes that are using the audio modules. The configuration will be applied during boot when the modules are first loaded.

If anything goes wrong, you can simply:
1. Delete `/etc/modprobe.d/sof-fix.conf`
2. Remove the symlink: `sudo rm /lib/firmware/intel/sof-ipc4-tplg`
3. Reboot again to return to defaults

Let me know how it works after the reboot!
