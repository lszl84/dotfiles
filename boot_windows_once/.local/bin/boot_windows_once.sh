#!/bin/bash
# Secure Windows boot selector for Fedora (uses pkexec)

set_windows_boot() {
    # Find Windows boot entry (Fedora uses /boot/efi/EFI)
    WINDOWS_ENTRY=$(efibootmgr | grep -i windows | awk '{print $1}' | sed 's/Boot//;s/\*//')
    
    [ -z "$WINDOWS_ENTRY" ] && {
        notify-send -u critical "Error" "Windows boot entry not found in EFI"
        exit 1
    }

    # Set next boot (Fedora method)
    if ! efibootmgr --bootnext "$WINDOWS_ENTRY"; then
        notify-send -u critical "Error" "Failed to set Windows as next boot"
        exit 1
    fi

    systemctl reboot
}

# Main execution
if [ "$(id -u)" -eq 0 ]; then
    set_windows_boot
else
    pkexec --disable-internal-agent "$0"
fi
