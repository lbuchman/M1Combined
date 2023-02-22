# Testfixture PC

1. Install Ubuntu-Mate 20.04

2. Setting->Power Mng->Disable Screen Saver and Screen Blank
   
3. Edit 

   ```sudo nano /etc/default/grub
   sudo nano /etc/default/grub # change CMDLINE_LINUX_DEFAULT="quiet pcie_aspm=off splash libata.noacpi=1"
   sudo update-grub
   ```

10. Install nodejs 16
    curl -sL https://deb.nodesource.com/setup_16.x -o nodesource_setup.sh
    sudo bash nodesource_setup.sh
    sudo apt update
    sudo apt  install nodejs

11. edit

    ```nano /etc/enviroment```

    replace file with the context below


```
PATH="/snap/bin:/home/lenel/.local/bin:/home/lenel/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin:/opt/arm-gnu-toolchain-12.2.mpacbti-bet1-x86_64-arm-none-eabi/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"
export BROTHER_QL_PRINTER=usb://0x04f9:0x209c
export BROTHER_QL_MODEL=QL-810W
```




add autossh, desktop program,keys

9. Use install.txz file and unpack it to any directory. Change to this directory and run sudo ./install.sh script.

   The STM32CubeProgrammer, required udev rules and network configuration files will be installed.

10. ```sudo reboot```

11. Autossh

    modify autossh,service and select not used on the relay server port

    testf 1 is port 20006, the rest is + 1

    cp autossh.service to /lib/systemd/system

    make sure to copy keys 

    cloud.key -> ~/.ssh/

    pikey -> ~/.ssh/

    pikey.pub -> ~/.ssh/authorized_keys 

On service machine to connect you need m1pikey

examplelenelr@supportvm1.eastus.cloudapp.azure.com -i /home/lenelcloud.key -p 20007



9. cp gui to /usr/bin
10. cp gui.desktop to ~/Desktop
11. cp rules/99-usb-serial.rules to /etc/udev/rules, but edit the file before and change serial numbers for usb/serial cables so the match test board and M1

SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", GROUP="users", MODE="0666"
SUBSYSTEMS=="usb", ATTRS{idVendor}=="04f9", ATTRS{idProduct}=="209c", GROUP="users", MODE="0666"


21. add dialout group to lenel user
    sudo usermod -a -G dialout $USER
    sudo reboot

22. Install M1 images into $HOME/fw, ths include BM.stm32 and yocto images

23. copy autossh.service and change the port

24. Add to /etc/enviroment 
        /home/lbuchman/.local/bin && 
    16 install brother_ql

       pip install --upgrade brother_ql

    ***sudo apt-get remove --auto-remove ippusbxd***

    sudo apt-mark hold ippusbxd

    sudo chmod 4755  /usr/sbin/arp-scan

25. Example /etc/enviroment
    PATH="/snap/bin:/home/lenel/.local/bin:/home/lenel/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin:/opt/arm-gnu-toolchain-12.2.mpacbti-bet1-x86_64-arm-none-eabi/bin/:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin"
    export BROTHER_QL_PRINTER=usb://0x04f9:0x209c
    export BROTHER_QL_MODEL=QL-810W

26. Add to 99-usb-serial.rules  rule for the label Printer
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="04f9", ATTRS{idProduct}=="209c", GROUP="users", 

27. 

SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", GROUP="users", MODE="0666" 
SUBSYSTEMS=="usb", ATTRS{idVendor}=="04f9", ATTRS{idProduct}=="209c", GROUP="users", MODE="0666"



27. https://ostechnix.com/how-to-find-usb-device-bandwidth-usage-on-linux/
do not do grub

there are netword files in rep



$ sudo systemctl disable NetworkManager
$ sudo systemctl enable systemd-networkd

$ sudo systemctl enable systemd-resolved
$ sudo systemctl start systemd-resolved

$ sudo rm /etc/resolv.conf
$ sudo ln -s /run/systemd/resolve/resolv.conf /etc/resolv.conf

$ sudo mkdir /etc/systemd/network

#/etc/systemd/network/10-persistent-lan0.link
[Match]
MACAddress=00:e0:4c:88:53:02
[Link]
Name=eth0
[Network]
DHCP=yes



or

[Network]
Address=192.168.10.50/24


https://rigacci.org/wiki/doku.php/doc/appunti/linux/sa/if_rename

https://www.xmodulo.com/switch-from-networkmanager-to-systemd-networkd.html