#!/bin/sh
set -e
MAC1=$1
MAC2=$2
SSHPORT=$3
HOSTNAME=$4
STARTMAC=$5

usage() {
        echo "usage: $0 MAC1 MAC2 SSHPORT HOSTNAME<m1testf?> STARTMAC<00:0F:A6:00:00:00>"
}


if [ -z "MAC1" ]; then
   echo must sspecify MAC1 address
   usage
   exit 1
fi

if [ -z "MAC2" ]; then
   echo must sspecify MAC1 address
   usage
   exit 1
fi

if [ -z "SSHPORT" ]; then
   usage
   exit 1
fi

if [ -z "HOSTNAME" ]; then
   usage
   exit 1
fi


if [ -z "STARTMAC" ]; then
   usage
   exit 1
fi

echo MAC1=$MAC1 MAC2=$MAC2 SSHPORT=$SSHPORT HOSTNAME=$HOSTNAME STARTMAC=$STARTMAC

mkdir -p /home/lenel/m1mtf
sudo apt update
sudo apt upgrade
sudo apt install net-tools openssh-server
sudo apt install sqlite3 arp-scan curl  python3-pip autossh ethtool imagemagick-6.q16  libusb-1.0-0 cron
sudo chmod 4755  /usr/sbin/arp-scan
pip install --upgrade brother_ql
sudo apt remove ippusbxd
sudo apt-mark hold ippusbxd
sudo sed -i '/CMDLINE_LINUX_DEFAULT/c\CMDLINE_LINUX_DEFAULT="quiet pcie_aspm=off splash libata.noacpi=1"' /etc/default/grub
sudo update-grub
echo "PATH=\"/home/lenel/.local/bin:/home/lenel/STMicroelectronics/STM32Cube/STM32CubeProgrammer/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin\"" > /etc/environment
sed -i '/BROTHER_QL_PRINTER/d' /etc/environment
echo "export BROTHER_QL_PRINTER=usb://0x04f9:0x209c" >> /etc/environment
echo "export BROTHER_QL_MODEL=QL-810W" >> /etc/environment

cp -f rules.d/* /etc/udev/rules.d
mkdir -p /home/lenel/.ssh
echo "cp -f cloud.key id_rsa authorized_keys /home/lenel/.ssh"
cp -f cloud.key id_rsa authorized_keys /home/lenel/.ssh
sudo chown lenel: .* -R /home/lenel/.ssh
chmod 700 /home/lenel/.ssh
sudo usermod -a -G dialout lenel
cp -f M1-3200.desktop /home/lenel/Desktop
tar -xJf STMicroelectronics.txz -C /home/lenel
sudo mkdir -p /etc/systemd/network
sudo cp network/* /etc/systemd/network
sed -i 's/00:01:29:9f:fd:1d/'"${MAC1}"'/' /etc/systemd/network/10-persistent-eth0.link
sed -i 's/00:01:29:9f:fd:1e/'"${MAC2}"'/' /etc/systemd/network/10-persistent-eth1.link
systemctl disable NetworkManager
systemctl enable systemd-networkd
systemctl enable systemd-resolved
systemctl start systemd-resolved
rm /etc/resolv.conf
ln -sf /run/systemd/resolve/resolv.conf /etc/resolv.conf
cp -f autossh.service /lib/systemd/system
systemctl restart autossh
sed -i 's/20007/'"${SSHPORT}"'/'  /lib/systemd/system/autossh.service
curl -sL https://deb.nodesource.com/setup_16.x -o /tmp/nodesource_setup.sh
sudo bash /tmp/nodesource_setup.sh
sudo apt update
sudo apt  install nodejs

cp -f tf.db  /home/lenel/m1mtf
sqlite3 /home/lenel/m1mtf/tf.db "insert into uid values ('${STARTMAC}')"
sed -i '/m1client/d' /etc/crontab
sed -i '/systemctl/d' /etc/crontab
sed -i '/m1mtf/d' /etc/crontab

echo "@reboot sleep 120  && systemctl restart autossh" >> /etc/crontab
echo "0  3  * * *   root /snap/bin/m1client update > /etc/log" >> /etc/crontab
echo "20  3  * * *   root /snap/bin/m1client synclogs > /etc/log" >> /etc/crontab
echo "40  3  * * *   root /snap/sbin/m1client syncsecrets > /etc/log" >> /etc/crontab
echo "50  3  * * *   root find //home/lenel/m1mtf/logs -type f -mtime +90 -delete > /tmp/log" >> /etc/crontab
echo "10  4  * * *   root find /home/lenel/m1mtf/logs -type d -mtime +90 -delete > /tmp/log" >> /etc/crontab

snap install --classic --dangerous  m1client.snap 
snap install --classic --dangerous  m1tfd1.snap
cp config.json public.key /var/snap/m1tfd1/current

echo $HOSTNAME > /etc/hostname
sed -i 's/lenel-EB100-KU0061/'"${HOSTNAME}"'/' /etc/hosts
sudo chown lenel: * -R /home/lenel

m1client update

