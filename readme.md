# Assorted things I've run

`status="okay";` on mmc0 in devicetree

resolvconf -a usb0

echo 1 | sudo tee /proc/sys/net/ipv4/ip_forward > /dev/null
sudo iptables -A POSTROUTING -t nat -j MASQUERADE -s 192.168.7.1
sudo iptables -P FORWARD ACCEPT
