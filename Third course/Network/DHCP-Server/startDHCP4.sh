#!/bin/bash
sudo ifconfig tap0 inet 10.25.155.1
sudo /usr/local/sbin/dhcpd -4 -f -d -cf ./dhcpd4.conf tap0
