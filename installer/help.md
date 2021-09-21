Some offline documentation to help me during installation.

# USB Wi-Fi Adapter

Sample

Sample `/etc/interfaces` to configure USB Wi-Fi adapter on a Raspberry Pi.

```
auto wlan0
allow-hotplug wlan0
iface wlan0 inet manual
wpa-roam /etc/wpa_supplicant.conf
```

