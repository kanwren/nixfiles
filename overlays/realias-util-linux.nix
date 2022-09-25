# Temporarily realias utillinux to util-linux to fix slack, fontconfig, etc.

final: prev:

{
  utillinux = final.util-linux;
}
