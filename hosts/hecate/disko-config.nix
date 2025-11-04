{
  disko.devices = {
    disk = {
      main = {
        device = "/dev/disk/by-id/nvme-Micron_2210_MTFDHBA1T0QFD_203029A09CA4";
        type = "disk";
        content = {
          type = "gpt";
          partitions = {
            EFI = {
              type = "EF00";
              size = "500M";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = [ "umask=0077" ];
              };
            };
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/";
              };
            };
          };
        };
      };
    };
  };
}
