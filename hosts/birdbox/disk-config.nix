{lib, ...}: {
  boot.initrd.postResumeCommands =
    lib.mkAfter
    /*
    bash
    */
    ''
      mkdir -p /btrfs_tmp
      mount /dev/disk/by-partlabel/disk-ssd-primary /btrfs_tmp

      # save the old root
      if [[ -e /btrfs_tmp/root ]]; then
        mkdir -p /btrfs_tmp/old_roots
        time="$(stat -c '%Y' /btrfs_tmp/root)"
        timestamp="$(date --date='@'"$time" '+%Y-%m-%d_%H:%M:%S')"
        mv /btrfs_tmp/root '/btrfs_tmp/old_roots/'"$timestamp"
      fi

      # delete roots older than 30 days
      delete_subvolume() {
        local subvolume="$1"
        btrfs subvolume list -o "$subvolume" \
        | cut -f9- -d' ' \
        | while IFS= read -r subsubvolume; do
          delete_subvolume '/btrfs_tmp/'"$subsubvolume"
        done
        btrfs subvolume delete "$subvolume"
      }
      find /btrfs_tmp/old_roots/ -maxdepth 1 -mtime +30 -print0 \
      | while IFS= read -r -d ''' subvolume; do
        delete_subvolume "$subvolume"
      done

      # make a new blank root
      btrfs subvolume create /btrfs_tmp/root

      umount /btrfs_tmp
    '';

  fileSystems = {
    "/persist".neededForBoot = true;
  };

  disko.devices = {
    disk = {
      ssd = {
        type = "disk";
        device = "/dev/disk/by-diskseq/9";
        content = {
          type = "gpt";
          partitions = {
            ESP = {
              priority = 1;
              name = "ESP";
              start = "1M";
              end = "1GiB";
              type = "EF00";
              content = {
                type = "filesystem";
                format = "vfat";
                mountpoint = "/boot";
                mountOptions = ["defaults" "umask=0077"];
              };
            };
            primary = {
              end = "-20G";
              content = {
                type = "btrfs";
                extraArgs = ["-f"];
                subvolumes = {
                  "/root" = {
                    mountpoint = "/";
                  };
                  "/nix" = {
                    mountpoint = "/nix";
                    mountOptions = ["compress=zstd" "noatime"];
                  };
                  "/persist" = {
                    mountpoint = "/persist";
                    mountOptions = ["compress=zstd" "noatime"];
                  };
                };
              };
            };
            swap = {
              size = "100%";
              content = {
                type = "swap";
                discardPolicy = "both";
                resumeDevice = true;
              };
            };
          };
        };
      };

      hdd = {
        type = "disk";
        device = "/dev/disk/by-diskseq/10";
        content = {
          type = "gpt";
          partitions = {
            root = {
              size = "100%";
              content = {
                type = "filesystem";
                format = "ext4";
                mountpoint = "/media/hdd";
              };
            };
          };
        };
      };
    };
  };
}
