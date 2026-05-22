{
  inputs,
  ...
}:

{
  flake.modules.nixos.asus-tuf-a17 =
    {
      config,
      lib,
      pkgs,
      ...
    }:
    {
      imports = [
        inputs.home-manager.nixosModules.home-manager
        inputs.nixos-hardware.nixosModules.common-pc-laptop
        inputs.nixos-hardware.nixosModules.common-pc-laptop-ssd
        inputs.nixos-hardware.nixosModules.common-cpu-amd
        inputs.nixos-hardware.nixosModules.common-gpu-nvidia
      ];

      boot = {
        initrd.availableKernelModules = [
          "xhci_pci"
          "nvme"
          "ahci"
          "usb_storage"
          "sd_mod"
        ];
        kernelModules = [ "kvm-amd" ];
        kernelParams = [ "nvidia_drm.fbdev=1" ];

        loader = {
          systemd-boot.enable = true;
          efi.canTouchEfiVariables = true;
          timeout = 3;
        };

        binfmt.emulatedSystems = [ "aarch64-linux" ];
      };

      hardware = {
        cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

        nvidia = {
          prime = {
            intelBusId = "PCI:5:0:0";
            nvidiaBusId = "PCI:1:0:0";
          };
          open = false;
        };

        nvidia-container-toolkit.enable = true;

        acpilight.enable = true;

        trackpoint.enable = true;
      };

      networking = {
        useDHCP = lib.mkDefault true;
        networkmanager.wifi.backend = "iwd";
      };

      environment.systemPackages = [ pkgs.nvidia-container-toolkit ];

      services.upower.enable = true;

      nixpkgs = {
        hostPlatform = "x86_64-linux";
        allowedUnfreePackages = [
          "nvidia-x11"
          "nvidia-settings"
        ];
      };
    };
}
