self: super:

{
  firmwareLinuxNonfree = super.firmwareLinuxNonfree.overrideAttrs (old: {
    version = "2020-12-18";
    src = super.fetchgit {
      url = "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git";
      rev = "b79d2396bc630bfd9b4058459d3e82d7c3428599";
      sha256 = "1rb5b3fzxk5bi6kfqp76q1qszivi0v1kdz1cwj2llp5sd9ns03b5";
    };
    outputHash = "1p7vn2hfwca6w69jhw5zq70w44ji8mdnibm1z959aalax6ndy146";
  });
}
