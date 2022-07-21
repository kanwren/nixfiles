final: prev:

{
  firmwareLinuxNonfree = prev.firmwareLinuxNonfree.overrideAttrs (_: {
    version = "2020-12-18";
    src = prev.fetchgit {
      url = "https://git.kernel.org/pub/scm/linux/kernel/git/firmware/linux-firmware.git";
      rev = "b79d2396bc630bfd9b4058459d3e82d7c3428599";
      sha256 = "sha256-ZQ2gbWq6XEqF5Cz8NsMGccevccDmXOymiavM/t1YZeU=";
    };
    outputHash = "sha256-hgTfrOmKKpVK+qGuaFtFURLCwcG/cCiT4UYx7qCw+9w=";
  });
}
