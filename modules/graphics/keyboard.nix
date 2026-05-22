{
  flake.modules.nixos.graphics = {
    services.xserver.xkb = {
      layout = "us";
      options = "caps:escape,compose:ralt,terminate:ctrl_alt_bksp";
    };
  };

  flake.modules.darwin.graphics = {
    system.keyboard = {
      enableKeyMapping = true;
      userKeyMapping =
        let
          escapeKey = 30064771113;
          capsLockKey = 30064771129;
          remap = from: to: {
            HIDKeyboardModifierMappingSrc = from;
            HIDKeyboardModifierMappingDst = to;
          };
        in
        [
          # (remap escapeKey capsLockKey)
          (remap capsLockKey escapeKey)
        ];
    };
  };
}
