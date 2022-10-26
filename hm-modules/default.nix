{ self }:

{
  mixins = import ./mixins { inherit self; };

  spicetify = import ./spicetify.nix;
}
