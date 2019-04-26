let
  pkgs1903Beta = fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/19.03-beta.tar.gz;
    sha256 = "1wr6dzy99rfx8s399zjjjcffppsbarxl2960wgb0xjzr7v65pikz";
  };
in
  { pkgs1903 = import pkgs1903Beta {};
  }
