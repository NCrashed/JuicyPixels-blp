let
  nixpkgs = import ./pkgs.nix;
  project = import ((nixpkgs {}).fetchFromGitHub {
    owner = "NCrashed";
    repo = "haskell-nix";
    rev = "b3adf96fe319c59d4d8f0ddde23b75a2e452c087";
    sha256  = "0ld2bfhval31i6f23cvlz33ays6gaazhqg1r16y0aikwaciw0np7";
  }) { inherit nixpkgs; };
in project {
  packages = {
    JuicyPixels-blp = ./.;
  };
}
