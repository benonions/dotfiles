{
  lib,
  fetchFromGitHub,
  installShellFiles,
  makeWrapper,
  openssl,
  pkl,
  pkg-config,
  python3,
  rustPlatform,
  stdenv,
  usage,
  zlibCompat ? null,
}:

rustPlatform.buildRustPackage (finalAttrs: {
  pname = "hk";
  version = "1.38.0";

  src = fetchFromGitHub {
    owner = "jdx";
    repo = "hk";
    rev = "v${finalAttrs.version}";
    fetchSubmodules = true;
    hash = "sha256-GVfx+g0pF+/5vOPlfIOZYU+1Zhc5zTZiu2BDbCXu4mE=";
  };

  cargoHash = "sha256-S1v1Yo3sXqPgpFrrGJsVo4qA3jgM8tbvq9mJeB+/mdk=";

  env = {
    OPENSSL_DIR = "${openssl.out}";
    OPENSSL_INCLUDE_DIR = "${openssl.dev}/include";
    OPENSSL_LIB_DIR = "${openssl.out}/lib";
    OPENSSL_NO_VENDOR = 1;
  };

  nativeBuildInputs = [
    installShellFiles
    makeWrapper
    pkg-config
    python3
    usage
  ];

  buildInputs = [ openssl ]
    ++ lib.optionals (stdenv.hostPlatform.isLinux && zlibCompat != null) [ zlibCompat ];

  cargoBuildFlags = [
    "--package"
    "hk"
    "--bin"
    "hk"
  ];

  doCheck = false;

  postInstall = ''
    installShellCompletion --cmd hk \
      --bash <($out/bin/hk completion bash) \
      --fish <($out/bin/hk completion fish) \
      --zsh <($out/bin/hk completion zsh)

    mkdir -p $out/share/hk
    cp -r pkl $out/share/hk/

    wrapProgram $out/bin/hk \
      --prefix PATH : ${lib.makeBinPath [ pkl usage ]}
  '';

  meta = {
    description = "Git hook and pre-commit lint manager";
    homepage = "https://hk.jdx.dev";
    license = lib.licenses.mit;
    mainProgram = "hk";
    platforms = lib.platforms.unix;
  };
})
