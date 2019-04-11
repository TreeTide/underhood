# Common settings used for nixops-based nixos deployments.
{}:

{
  # Not critical, but removes annoying error from `nixops check` output.
  # See https://github.com/NixOS/nixops/issues/574.
  systemd.additionalUpstreamSystemUnits =
      [ "proc-sys-fs-binfmt_misc.automount"
        "proc-sys-fs-binfmt_misc.mount"
      ];
}
