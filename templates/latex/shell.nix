{ mkShell
, tex-env
,
}:
mkShell {
  packages = [ tex-env ];
}
