version="$1"

echo "Version: $version"

echo "GHC executables:"

echo

for v in 8.6.4 8.6.5 8.8.2 8.8.3 8.8.4 8.10.1 8.10.2; do
  hash="$(nix-prefetch-url https://github.com/haskell/haskell-language-server/releases/download/$version/haskell-language-server-Linux-$v.gz 2>/dev/null)"
  echo "{ ghcVersion = \"$v\"; sha256 = \"$hash\"; }"
done

echo

wrapper_hash="$(nix-prefetch-url https://github.com/haskell/haskell-language-server/releases/download/$version/haskell-language-server-wrapper-Linux.gz 2>/dev/null)"
echo "Wrapper hash: $wrapper_hash"

