#!/bin/sh

set -ex

NEW_VERSION="$(gh release list -R 5etools-mirror-1/5etools-mirror-1.github.io -L 1 | sed 's/\(v[0-9\.]*\)\s*Latest.*/\1/')"
SCRIPTPATH="$( cd "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
URL="https://github.com/5etools-mirror-1/5etools-mirror-1.github.io/releases/download/$NEW_VERSION/5etools-$NEW_VERSION.zip"
HASH="$(nix-prefetch-url --unpack "$URL")"

echo "{ url = \"$URL\"; sha256 = \"$HASH\";}" > "$SCRIPTPATH/5etools-url.nix"
