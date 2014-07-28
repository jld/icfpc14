#!/bin/sh
set -e -u -x
lman=$1
tmpdir=$(mktemp -dt icfpc14.XXXXXXXXXX)
final=${2:-submission.tgz}
ghost=${3:-}
if [ -e "$final" ]; then
    set +x
    echo "*** ERROR: $final already exists" >&2
    exit 1
fi
mkdir "$tmpdir/solution" "$tmpdir/code"
cp "$lman" "$tmpdir/solution/lambdaman.gcc"
if [ -n "$ghost" ]; then
    ./gpp.pl "$ghost" > "$tmpdir/solution/ghost0.ghc"
fi
git ls-tree -r --name-only HEAD | cpio -pdmv "$tmpdir/code"
tar -zf "$final" -C "$tmpdir" -vc code solution
rm -r "$tmpdir"
sha1sum "$final" || sha1 "$final" || openssl sha1 "$final"
