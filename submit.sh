#!/bin/sh
set -e -u -x
lman=$1
tmpdir=$(mktemp -dt icfpc14.XXXXXXXXXX)
final=${2:-submission.tgz}
mkdir "$tmpdir/solution" "$tmpdir/code"
cp "$lman" "$tmpdir/solution"
git ls-tree -r --name-only HEAD | cpio -pdmv "$tmpdir/code"
tar -zf "$final" -C "$tmpdir" -vc code solution
rm -r "$tmpdir"
sha1sum "$final" || sha1 "$final" || openssl sha1 "$final"
