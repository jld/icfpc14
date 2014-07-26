#!/bin/sh
set -e -u -x
lman=$1
tmpdir=$(mktemp -dt icfpc14.XXXXXXXXXX)
mkdir "$tmpdir/solution" "$tmpdir/code"
cp "$lman" "$tmpdir/solution"
git ls-tree -r --name-only HEAD | cpio -pdmv "$tmpdir/code"
tar -zf submission.tgz -C "$tmpdir" -vc code solution
rm -r "$tmpdir"
