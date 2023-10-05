#/bin/sh

export COMMITID=`git log -n1 --format="%h"`
echo commitid = $COMMITID
sed -i "s/^version:.*/version: \"$COMMITID\"/"  ./snap/snapcraft.yaml
snapcraft

