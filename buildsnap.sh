#/bin/sh

COMMITID=`git log -n1 --format="%h"`
echo commitid = $COMMITID
sed -i "/version:/c\version: \"$COMMITID"\" ./snap/snapcraft.yaml 
snapcraft

