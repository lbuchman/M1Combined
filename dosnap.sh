#!/bin/sh

hash=`git rev-parse --short HEAD`
sed -i '/version:/c\version: '"${hash}"'' snap/snapcraft.yaml
snapcraft
