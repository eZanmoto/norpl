# Copyright 2021 Sean Kelleher. All rights reserved.
# Use of this source code is governed by an MIT
# licence that can be found in the LICENCE file.

# `$0 <img-name> <tag>` builds a docker image that replaces the docker image
# `<img-name>:<tag>`, or creates it if it doesn't already exist.
#
# This script uses `<img-name>:cached` as a temporary tag and so may clobber
# such existing images if present.

if [ $# -lt 2 ] ; then
    echo "usage: $0 <img-name> <tag> ..." >&2
    exit 1
fi

img_name="$1"
shift
tag="$1"
shift

docker tag "$img_name:$tag" "$img_name:cached" &>/dev/null
if docker build --tag="$img_name:$tag" $@ ; then
    docker rmi "$img_name:cached" &>/dev/null
    # We return a success code in case `rmi` failed.
    true
else
    exit_code=$?
    docker tag "$img_name:cached" "$img_name:$tag" &>/dev/null
    exit $exit_code
fi
