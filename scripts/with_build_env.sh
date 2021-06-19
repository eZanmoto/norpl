# Copyright 2021 Sean Kelleher. All rights reserved.
# Use of this source code is governed by an MIT
# licence that can be found in the LICENCE file.

set -o errexit

org='ezanmoto'
proj='norpl'
build_img="$org/$proj.build"

bash scripts/docker_rbuild.sh \
    "$build_img" \
    "latest" \
    --file='build.Dockerfile' \
    scripts

vol_name="$org.$proj.cargo_cache"
vol_dir='/cargo'

docker run \
    --rm \
    --mount="type=volume,src=$vol_name,dst=$vol_dir" \
    "$build_img:latest" \
    chmod 0777 "$vol_dir"

work_dir='/app'

docker run \
    --interactive \
    --tty \
    --rm \
    --mount="type=volume,src=$vol_name,dst=$vol_dir" \
    --env="CARGO_HOME=$vol_dir" \
    --user="$(id --user):$(id --group)" \
    --mount="type=bind,src=$(pwd),dst=$work_dir" \
    --workdir="$work_dir" \
    "$build_img:latest" \
    "$@"
