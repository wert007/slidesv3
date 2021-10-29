@echo off
pushd reaktor
    cargo fix
    cargo +nightly clippy --fix -Z unstable-options
    cargo fmt
popd

pushd slides
    cargo fix
    cargo +nightly clippy --fix -Z unstable-options
    cargo fmt
popd