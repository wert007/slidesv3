@echo off
pushd reaktor
    cargo clippy
    cargo fmt
popd

pushd slides
    cargo clippy
    cargo fmt
popd