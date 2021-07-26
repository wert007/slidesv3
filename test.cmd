@echo off
pushd reaktor
    cargo test --quiet
popd

pushd slides
    cargo test --quiet
popd