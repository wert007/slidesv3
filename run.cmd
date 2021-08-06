@echo off
pushd reaktor
cargo run -- "..\examples\%1.sld" %2 %3 %4 %5 %6
popd