@echo off
pushd reaktor
for %%e in ("..\examples\*.sld") do cargo run -- "%%e" %2 %3 %4 %5 %6
popd