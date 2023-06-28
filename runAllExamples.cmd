@echo off
pushd reaktor
for %%e in ("..\examples\*.sld") do echo [33mCompiling %%e[0m && cargo run --quiet -- "%%e" %1 %2 %3 %4 %5 %6
popd