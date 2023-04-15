set positional-arguments

@run *args='./examples/foo.sld':
    cargo run --package reaktor --quiet -- "$@"

@test *args:
    cd ./lang-tests && cargo run --package lang-tests --quiet -- "$@" --directory "../tests"