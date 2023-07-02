set positional-arguments

@run *args='./examples/foo.sld':
    cargo run --quiet -- $@

@test *args:
    cd ./lang-tests && cargo run --package lang-tests --quiet -- "$@" --directory "../tests"
#    cargo test -q -p slides --message-format short