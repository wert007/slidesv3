use std::env;
use std::fs;
use std::fs::File;
use std::io;
use std::io::Write;
use std::path::Path;
use std::path::PathBuf;

// build script's entry point
fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let destination = Path::new(&out_dir).join("tests.rs");
    let mut test_file = File::create(destination).unwrap();

    // write test file header, put `use`, `const` etc there
    write_header(&mut test_file);

    write_tests(&mut test_file, "../tests/".into()).unwrap();

    test_file.flush().unwrap();
}

fn write_tests(test_file: &mut File, path: PathBuf) -> io::Result<()> {
    if path.is_dir() {
        let test_data_directories = fs::read_dir(path)?;
        for directory in test_data_directories {
            let directory = directory?;
            write_tests(test_file, directory.path())?;
        }
    } else {
        match path.extension().unwrap().to_str().unwrap() {
            "sld" => write_test(test_file, path),
            _ => {}
        }
    }
    Ok(())
}

fn write_test(test_file: &mut File, path: PathBuf) {
    let test_name = format!("test_file_{}", path.file_stem().unwrap().to_string_lossy());
    let path = path.canonicalize().unwrap();
    let mut path_output = path.clone();
    path_output.set_extension("out");
    write!(
            test_file,
            r#"
#[test]
fn {name}() {{
    let output = Command::new("../target/debug/reaktor.exe").arg("--test-runner").arg({path:?}).output().unwrap();
    assert!(output.stderr.is_empty(), "{{}}", String::from_utf8(output.stderr).unwrap());
    let actual_output = String::from_utf8(output.stdout).unwrap();
    let expected_output = include_str!({path_output:?});

    assert_eq!(expected_output, actual_output);
}}
"#,
        name = test_name,
        path = path.display(),
        path_output = path_output.display(),
    )
    .unwrap();
}

fn write_header(test_file: &mut File) {
    write!(
        test_file,
        r#"
use std::process::Command;
"#
    )
    .unwrap();
}
