//! Integration tests for CO I/O semantics in both C and interpreter backends.

use speculate::speculate;
use std::io::Write;
use std::path::Path;
use std::process::{Command, Stdio};
use tempfile::{Builder, NamedTempFile};

speculate! {
    describe "reading integers" {
        const SOURCE: &'static str = r#"
            fun main() {
                var a: int, b: int;
                read a, b;
                writeln a;
                writeln b;
            }
        "#;

        before {
            let mut source_file = NamedTempFile::new().unwrap();
            source_file.write_all(SOURCE.as_bytes()).unwrap();
        }

        describe "when they are positive" {
            const INPUT: &'static [u8] = b"2 4\n";
            const OUTPUT: &'static [u8] = b"2\n4\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "when they are negative" {
            const INPUT: &'static [u8] = b"-2 -4\n";
            const OUTPUT: &'static [u8] = b"-2\n-4\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }
    }

    describe "reading string words" {
        const SOURCE: &'static str = r#"
            fun main() {
                var a: string, b: string, c: string;
                read a;
                read b;
                read c;
                writeln a;
                writeln b;
                writeln c;
            }
        "#;

        before {
            let mut source_file = NamedTempFile::new().unwrap();
            source_file.write_all(SOURCE.as_bytes()).unwrap();
        }

        describe "from single newline terminated line" {
            const INPUT: &'static [u8] = b"foo bar baz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "from mixed newline terminated lines" {
            const INPUT: &'static [u8] = b"foo\nbar baz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "from individual newline terminated lines" {
            const INPUT: &'static [u8] = b"foo\nbar\nbaz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "from individual newline terminated possibly empty lines" {
            const INPUT: &'static [u8] = b"\nfoo\n\nbar\n\n\nbaz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "from a single line without terminating newline" {
            const INPUT: &'static [u8] = b"foo bar baz";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "from multiple lines without terminating newline" {
            const INPUT: &'static [u8] = b"foo\nbar\nbaz";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }
    }

    describe "reading whole lines" {
        const SOURCE: &'static str = r#"
            fun main() {
                var a: string, b: string;
                readln a;
                readln b;
                writeln a;
                writeln b;
            }
        "#;

        before {
            let mut source_file = NamedTempFile::new().unwrap();
            source_file.write_all(SOURCE.as_bytes()).unwrap();
        }

        describe "when lines are single words" {
            const INPUT: &'static [u8] = b"foo\nbar\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "when lines are sentences" {
            const INPUT: &'static [u8] = b"hello world\nhi\n";
            const OUTPUT: &'static [u8] = b"hello world\nhi\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "when lines may be empty" {
            const INPUT: &'static [u8] = b"\nhello world\n\n\nhi\n\n";
            const OUTPUT: &'static [u8] = b"hello world\nhi\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "when no terminating endline" {
            const INPUT: &'static [u8] = b"hello world\nhi";
            const OUTPUT: &'static [u8] = b"hello world\nhi\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }
    }

    describe "reading mixed words and lines" {
        const SOURCE: &'static str = r#"
            fun main() {
                var w1: string, w2: string;
                var l1: string, l2: string;
                
                read w1;
                readln l1;
                read w2;
                readln l2;
                
                writeln w1;
                writeln l1;
                writeln w2;
                writeln l2;
            }
        "#;

        before {
            let mut source_file = NamedTempFile::new().unwrap();
            source_file.write_all(SOURCE.as_bytes()).unwrap();
        }

        describe "in trivial case" {
            const INPUT: &'static [u8] = b"foo\nbar\nbaz\nxyz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\nxyz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }

        describe "when some words must be dropped" {
            const INPUT: &'static [u8] = b"foo foo2\nbar\nbaz baz2 baz3\nxyz\n";
            const OUTPUT: &'static [u8] = b"foo\nbar\nbaz\nxyz\n";

            it "works in interpreter" {
                assert_eq!(run_with_interpreter(source_file.path(), INPUT), OUTPUT);
            }

            it "works in compiler" {
                assert_eq!(run_with_compiler(source_file.path(), INPUT), OUTPUT);
            }
        }
    }
}

fn run_with_interpreter(file_path: &Path, input: &[u8]) -> Vec<u8> {
    let mut child = Command::new(env!("CARGO_BIN_EXE_colang-cli"))
        .arg("run")
        .arg(&file_path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();

    {
        let child_stdin = child.stdin.as_mut().unwrap();
        child_stdin.write_all(input).unwrap();
    }

    let output = child.wait_with_output().unwrap();
    assert!(output.status.success());
    output.stdout
}

fn run_with_compiler(file_path: &Path, input: &[u8]) -> Vec<u8> {
    let c_file = Builder::new()
        .suffix(".c")
        .rand_bytes(10)
        .tempfile()
        .unwrap()
        .into_temp_path();

    let colang_status = Command::new(env!("CARGO_BIN_EXE_colang-cli"))
        .arg("compile")
        .arg(&file_path)
        .arg("-o")
        .arg(&c_file)
        .status()
        .unwrap();
    assert!(colang_status.success());

    let run_file = Builder::new()
        .suffix(".run")
        .rand_bytes(10)
        .tempfile()
        .unwrap()
        .into_temp_path();
    let gcc_status = Command::new("gcc")
        .arg("-o")
        .arg(&run_file)
        .arg(&c_file)
        .status()
        .unwrap();
    assert!(gcc_status.success());

    let mut child = Command::new(&run_file)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    {
        let child_stdin = child.stdin.as_mut().unwrap();
        child_stdin.write_all(input).unwrap();
    }

    let program_output = child.wait_with_output().unwrap();
    assert!(program_output.status.success());
    program_output.stdout
}
