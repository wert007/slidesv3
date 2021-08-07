use assert_matches::assert_matches;

use crate::{instruction_converter::instruction::op_codes::OpCode, text::SourceText};

use super::*;

#[test]
fn instruction_converter_success() {
    converter_helper("1;", |instructions| {
        assert_eq!(instructions.len(), 2);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 1
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::Pop,
                arg: 0
            }
        );
    });

    converter_helper("-99;", |instructions| {
        assert_eq!(instructions.len(), 3);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 99
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::BitwiseTwosComplement,
                arg: 0
            }
        );
        assert_matches!(
            instructions[2],
            Instruction {
                op_code: OpCode::Pop,
                arg: 0
            }
        );
    });

    converter_helper("321 - 99;", |instructions| {
        assert_eq!(instructions.len(), 4);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 321
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 99
            }
        );
        assert_matches!(
            instructions[2],
            Instruction {
                op_code: OpCode::Subtraction,
                arg: 0
            }
        );
        assert_matches!(
            instructions[3],
            Instruction {
                op_code: OpCode::Pop,
                arg: 0
            }
        );
    });

    converter_helper("321 != 99;", |instructions| {
        assert_eq!(instructions.len(), 4);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 321
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 99
            }
        );
        assert_matches!(
            instructions[2],
            Instruction {
                op_code: OpCode::NotEquals,
                arg: 0
            }
        );
        assert_matches!(
            instructions[3],
            Instruction {
                op_code: OpCode::Pop,
                arg: 0
            }
        );
    });

    converter_helper("if true { let a = 3; }", |instructions| {
        assert_eq!(instructions.len(), 4);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 1
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::JmpIfFalse,
                arg: 2
            }
        );
        assert_matches!(
            instructions[2],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 3
            }
        );
        assert_matches!(
            instructions[3],
            Instruction {
                op_code: OpCode::StoreInRegister,
                arg: 0
            }
        );
    });

    converter_helper("while false { let a = 3; }", |instructions| {
        assert_eq!(instructions.len(), 5);
        assert_matches!(
            instructions[0],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 0
            }
        );
        assert_matches!(
            instructions[1],
            Instruction {
                op_code: OpCode::JmpIfFalse,
                arg: 3
            }
        );
        assert_matches!(
            instructions[2],
            Instruction {
                op_code: OpCode::LoadImmediate,
                arg: 3
            }
        );
        assert_matches!(
            instructions[3],
            Instruction {
                op_code: OpCode::StoreInRegister,
                arg: 0
            }
        );
        // 18446744073709551611 == -5
        assert_matches!(
            instructions[4],
            Instruction {
                op_code: OpCode::JmpRelative,
                arg: 18446744073709551611
            }
        );
    });
}

fn converter_helper(input: &str, callback: impl FnOnce(&[Instruction]) -> ()) {
    let source_text = SourceText::new(input, "");
    let mut diagnostic_bag = DiagnosticBag::new(&source_text);
    let result = convert(&source_text, &mut diagnostic_bag, DebugFlags::default());
    assert!(!diagnostic_bag.has_errors());
    callback(&result[2..])
}
