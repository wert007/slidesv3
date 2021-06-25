use assert_matches::assert_matches;

use crate::instruction_converter::instruction::op_codes::OpCode;

use super::*;

#[test]
fn instruction_converter_success() {
    let instructions = converter_helper("1;", 2);
    assert_matches!(instructions[0], Instruction { op_code: OpCode::LoadImmediate, arg: 1});
    assert_matches!(instructions[1], Instruction { op_code: OpCode::Pop, arg: 0});
    
    let instructions = converter_helper("-99;", 3);
    assert_matches!(instructions[0], Instruction { op_code: OpCode::LoadImmediate, arg: 99});
    assert_matches!(instructions[1], Instruction { op_code: OpCode::BitwiseTwosComplement, arg: 0});
    assert_matches!(instructions[2], Instruction { op_code: OpCode::Pop, arg: 0});

    let instructions = converter_helper("321 - 99;", 4);
    assert_matches!(instructions[0], Instruction { op_code: OpCode::LoadImmediate, arg: 321});
    assert_matches!(instructions[1], Instruction { op_code: OpCode::LoadImmediate, arg: 99});
    assert_matches!(instructions[2], Instruction { op_code: OpCode::Subtraction, arg: 0});
    assert_matches!(instructions[3], Instruction { op_code: OpCode::Pop, arg: 0});

    let instructions = converter_helper("321 != 99;", 4);
    assert_matches!(instructions[0], Instruction { op_code: OpCode::LoadImmediate, arg: 321});
    assert_matches!(instructions[1], Instruction { op_code: OpCode::LoadImmediate, arg: 99});
    assert_matches!(instructions[2], Instruction { op_code: OpCode::NotEquals, arg: 0});
    assert_matches!(instructions[3], Instruction { op_code: OpCode::Pop, arg: 0});
}

fn converter_helper(input: &str, expected_instruction_length: usize) -> Vec<Instruction> {
    let mut diagnostic_bag = DiagnosticBag::new();
    let result = convert(input, &mut diagnostic_bag, DebugFlags::default());
    assert_eq!(result.len(), expected_instruction_length);
    assert!(!diagnostic_bag.has_errors());
    result
}