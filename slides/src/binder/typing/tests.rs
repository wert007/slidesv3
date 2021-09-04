use super::*;

#[test]
fn test_type_identifiers() {
    let t = Type::Integer;
    let tid = t.type_identifier();
    let ct = Type::from_type_identifier(tid).unwrap();
    assert_eq!(t, ct);
    
    let t = Type::Boolean;
    let tid = t.type_identifier();
    let ct = Type::from_type_identifier(tid).unwrap();
    assert_eq!(t, ct);
    
    let t = Type::String;
    let tid = t.type_identifier();
    let ct = Type::from_type_identifier(tid).unwrap();
    assert_eq!(t, ct);
    
    let t = Type::Any;
    let tid = t.type_identifier();
    let ct = Type::from_type_identifier(tid).unwrap();
    assert_eq!(t, ct);
}