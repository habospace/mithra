use mithra::data::MithraVal;
use mithra::data::Text;
use mithra::parser::mithra_parsers::parse_bool;

#[test]
fn test_parse_bool() {
    let chars = format!("False").chars().collect();
    let mut text = Text::new(chars);
    let result = parse_bool(&mut text).unwrap();
    assert_eq!(MithraVal::Bool(false), result);
}
