use mithra::data;
use mithra::data::MithraVal;
use mithra::data::MithraVal::Atomic;
use mithra::data::MithraVal::Bool;
use mithra::data::MithraVal::Dict;
use mithra::data::MithraVal::Float;
use mithra::data::MithraVal::Function;
use mithra::data::MithraVal::Int;
use mithra::data::MithraVal::List;
use mithra::data::MithraVal::Null;
use mithra::data::MithraVal::String;
use mithra::data::Text;
use mithra::parser::mithra_parsers;
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{self, BufRead};

fn expected_mithra_program() -> Vec<MithraVal> {
    [
        Null,
        Function(data::Function {
            name: "primeFactors".to_string(),
            params: ["n".to_string(), "x".to_string()].to_vec(),
            body: [
                List([Atomic("x".to_string()), Atomic("=".to_string()), Null].to_vec()),
                List([Atomic("y".to_string()), Atomic("=".to_string()), Bool(false)].to_vec()),
                List([
                    Atomic("xs".to_string()),
                    Atomic("=".to_string()),
                    Dict(BTreeMap::from([
                        ("1".to_string(), String("1".to_string())),
                        ("2".to_string(), List([Int(1), Int(-2), Int(3), Int(4)].to_vec())),
                        ("5".to_string(), Float(4.56)),
                        ("6".to_string(), Dict(BTreeMap::from([
                            ("1".to_string(), List([Int(1), Int(2)].to_vec())),
                            ("2".to_string(), Null),
                        ]))),
                    ]))
                ].to_vec()),
                List([
                    Atomic("if".to_string()),
                    List([
                        Atomic("eqv".to_string()),
                        List([Atomic("n".to_string()), Int(1)].to_vec())
                    ].to_vec()),
                    List([
                        List([String("return".to_string()), List([].to_vec())].to_vec())
                    ].to_vec())
                ].to_vec()),
                List([
                    Atomic("facs".to_string()),
                    Atomic("=".to_string()),
                    List([Atomic("factors".to_string()), List([Atomic("n".to_string())].to_vec())].to_vec())
                ].to_vec()),
                List([
                    Atomic("if".to_string()),
                    List([
                        Atomic("eqv".to_string()),
                        List([
                            List([Atomic("length".to_string()), List([Atomic("facs".to_string())].to_vec())].to_vec()),
                            Int(0)
                        ].to_vec())
                    ].to_vec()),
                    List([
                        List([String("return".to_string()), List([Atomic("n".to_string())].to_vec())].to_vec())
                    ].to_vec()),
                    Atomic("else".to_string()),
                    List([
                        List([
                            String("return".to_string()),
                            List([
                                Atomic("concat".to_string()),
                                List([
                                    Atomic("facs".to_string()),
                                    List([Atomic("primeFactors".to_string()), List([
                                        List([
                                            Atomic("div".to_string()),
                                            List([
                                                Atomic("n".to_string()),
                                                List([
                                                    Atomic("head".to_string()), 
                                                    List([
                                                        Atomic("facs".to_string())
                                                    ].to_vec())
                                                ].to_vec())
                                            ].to_vec())
                                        ].to_vec())
                                    ].to_vec())].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()),
                        Null,
                        Null
                    ].to_vec())
                ].to_vec())
            ].to_vec()
        }),
        Function(data::Function {
            name: "factors".to_string(),
            params: ["n".to_string()].to_vec(),
            body: [
                Function(data::Function {
                    name: "filterFunc".to_string(),
                    params: ["x".to_string()].to_vec(),
                    body: [
                        List([
                            Atomic("if".to_string()), 
                            List([
                                Atomic("kaka".to_string()), 
                                List([
                                    Int(8),
                                    Int(9),
                                    List([
                                        Int(1),
                                        String("buzi".to_string()),
                                        Int(3)
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec()),
                            List([
                                List([
                                    String("return".to_string()), 
                                    List([
                                        Atomic("eqv".to_string()), 
                                        List([
                                            List([
                                                Atomic("mod".to_string()), 
                                                List([
                                                    Atomic("n".to_string()), 
                                                    Atomic("x".to_string())
                                                ].to_vec())
                                            ].to_vec()), 
                                            Int(0)
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec()
                }),
                List([
                    String("return".to_string()), 
                    List([
                        Atomic("take".to_string()), 
                        List([
                            Int(1), 
                            List([
                                Atomic("filter".to_string()), 
                                List([
                                    Atomic("filterFunc".to_string()), 
                                    List([
                                        Atomic("generateList".to_string()), 
                                        List([
                                            Int(2), 
                                            List([
                                                Atomic("minus".to_string()), 
                                                List([
                                                    Atomic("n".to_string()), 
                                                    Int(1)
                                                ].to_vec())
                                            ].to_vec())
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()), 
                Null
            ].to_vec()
        }),
        Function(data::Function {
            name: "primeFactors".to_string(),
            params: ["n".to_string()].to_vec(),
            body: [
                List([
                    Atomic("if".to_string()), 
                    List([
                        Atomic("eqv".to_string()), 
                        List([
                            Atomic("n".to_string()), 
                            Int(1)
                        ].to_vec())
                    ].to_vec()), 
                    List([
                        List([
                            String("return".to_string()), 
                            Dict(BTreeMap::from([]))
                        ].to_vec())
                    ].to_vec()), 
                    Atomic("else".to_string()), 
                    List([
                        List([
                            Atomic("facs".to_string()), 
                            Atomic("=".to_string()), 
                            List([
                                Atomic("factors".to_string()), 
                                List([
                                    Atomic("n".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()), 
                        List([
                            Atomic("if".to_string()), 
                            List([
                                Atomic("eqv".to_string()), 
                                List([
                                    List([
                                        Atomic("length".to_string()), 
                                        List([
                                            Atomic("facs".to_string())
                                        ].to_vec())
                                    ].to_vec()), 
                                    Int(0)
                                ].to_vec())
                            ].to_vec()), 
                            List([
                                List([
                                    String("return".to_string()), 
                                    List([
                                        Atomic("n".to_string())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec()), 
                            Atomic("else".to_string()), 
                            List([
                                List([
                                    String("return".to_string()), 
                                    List([
                                        Atomic("concat".to_string()), 
                                        List([
                                            Atomic("facs".to_string()), 
                                            List([
                                                Atomic("primeFactors".to_string()), 
                                                List([
                                                    List([
                                                        Atomic("div".to_string()), 
                                                        List([
                                                            Atomic("n".to_string()), 
                                                            List([
                                                                Atomic("head".to_string()), 
                                                                List([
                                                                    Atomic("facs".to_string())
                                                                ].to_vec())
                                                            ].to_vec())
                                                        ].to_vec())
                                                    ].to_vec())
                                                ].to_vec())
                                            ].to_vec())
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec()), 
                                Null
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec())
            ].to_vec()
        }),
        Function(data::Function {
            name: "isPrime".to_string(),
            params: ["x".to_string()].to_vec(),
            body: [
                List([
                    String("return".to_string()),
                    List([
                        Atomic("and".to_string()),
                        List([
                            List([
                                Atomic("gte".to_string()),
                                List([
                                    Int(1),
                                    List([
                                        Atomic("length".to_string()),
                                        List([
                                            List([
                                                Atomic("primeFactors".to_string()),
                                                List([Atomic("x".to_string())].to_vec())
                                            ].to_vec())
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec()),
                            List([
                                Atomic("ne".to_string()),
                                List([Atomic("x".to_string()), Int(1)].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()),
                Null
            ].to_vec()
        }),
        Function(data::Function {
            name: "digits".to_string(),
            params: ["x".to_string()].to_vec(),
            body: [
                Function(data::Function {
                    name: "convertToNumber".to_string(),
                    params: ["x".to_string()].to_vec(),
                    body: [
                        List([
                            String("return".to_string()), 
                            List([
                                Atomic("toNumber".to_string()), 
                                List([
                                    Atomic("x".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec()
                }),
                List([
                    String("return".to_string()), 
                    List([
                        Atomic("map".to_string()), 
                        List([
                            Atomic("convertToNumber".to_string()), 
                            List([
                                Atomic("split".to_string()), 
                                List([
                                    List([
                                        Atomic("toString".to_string()), 
                                        List([
                                            Atomic("x".to_string())
                                        ].to_vec())
                                    ].to_vec()), 
                                    String("".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()), 
                Null
            ].to_vec()
        }),
        Function(data::Function {
            name: "allDigits".to_string(),
            params: ["xs".to_string()].to_vec(),
            body: [
                List([
                    Atomic("if".to_string()), 
                    List([
                        Atomic("eqv".to_string()), 
                        List([
                            List([
                                Atomic("length".to_string()), 
                                List([
                                    Atomic("xs".to_string())
                                ].to_vec())
                            ].to_vec()), 
                            Atomic("x".to_string())
                        ].to_vec())
                    ].to_vec()), 
                    List([
                        List([
                            String("return".to_string()), 
                            List([].to_vec())
                        ].to_vec())
                    ].to_vec()), 
                    Atomic("else".to_string()), 
                    List([
                        List([
                            Atomic("xsHead".to_string()), 
                            Atomic("=".to_string()), 
                            List([
                                Atomic("head".to_string()), 
                                List([
                                    Atomic("xs".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()), 
                        List([
                            Atomic("xsTail".to_string()), 
                            Atomic("=".to_string()), 
                            List([
                                Atomic("tail".to_string()), 
                                List([
                                    Atomic("xs".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()), 
                        List([
                            String("return".to_string()), 
                            List([
                                Atomic("concat".to_string()), 
                                List([
                                    List([
                                        Atomic("digits".to_string()), 
                                        List([
                                            Atomic("xsHead".to_string())
                                        ].to_vec())
                                    ].to_vec()), 
                                    List([
                                        Atomic("allDigits".to_string()), 
                                        List([
                                            Atomic("xsTail".to_string())
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()), 
                        Null
                    ].to_vec())
                ].to_vec())
            ].to_vec()
        }),
        Function(data::Function {
            name: "isSmith".to_string(),
            params: ["x".to_string()].to_vec(),
            body: [
                List([
                    Atomic("isNPrime".to_string()),
                    Atomic("=".to_string()),
                    List([
                        Atomic("isPrime".to_string()),
                        List([Atomic("x".to_string())].to_vec())
                    ].to_vec())
                ].to_vec()),
                List([
                    Atomic("digitSum".to_string()),
                    Atomic("=".to_string()),
                    List([
                        Atomic("sum".to_string()),
                        List([
                            List([
                                Atomic("digits".to_string()),
                                List([Atomic("x".to_string())].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()),
                List([
                    Atomic("nFactorDigitSum".to_string()),
                    Atomic("=".to_string()),
                    List([
                        Atomic("sum".to_string()),
                        List([
                            List([
                                Atomic("allDigits".to_string()),
                                List([
                                    List([
                                        Atomic("primeFactors".to_string()),
                                        List([Atomic("x".to_string())].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()),
                List([
                    String("return".to_string()),
                    List([
                        Atomic("and".to_string()),
                        List([
                            List([
                                Atomic("not".to_string()),
                                List([Atomic("isNPrime".to_string())].to_vec())
                            ].to_vec()),
                            List([
                                Atomic("eqv".to_string()),
                                List([
                                    Atomic("digitSum".to_string()),
                                    Atomic("nFactorDigitSum".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec()),
                Null
            ].to_vec()
        }),
        Function(data::Function {
            name: "allSmithsBelow".to_string(),
            params: ["x".to_string()].to_vec(), 
            body: [
                List([
                    Atomic("if".to_string()), 
                    List([
                        Atomic("lte".to_string()), 
                        List([
                            Atomic("x".to_string()), 
                            Int(0)
                        ].to_vec())
                    ].to_vec()), 
                    List([
                        List([
                            String("return".to_string()), 
                            Dict(BTreeMap::from([]))
                        ].to_vec())
                    ].to_vec()), 
                    Atomic("else".to_string()), 
                    List([
                        List([
                            String("return".to_string()), 
                            List([
                                Atomic("filter".to_string()), 
                                List([
                                    Atomic("isSmith".to_string()), 
                                    List([
                                        Atomic("generateList".to_string()), 
                                        List([
                                            Int(0), 
                                            Atomic("x".to_string())
                                        ].to_vec())
                                    ].to_vec())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec()), 
                        Null
                    ].to_vec())
                ].to_vec())
            ].to_vec()
        }),
        List([
            Atomic("x".to_string()), 
            Atomic("=".to_string()), 
            Int(22)
        ].to_vec()),
        Null,
        List([
            Atomic("facts".to_string()), 
            Atomic("=".to_string()), 
            List([
                Atomic("primeFactors".to_string()), 
                List([
                    Atomic("x".to_string())
                ].to_vec())
            ].to_vec())
        ].to_vec()),
        List([
            Atomic("isNPrime".to_string()), 
            Atomic("=".to_string()), 
            List([
                Atomic("isPrime".to_string()), 
                List([
                    Atomic("x".to_string())
                ].to_vec())
            ].to_vec())
        ].to_vec()),
        List([
            Atomic("digitSum".to_string()), 
            Atomic("=".to_string()), 
            List([
                Atomic("sum".to_string()), 
                List([
                    List([
                        Atomic("digits".to_string()), 
                        List([
                            Atomic("x".to_string())
                        ].to_vec())
                    ].to_vec())
                ].to_vec())
            ].to_vec())
        ].to_vec()),
        List([
            Atomic("nFactorDigitSum".to_string()), 
            Atomic("=".to_string()), 
            List([
                Atomic("sum".to_string()), 
                List([
                    List([
                        Atomic("allDigits".to_string()), 
                        List([
                            List([
                                Atomic("primeFactors".to_string()), 
                                List([
                                    Atomic("x".to_string())
                                ].to_vec())
                            ].to_vec())
                        ].to_vec())
                    ].to_vec())
                ].to_vec())
            ].to_vec())
        ].to_vec()),
        Null,
        List([
            Atomic("smiths".to_string()), 
            Atomic("=".to_string()), 
            List([
                Atomic("allSmithsBelow".to_string()), 
                List([
                    Int(100)].to_vec())
                ].to_vec())
            ].to_vec()),
        Null
    ].to_vec()
}

#[test]
pub fn test_parse_mithra_program() {
    fn actual_mithra_program() -> Vec<MithraVal> {
        let file_path = format!("tests/test_programs/valid_program.mth");
        let file = File::open(file_path).unwrap();
        let mut chars: Vec<char> = Vec::new();
        for line in io::BufReader::new(file).lines() {
            for c in line.unwrap().chars() {
                chars.push(c);
            }
            chars.push('\n');
        }
        let mut text = Text::new(chars);
        mithra_parsers::parse_inline_exprs(0)(&mut text).unwrap()
    }
    assert_eq!(
        expected_mithra_program(),
        actual_mithra_program()
    )
}
