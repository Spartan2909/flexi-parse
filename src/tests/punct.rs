use crate::parse;
use crate::Punct;

use super::scan;

#[test]
fn multi_part_punct() {
    parse::<Punct!["++", "-", "$"]>(scan("++-$")).unwrap();
}
