use crate::Punct;
use crate::parse;

use super::scan;

#[test]
fn multi_part_punct() {
    parse::<Punct!["++", "-", "$"]>(scan("++-$")).unwrap();
}
