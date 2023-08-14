use crate::parse;
use crate::Punct;

use super::scan;

#[test]
fn multi_part() {
    parse::<Punct!["++", "-", "$"]>(scan("++-$")).unwrap();
}

#[test]
fn joint() {
    parse::<Punct!["<="]>(scan("<=")).unwrap();
}

#[test]
fn joint_fail() {
    parse::<Punct!["+="]>(scan("+ =")).unwrap_err();
}
