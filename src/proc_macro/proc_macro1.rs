use crate::TokenStream;

extern crate proc_macro;

use proc_macro2::TokenStream as TokenStream2;

impl From<proc_macro::TokenStream> for TokenStream {
    fn from(value: proc_macro::TokenStream) -> Self {
        TokenStream2::from(value).into()
    }
}
