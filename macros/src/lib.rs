use proc_macro::TokenStream;
use proc_macro2::Literal;
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input, Ident, ImplItem, ItemImpl, Result, Token, Type,
};

#[proc_macro_attribute]
pub fn load_module(args: TokenStream, input: TokenStream) -> TokenStream {
    let name = match syn::parse::<ModuleLoadArgs>(args) {
        Ok(args) => format!("{}-", args.name.to_string()),
        Err(e) => String::new(),
    };

    let struct_impl = parse_macro_input!(input as ItemImpl);

    let Type::Path(path) = &*struct_impl.self_ty else {
        panic!("The load_module macro only accepts struct self-impls!");
    };
    let things = struct_impl.items;
    let fn_names = things
        .clone()
        .into_iter()
        .map(|x| {
            let ImplItem::Fn(fun) = x else {
                panic!("The load_module macro only accepts functions.");
            };

            fun.sig.ident
        })
        .collect::<Vec<Ident>>();

    let fn_names_as_strings = fn_names
        .iter()
        .map(|x| format!("{name}{x}"))
        .collect::<Vec<String>>();

    let quote = quote::quote! {
        impl #path {
        #(#things)*
        }

        impl Module for #path {
            fn load(self, symbols: &mut SymbolTable) {
                #(
                    symbols.register_fn(
                        #fn_names_as_strings.into(),
                        Box::new(Expr::RustFn(Box::new(Self::#fn_names)))
                    );
                )*
            }
        }
    };

    TokenStream::from(quote)
}

struct ModuleLoadArgs {
    ident: Ident,
    equals: Token![=],
    name: Literal,
}

impl Parse for ModuleLoadArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        Ok(Self {
            ident: input.parse()?,
            equals: input.parse()?,
            name: input.parse()?,
        })
    }
}
