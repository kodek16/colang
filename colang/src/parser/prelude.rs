//! Prelude for various parser modules.

pub use crate::parser::base::CharsParser;
pub use crate::parser::base::Ignored;
pub use crate::parser::base::WithIgnored;
pub use crate::parser::base::WordParser;
pub use crate::parser::chars::*;
pub use crate::parser::common::ParseResult;
pub use crate::parser::common::ParsedNode;
pub use crate::parser::common::Parser;
pub use crate::parser::common::SyntaxError;
pub use crate::parser::context::ParsingContext;
pub use crate::parser::input::Input;
pub use crate::parser::repeat::DontRecover;
pub use crate::parser::repeat::RecoveryConsumer;
pub use crate::parser::repeat::RepeatZeroOrMore;
pub use crate::parser::seq::AbortIfMissing;
pub use crate::parser::seq::OnMissingStrategy;
pub use crate::parser::seq::OnMissingStrategyOutput;
pub use crate::parser::seq::Optional;
pub use crate::parser::seq::SynthesizeIfMissing;
pub use crate::parser::seq::{Seq2, Seq3, Seq4, Seq5, Seq6, Seq7};
pub use crate::parser::words::*;
pub use crate::source::InputSpan;
