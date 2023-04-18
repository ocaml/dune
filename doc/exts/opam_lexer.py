from pygments.lexer import RegexLexer
from pygments.token import (
    Keyword,
    Number,
    Punctuation,
    String,
    Text,
    Whitespace,
)


ident = r"[-\w]+"


class OpamLexer(RegexLexer):
    name = "opam"
    tokens = {
        "root": [
            (r"\d\.\d", Number),
            (r"^" + ident + ":", Keyword),
            (ident, Text),
            (r"[\[\]{}>=]", Punctuation),
            (r"\s+", Whitespace),
            (r'"[^\"]*"', String),
        ]
    }
