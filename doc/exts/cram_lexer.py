from pygments.lexer import DelegatingLexer, RegexLexer, bygroups, default
from pygments.lexers.shell import BashLexer
from pygments.token import Generic, Other, Comment


class CramBaseLexer(RegexLexer):
    tokens = {
        "root": [
            (r"(  \$ )(.*\n)", bygroups(Generic.Prompt, Other.Code), "continuations"),
            (r"^  .*\n", Generic.Output),
            (r"^.*\n", Comment.Multiline),
        ],
        "continuations": [
            (r"(  > )(.*\n)", bygroups(Generic.Prompt, Other.Code)),
            default("#pop"),
        ],
    }


class CramLexer(DelegatingLexer):
    name = "cram"

    def __init__(self, **options):
        super().__init__(BashLexer, CramBaseLexer, Other.Code, **options)
