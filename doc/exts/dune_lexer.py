from pygments.token import Comment, Punctuation, Text, Name, String, Generic
from pygments.lexer import RegexLexer


def after_paren(r):
    return r"(?<=\()(%s)" % r


class DuneLexer(RegexLexer):
    name = "dune"

    atom = r'[^\s()"]+'

    tokens = {
        "root": [
            (r";.*$", Comment.Single),
            (r"(\(|\))", Punctuation),
            # pforms (%{var} and %{fun:arg}
            (r"%{[^}]+}", String.Backtick),
            (r":[-\w]+", String.Symbol),
            # placeholders like <arg> and ... that appear in docs
            (r"<[-\w ]+>", Generic.Emph),
            (r"\.\.\.", Generic.Emph),
            (r'"\\\|', String.Double, "string-multiline"),
            (r'"', String, "string"),
            # instead of hardcoding "builtin" names,
            # highlight the first atom in a list differently
            (after_paren(atom), Name.Function),
            (atom, Name),
            (r"\s+", Text),
        ],
        "string": [
            (r'(\\\\|\\"|[^"])*"', String, "#pop"),
        ],
        "string-multiline": [(r"[^\n]*\n", String, "#pop")],
    }
