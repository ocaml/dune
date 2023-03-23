from docutils.parsers.rst import directives
from sphinx import addnodes
from sphinx.domains import Domain, Index
from sphinx.directives import ObjectDescription
from sphinx.roles import XRefRole
from collections import defaultdict
from sphinx.util.nodes import make_refnode


class StanzaIndex(Index):
    name = "stanzaindex"
    localname = "Stanzas"

    def generate(self, docnames=None):
        content = defaultdict(list)

        stanzas = {
            name: (dispname, typ, docname, anchor, prio)
            for (
                name,
                dispname,
                typ,
                docname,
                anchor,
                prio,
            ) in self.domain.get_objects()
        }

        for dispname, typ, docname, anchor, _priority in stanzas.values():
            content[dispname[0].lower()].append(
                (dispname, 0, docname, anchor, docname, "", typ)
            )

        content = sorted(content.items())
        return content, True


class FieldIndex(Index):
    name = "fieldindex"
    localname = "Fields"

    def generate(self, docnames=None):
        content = defaultdict(list)

        fields = {
            name: (dispname, typ, docname, anchor)
            for (name, dispname, typ, docname, anchor) in self.domain.data["fields"]
        }

        for (
            dispname,
            typ,
            docname,
            anchor,
        ) in fields.values():
            content[dispname[0].lower()].append(
                (dispname, 0, docname, anchor, docname, "", typ)
            )

        content = sorted(content.items())
        return content, True


class StanzaDirective(ObjectDescription):
    """
    The stanza directive.

    It accepts a parameter named :param: which will be used as a placeholder.
    For example:

    .. dune:stanza:: name
       :param: <name>

    gets expanded to (name <name>), but the following:

    .. dune:stanza:: env

    gets expanded to (env ...).
    """

    option_spec = {
        "param": directives.unchanged_required,
    }

    required_arguments = 1

    def handle_signature(self, sig, signode):
        param = self.options.get("param", "...")
        text = f"({sig} {param})"
        signode += addnodes.desc_name(text=text)
        return sig

    def before_content(self, *args):
        self.env.current_path.append(self.arguments[0])

    def after_content(self, *args):
        self.env.current_path.pop()

    def add_target_and_index(self, name_cls, sig, signode):
        signode["ids"].append("stanza" + "-" + sig)
        domain = self.env.get_domain("dune")
        domain.add_stanza(sig)


class FieldDirective(ObjectDescription):
    """
    The field directive.

    A field is part of a stanza, or of another field. This nesting is tracked
    by `env.current_path`.
    """

    option_spec = {
        "param": directives.unchanged_required,
    }

    def handle_signature(self, sig, signode):
        param = self.options.get("param", "...")
        text = f"({sig} {param})"
        signode += addnodes.desc_name(text=text)
        return sig

    def before_content(self, *args):
        self.env.current_path.append(self.arguments[0])

    def after_content(self, *args):
        self.env.current_path.pop()

    def add_target_and_index(self, name_cls, sig, signode):
        path = self.env.current_path
        path_string = "-".join(path)
        signode["ids"].append(f"field-{path_string}-{sig}")
        domain = self.env.get_domain("dune")
        domain.add_field(path, sig)


class DuneDomain(Domain):
    name = "dune"

    directives = {
        "stanza": StanzaDirective,
        "field": FieldDirective,
    }
    roles = {"ref": XRefRole()}
    indices = {StanzaIndex, FieldIndex}
    initial_data = {"stanzas": [], "fields": []}

    def get_full_qualified_name(self, node):
        return f"stanza.{node.arguments[0]}"

    def get_objects(self):
        return self.data["stanzas"]

    def add_stanza(self, signature):
        name = f"stanza.{signature}"
        anchor = f"stanza-{signature}"
        self.data["stanzas"].append(
            (name, signature, "Stanza", self.env.docname, anchor, 0)
        )

    def add_field(self, path, field):
        path_string = "-".join(path)
        name = f"field.{path_string}.{field}"
        anchor = f"field-{path_string}-{field}"
        rpath = path[::-1]
        pretty_path = f"({rpath[0]})"
        for s in rpath[1:]:
            pretty_path = f"({s} {pretty_path})"
        typ = f"Field in {pretty_path}"
        self.data["fields"].append((name, field, typ, self.env.docname, anchor))

    def setup(self):
        super().setup()
        self.env.current_path = []


def setup(app):
    app.add_domain(DuneDomain)
    return {
        "version": "0.1",
    }
