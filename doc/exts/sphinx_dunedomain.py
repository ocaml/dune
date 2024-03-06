from docutils.parsers.rst import directives
from docutils.nodes import literal, Text
from sphinx import addnodes
from sphinx.domains import Domain, Index, ObjType
from sphinx.directives import ObjectDescription
from collections import defaultdict
from sphinx.util.nodes import make_refnode


class StanzaIndex(Index):
    name = "stanzaindex"
    localname = "Stanzas"

    def generate(self, docnames=None):
        content = defaultdict(list)

        stanzas = [
            (dispname, typ, docname, anchor)
            for (
                name,
                dispname,
                typ,
                docname,
                anchor,
                prio,
            ) in self.domain.get_stanzas()
        ]

        for dispname, typ, docname, anchor in stanzas:
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


class DuneDomain(Domain):
    name = "dune"

    directives = {
        "stanza": StanzaDirective,
    }
    indices = {StanzaIndex}
    initial_data = {"stanzas": []}

    def get_full_qualified_name(self, node):
        return f"stanza.{node.arguments[0]}"

    def get_stanzas(self):
        return self.data["stanzas"]

    def get_objects(self):
        return self.get_stanzas()

    def add_stanza(self, signature):
        name = f"stanza.{signature}"
        anchor = f"stanza-{signature}"
        self.data["stanzas"].append(
            (name, signature, "Stanza", self.env.docname, anchor, 0)
        )

    def setup(self):
        super().setup()
        self.env.current_path = []

    def find_object(self, typ, name):
        objects = self.get_objects()
        matches = [
            (docname, anchor)
            for _, obj_name, obj_typ, docname, anchor, _ in objects
            if obj_name == name and obj_typ == typ
        ]
        assert matches, f"dune domain: found no {typ} named {name}"
        assert len(matches) == 1, f"dune domain: found several {typ} named {name}"
        return matches[0]


def setup(app):
    app.add_domain(DuneDomain)
    return {
        "version": "0.1",
    }
