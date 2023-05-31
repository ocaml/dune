from docutils.parsers.rst import directives
from docutils.nodes import literal, Text
from sphinx import addnodes
from sphinx.domains import Domain, Index, ObjType
from sphinx.directives import ObjectDescription
from sphinx.roles import XRefRole
from collections import defaultdict
from sphinx.util.nodes import make_refnode


def set_xref_text(node, new_text):
    assert type(node) is literal, type(mode)
    (child,) = node.children
    assert type(child) is Text, type(child)
    node.children = [Text(new_text)]


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


class ActionDirective(ObjectDescription):
    """
    The action directive.
    """

    option_spec = {
        "param": directives.unchanged_required,
    }

    def handle_signature(self, sig, signode):
        param = self.options.get("param", "...")
        text = f"({sig} {param})"
        signode += addnodes.desc_name(text=text)
        return sig

    def add_target_and_index(self, name_cls, sig, signode):
        signode["ids"].append(f"action-{sig}")
        domain = self.env.get_domain("dune")
        domain.add_action(name_cls)


class DuneDomain(Domain):
    name = "dune"

    directives = {
        "stanza": StanzaDirective,
        "field": FieldDirective,
        "action": ActionDirective,
    }
    roles = {"ref": XRefRole()}
    indices = {StanzaIndex, FieldIndex}
    initial_data = {"stanzas": [], "fields": []}
    object_types = {"action": ObjType("action")}

    def get_full_qualified_name(self, node):
        return f"stanza.{node.arguments[0]}"

    def get_stanzas(self):
        return self.data["stanzas"]

    def get_actions(self):
        return [
            (f"action.{name}", name, "action", docname, f"action-{name}", 0)
            for name, docname in self.actions.items()
        ]

    def get_objects(self):
        return self.get_stanzas() + self.get_actions()

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

    def add_action(self, name):
        self.actions[name] = self.env.docname

    def setup(self):
        super().setup()
        self.actions = {}
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

    def resolve_xref(self, env, fromdocname, builder, typ, target, node, contnode):
        """
        Replace dune:ref:`action-x` by a link to where x is defined.
        The text of is changed so that it is (x) rather than action-x.
        """
        if not target.startswith("action-"):
            # target is not from this domain
            return None

        action_name = target.removeprefix("action-")
        todocname, targ = self.find_object("action", action_name)
        set_xref_text(contnode, f"({action_name})")
        return make_refnode(builder, fromdocname, todocname, targ, contnode, targ)


def setup(app):
    app.add_domain(DuneDomain)
    return {
        "version": "0.1",
    }
