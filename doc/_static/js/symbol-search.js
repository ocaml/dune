(function () {
  "use strict";

  function documentationRoot() {
    var currentScript = document.currentScript;

    if (!currentScript || !currentScript.src) {
      for (var i = 0; i < document.scripts.length; i++) {
        var script = document.scripts[i];
        if (script.src && script.src.indexOf("/_static/js/symbol-search.js") !== -1) {
          currentScript = script;
          break;
        }
      }
    }

    if (!currentScript || !currentScript.src) {
      return null;
    }

    return new URL("../../", currentScript.src).href;
  }

  var root = documentationRoot();

  function anchorForQuery(query) {
    var q = query.trim();

    if (q === "") {
      return null;
    }

    if (q.indexOf("@@") === 0) {
      return "symbol-double-at";
    }

    if (q.indexOf("@") === 0) {
      return "symbol-at";
    }

    if (q.indexOf("%{") === 0) {
      return "symbol-percent-brace";
    }

    if (q.indexOf("(:include") === 0) {
      return "symbol-include";
    }

    switch (q) {
      case ":standard":
        return "symbol-colon-standard";
      case "\\":
        return "symbol-backslash";
      case "->":
        return "symbol-arrow";
      case "!":
        return "symbol-bang";
      case "*":
        return "symbol-star";
      case "**":
        return "symbol-star";
      case "?":
        return "symbol-question";
      default:
        return null;
    }
  }

  function redirectToSymbol(query) {
    var anchor = anchorForQuery(query);

    if (anchor === null || root === null) {
      return false;
    }

    window.location.assign(root + "reference/syntax-symbols.html#" + anchor);
    return true;
  }

  function maybeRedirectSearchPage() {
    if (!/\/search\.html$/.test(window.location.pathname)) {
      return;
    }

    var params = new URLSearchParams(window.location.search);
    var query = params.get("q") || params.get("query");

    if (query !== null) {
      redirectToSymbol(query);
    }
  }

  function connectSearchForms() {
    var forms = document.querySelectorAll("form[action]");

    for (var i = 0; i < forms.length; i++) {
      var form = forms[i];

      if (form.action.indexOf("search") === -1) {
        continue;
      }

      var input = form.querySelector(
        'input[name="q"], input[name="query"], input[type="search"]'
      );

      if (input === null) {
        continue;
      }

      form.addEventListener(
        "submit",
        (function (searchInput) {
          return function (event) {
            if (redirectToSymbol(searchInput.value)) {
              event.preventDefault();
            }
          };
        })(input)
      );
    }
  }

  maybeRedirectSearchPage();
  window.addEventListener("DOMContentLoaded", connectSearchForms);
})();
