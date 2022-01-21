This test generates documentation using odoc for a library:

  $ dune build @doc

This test if `.odocl` files are generated
  $ find _build/default/_doc/_odocls -name '*.odocl' | sort -n
  _build/default/_doc/_odocls/bar/bar.odocl
  _build/default/_doc/_odocls/bar/page-index.odocl
  _build/default/_doc/_odocls/foo/foo.odocl
  _build/default/_doc/_odocls/foo/foo2.odocl
  _build/default/_doc/_odocls/foo/foo_byte.odocl
  _build/default/_doc/_odocls/foo/page-index.odocl

  $ dune runtest
  HTML:
  <!DOCTYPE html>
  <html xmlns="http://www.w3.org/1999/xhtml">
    <head>
      <title>index</title>
      <link rel="stylesheet" href="./odoc.css"/>
      <meta charset="utf-8"/>
      <meta name="viewport" content="width=device-width,initial-scale=1.0"/>
    </head>
    <body>
      <main class="content">
        <div class="by-name">
        <h2>OCaml package documentation</h2>
        <ol>
        <li><a href="bar/index.html">bar</a></li>
        <li><a href="foo/index.html">foo</a></li>
        </ol>
        </div>
      </main>
    </body>
  </html>
  
  LATEX:
  \documentclass[11pt,a4paper,twoside]{article}
  \usepackage[left=25mm,right=25mm,top=25mm,bottom=25mm,marginparwidth=50pt]{geometry}
  \usepackage[utf8]{inputenc}
  \usepackage[colorlinks=true,breaklinks=true]{hyperref}
  \usepackage{color}
  \usepackage{changepage}
  \usepackage{listings}
  
  \newcommand{\ocamlcodefragment}[1]{{\ttfamily\setlength{\parindent}{0cm}%
  \raggedright#1}}
  \newcommand{\ocamlinlinecode}[1]{{\ttfamily#1}}
  \newcommand{\bold}[1]{{\bfseries#1}}
  \newenvironment{ocamlexception}{\bfseries}{}
  \newenvironment{ocamlextension}{\bfseries}{}
  \newenvironment{ocamlarrow}{}
  
  \newcommand{\ocamltag}[2]{\begin{ocaml#1}#2\end{ocaml#1}}
  \newenvironment{ocamlkeyword}{\bfseries}{}
  \newenvironment{ocamlconstructor}{\bfseries}{}
  \newenvironment{ocamltype-var}{\itshape\ttfamily}{}
  
  \newcommand{\ocamlhighlight}{\bfseries\uline}
  \newcommand{\ocamlerror}{\bfseries}
  \newcommand{\ocamlwarning}{\bfseries}
  
  \definecolor{lightgray}{gray}{0.97}
  \definecolor{gray}{gray}{0.5}
  \newcommand{\ocamlcomment}{\color{gray}\normalfont\small}
  \newcommand{\ocamlstring}{\color{gray}\bfseries}
  \newenvironment{ocamlindent}{\begin{adjustwidth}{2em}{0pt}}{\end{adjustwidth}}
  \newenvironment{ocamltabular}[2][l]{\begin{tabular}{#2}}%
  {\end{tabular}}
  
  \lstnewenvironment{ocamlcodeblock}{
    \lstset{
      backgroundcolor = \color{lightgray},
      basicstyle=\ttfamily,
      showstringspaces=false,
      language=caml,
      escapeinside={$}{$},
      columns=fullflexible,
      stringstyle=\ocamlstring,
      commentstyle=\ocamlcomment,
      keepspaces=true,
      keywordstyle=\ocamlkeyword,
      moredelim=[is][\ocamlhighlight]{<<}{>>},
      moredelim=[s][\ocamlstring]{\{|}{|\}},
      moredelim=[s][\ocamlstring]{\{delimiter|}{|delimiter\}},
      keywords={[2]{val,initializer,nonrec}}, keywordstyle={[2]\ocamlkeyword},
      belowskip=0\baselineskip,
      upquote=true,
      literate={'"'}{\textquotesingle "\textquotesingle}3
      {'\\"'}{\textquotesingle \textbackslash"\textquotesingle}4,
    }
  }{}
  
  \title{Documentation of Package}
  \date{}
  \author{}
  \begin{document}
  \maketitle
  
  \tableofcontents
  
  \input{bar/index.tex}
  \input{bar/Bar.tex}
  
  \input{foo/index.tex}
  \input{foo/Foo.tex}
  
  \end{document}
  
  

  $ dune build @foo-mld
  {0 foo index}
  {1 Library foo}
  This library exposes the following toplevel modules:
  {!modules:Foo Foo2}
  {1 Library foo.byte}
  The entry point of this library is the module:
  {!module-Foo_byte}.

  $ dune build @bar-mld
  {0 bar index}
  {1 Library bar}
  The entry point of this library is the module:
  {!module-Bar}.
