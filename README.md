# docbuilder


This used to be a small build script that would compile some written notes and from a directory to html.

It sort of grew bigger as I wanted more features and automation, as a result this came into existence.

It is now a ~200 loc Haskell program featuring on demand markdown and ascii source file discovery and compilation (uses the pandoc library and asciidoctor executable), file system watching and subsequent automatic recompile as well as a server serving the compiled files and handling additional assets like js and css.

## Requirements

Markdown compilation relies on the [pandoc][] library and is directly linked into the program, thus requires no external resources.

Asciidoc compilation relies on [asciidoctor][] being installed on your system in your `$PATH`.


## Usage


    $ docbuilder --help
    Compile helper for asciidoc and markdown

    docbuilder [OPTIONS] [COMMANDS]

    Common flags:
      -s --sourcefolders=DIR         Root folders for the source files
      -p --port=INT                  Run the server on this port
      -t --pandoctemplate=FILE.html  A html template for the mardown compiler
      -i --indextemplate=FILE.html   Mustache template for the index page
      -? --help                      Display help message
      -V --version                   Print version information
