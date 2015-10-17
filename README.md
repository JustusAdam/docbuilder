# docbuilder


This used to be a small build script that would compile some written notes and from a directory to html.

It sort of grew bigger as I wanted more features and automation, as a result this came into existence.

It is now a ~200 loc Haskell program featuring on demand markdown and ascii source file discovery and compilation (uses the pandoc library and asciidoctor executable), file system watching and subsequent automatic recompile as well as a server serving the compiled files and handling additional assets like js and css.

## Requirements

Markdown compilation relies on the [pandoc][] library and is directly linked into the program, thus requires no external resources.

Asciidoc compilation relies on [asciidoctor][] being installed on your system in your `$PATH`.


## Usage


    $ docbuilder --help
    The docbuilderopts program

    docbuilderopts [OPTIONS] [ITEM]

    Common flags:
      -s --sourcefolders=DIRECTORY
      -p --port=INT                
      -t --pandoctemplate=FILE.html
      -i --indextemplate=FILE.html
      -? --help                      Display help message
      -V --version                   Print version information
