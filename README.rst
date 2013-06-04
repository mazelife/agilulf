=======
Agiluf
=======

A basic static blogging engine, written in Haskell. It's meant to be bare-bones but supports:

* html templating
* tagging
* multiple authors
* RSS

It's built on top of the awesome `Pandoc <http://johnmacfarlane.net/pandoc/>`_ document converter. You just write your blog entries in `RST <http://docutils.sourceforge.net/docs/ref/rst/introduction.html>`_, add a little metadata to the top, and let Agiluf do the rest.


A typical project structure::

    my_blog/
        entries/
            lorem-ipsum.rst
            sit-amet.rst
            (etc.)
        templates/
            entry.html
            index.html
            tag.html
        media/
            a-blog-entry-image.jpg
            another-entry-media.svg
            (etc.)
        static/
            css/
                (user-determined)
            javascript/
                (user-determined)
            images/
                (user-determined)
        publish/
            index.html
            page2.html
            page3.html
            (etc.)
            entries/
                lorem-ipsum.html
                sit-amet.html
                (etc.)
            media/
                (copied from above)
            static/
                (copied from above)
            tags/
                my-first-tag.html
                another-tag.html
                (etc.)


A skeleton project with a simple theme based on Twitter's Bootstrap framework is located in the ``example_blog`` folder in this project. You can use this as a base for building your own blog. The compiled binary can be used thusly::

    ?> agiluf /path/to/my_blog

The blog will be output into the ``publish`` folder (as shown above). No fancy publishing workflows needed; from there it's a simple step to rsync the new blog contents up to your server::

    ?> rsync -avh /path/to/my_blog/publish/ me@myserver.com:/var/www

Compiling
---------

The following should be sufficient to compile the ``agiluf`` binary::

    ghc -o agiluf -XTemplateHaskell -XNoMonomorphismRestriction main.hs


Colophon
--------

Why the name? There were a `couple <http://en.wikipedia.org/wiki/Agilulf>`_ of `historical <http://en.wikipedia.org/wiki/Agilulf_(Bishop_of_Metz)>`_ figures who bore this sobriquet. However, I took the name from Italo Calvino's wonderful novel `The Nonexistent Knight <http://en.wikipedia.org/wiki/The_Nonexistent_Knight>`_. It's something of a nod to the challenges of learning Haskell.
