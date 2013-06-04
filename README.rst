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


A skeleton project with a simple theme based on Twitter's Bootstrap framework is located in the ``example_blog`` folder in this project. The compiled binary can be used thusly::

    ?> qfwfq /path/to/my_blog

The blog will be output into the ``publish`` folder (as shown above). From there it's a simple step to rsync the new blog contents up to your server::

    ?> rsync -avh /path/to/my_blog/publish/ me@myserver.com:/var/www

Compiling
---------

The following should be sufficient to compile the ``qfwfq`` binary::

    ghc -o qfwfq -XTemplateHaskell -XNoMonomorphismRestriction main.hs