module Mock ( post_template
            , mock_posts
            ) where



post_template = unlines [
    "<h1>{{metadata.title}}</h1>",
    "<h2 class=\"dateline\">{{metadata.displayDate}}</h2>",
    "<nav id=\"pagination\"><ul>",
    "{{#navigation.previous}}<li id=\"previous\"><a href=\"{{navigation.previousHref}}\">{{navigation.previousLabel}}<a/></li>{{/navigation.previous}}",
    "{{#navigation.next}}<li id=\"next\"><a href=\"{{navigation.nextHref}}\">{{navigation.nextLabel}}<a/></li>{{/navigation.next}}",
    "</ul></nav>"
    ]


mock_posts = [entry1, entry2, entry3, entry4]

entry1 = unlines [
    "=====================================================",
    " The reStructuredText_ Cheat Sheet: Syntax Reminders",
    "=====================================================",
    ":Title: I am a title string",
    ":Author: David Goodger <goodger@python.org>",
    ":Date: 2012-06-22 19:49:51",
    ":Description: This is a \\docinfo block\\, or bibliographic field list",
    ":Tags: Haskell|Python|Test Tag",
    ":Slug: i-am-da-coolest",
    "",
    "Hello World!",
    "=================",
    "",
    "Eu case philosophia vis. At postea expetenda theophrastus ius. Agam ignota pri et."
    ]


entry2 = unlines [
    "=====================================================",
    " Tell me a story",
    "=====================================================",
    ":Title: All about things!",
    ":Author: David Goodger <goodger@python.org>",
    ":Date: 2013-03-01 07:10:51",
    ":Description: This is a \\docinfo block\\, or bibliographic field list",
    ":Tags: Haskell|Voodoo|Python",
    ":Slug: foo-bar-baz",
    "",
    "Hello Again!",
    "=================",
    "",
    "Dicat dignissim suscipiantur te mel, eum no veritus volumus repudiare."
    ]

entry3 = unlines [
    "=====================================================",
    " Let's take a trip to crazytown",
    "=====================================================",
    ":Title: Here we go again!",
    ":Author: David Goodger <goodger@python.org>",
    ":Date: 2013-04-12 04:11:51",
    ":Description: All about a town that's crzy",
    ":Tags: Python",
    ":Slug: crazeeeeeeee",
    "",
    "Hello Again!",
    "=================",
    "",
    "Dicat dignissim suscipiantur te mel, eum no veritus volumus repudiare."
    ]

entry4 = unlines [
    "=====================================================",
    " Reminder Syntack Sheet Cheat reStructuredText_ OK!",
    "=====================================================",
    ":Title: Mixed up crazytown!",
    ":Author: David Goodger <goodger@python.org>",
    ":Date: 2013-02-09 17:10:51",
    ":Description: Bloop blorp.",
    ":Tags: Bloop|Voodoo|Python",
    ":Slug: mixed-up-words",
    "",
    "Hello Again!",
    "=================",
    "",
    "Dicat dignissim suscipiantur te mel, eum no veritus volumus repudiare."
    ]