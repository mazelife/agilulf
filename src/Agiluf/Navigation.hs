module Navigation where


import Data.List (unfoldr)

import Definition


-- | Split a list into length-n pieces, with last piece length < n
splitEvery :: Int -> [a] -> [[a]]
splitEvery n xs = (takeWhile (not . null) . unfoldr (Just . splitAt n)) xs


-- | Return a list of pages
paginate :: Int -> [a] -> [(IndexNavigation, [a], Int)]
paginate n xs = map gn pages
    where groups = splitEvery n xs
          ln = length groups
          pages = zip [1..ln] groups
          gn = getIndexNavigation ln


getIndexNavigation :: Int -> (Int, [a]) -> (IndexNavigation, [a], Int)
getIndexNavigation last_number (current, pages)
    | current ==  1 = (IndexNavigation current_href False "" True "page2.html", pages, current)
    | current == last_number = (IndexNavigation current_href True (pHref $ current - 1) False "", pages, current)
    | otherwise = (IndexNavigation current_href True (pHref $ current - 1) True (pHref $ current + 1), pages, current)
    where pHref n = if n == 1 then "index.html" else "page" ++ show n ++ ".html"
          current_href = pHref current


-- | Create a list Navigation items from a list of entries.
getEntryNavigation :: [Entry] -> [EntryNavigation]
getEntryNavigation [] = []
getEntryNavigation [aE, bE] = [firstNavigation aN, lastNavigation bN]
    where aN = (fileName aE, title aE)
          bN = (fileName bE, title bE)
getEntryNavigation entries = map makeNav indexed
    where slugs = map (\e -> (fileName e, title e)) entries
          l = length entries
          indexed = zip [1..l] slugs
          makeNav (index, _)
              | index == 1 = firstNavigation (slugs!!index) -- first page, needs a next
              | index == l = lastNavigation (slugs!!(index-2)) -- last page, needs a previous
              | otherwise = middleNavigation (slugs!!(index-2)) (slugs!!index)
              where


firstNavigation (slug, label) = EntryNavigation True slug label False "" ""
middleNavigation (slugR, labelR) (slugL, labelL) = EntryNavigation True slugL labelL True slugR labelR
lastNavigation (slug, label) = EntryNavigation False "" "" True slug label

