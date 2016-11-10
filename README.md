# realworldhaskell-util

utilties for the unofficial "Real World Haskell" site

* book content and comments crawler
* html-to-markdown converter (more customized than pandoc)

## See Also

* [Real World Haskell repository](https://github.com/maczniak/realworldhaskell)
* [Real World Haskell sample code](https://github.com/maczniak/realworldhaskell-src)

## TODO

* crawler
 * crawler
 * relativize URLs
 * handle bookstore links
 * parallel fetch
 * use either ByteString or Text
* converter
 * generate with-comment and without-comment versions
 * handle "Tip"
 * handle icons
 * handle RSS links
 
## ChangeLog

* 2016-11-03 - replace taggy with tagsoup for preserving newlines in `pre` block

