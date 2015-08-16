# Website

This is the source for my personal website, written using [Hakyll](http://jaspervdj.be/hakyll/examples.html), a wonderful static site compiler in Haskell.

All of the relevant Haskell code is in `hakyll.hs`---right now, there's not too much of it. In the near future, I am probably going to add some features for things like more intelligent includes and a blogging.

Right now, the only interesting customization is the include system. It lets me include the text of any files in an `include` directory just by using their base file name as a variable. For example, I could include `include/life.hs` as `$life$`. I've been using this for things like long code samples, because it's much easier to edit code in an appropriate file than inline with markdown.

However, I'm not entirely satisfied with the way the system works right now, so I'll probably be rewriting it at some point. Maybe after I get my blog set up.