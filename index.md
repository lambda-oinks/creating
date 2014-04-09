---
title: Creating Lambda Oinks: A Scientific Blogging Platform That Just Works
date: 2014-04-05
author: Oinkina
published: true
---

Mostly, I just wanted to get my hands dirty with a project. 

I mean, I had sorta-kinda-maybe been thinking about starting a blog for a while, but never had enough motivation to do so. In the end, it was my friend's incessant complaints about the poor support typical blogging platforms have for code with beautiful syntax highlighting and math[^WP] that got me to dive in. *I can do better than that*, I thought.

[^WP]: WordPress supports LaTeX syntax, but converts it into an image file.

The core of the blog setup is that posts are written in markdown and turned into static webpages by [Hakyll], and then hosted on [GitHub Pages]. I also have a [GitHub repo for the blog], another that's a submodule within it [for all the posts], and, within that, each post is its own submodule that lives in a [GitHub organization]. 

[Hakyll]: http://jaspervdj.be/hakyll/
[GitHub Pages]: https://pages.github.com/
[GitHub repo for the blog]: https://github.com/oinkina/Lambda-Oinks
[for all the posts]: https://github.com/oinkina/lambda-oinks-posts
[GitHub organization]: https://github.com/lambda-oinks

As a user, it is remarkably simple to write posts with the exact aesthetic I want. To start a new post, I run a little script ```./newpost.sh``` that sets up the git repo linked as a submodule with a template that has metadata, and add content to the markdown file:

```markdown
---
title: Creating Lambda Oinks: A Scientific Blogging Platform That Just Works
date: 2014-04-05
author: Oinkina
mathjax: on
---

Mostly, I just wanted to get my hands dirty with a project. 

...
```

I can see all my edits before publishing by keeping ```./site watch``` running and refreshing my browser pointed to localhost. When I'm done, I just run an update script: ```./update.sh```. That simple.

Hakyll
-------

Hakyll is like [Jekyll], but in Haskell (and therefore more elegant). I'll briefly run through how Hakyll works[^tutorials] before explaining how I adapted it. In essence, Hakyll runs Pandoc on markdown files to turn them into HTML, then puts that body content into HTML templates.[^diagram] To install and create an example site[^haskell]:

[^tutorials]: [Jasper Van der Jeugt], the creator of Hakyll, also has nice [tutorials].
[^diagram]: See [this blog post] for a nice diagram.
[^haskell]: You need [Haskell] for this.

[Jekyll]: http://jekyllrb.com/
[Jasper Van der Jeugt]: http://jaspervdj.be/
[tutorials]: http://jaspervdj.be/hakyll/tutorials.html
[this blog post]: http://yannesposito.com/Scratch/en/blog/Hakyll-setup/#the-concepts-and-syntax
[Haskell]: http://www.haskell.org/platform/

```bash
cabal install hakyll
hakyll-init [DIR NAME]
cd [DIR NAME]
```

To compile, build, and watch, so that we can simply refresh to see a preview:

```bash
ghc --make site.hs
./site rebuild
./site watch
```

### Rules

For my ```site.hs``` file[^github], I needed the following imports:

[^github]: You can find the latest version of my ```site.hs``` file [on GitHub](https://github.com/oinkina/Lambda-Oinks/blob/master/site.hs).


```haskell
import           Data.Monoid            ((<>))
import           Hakyll
import qualified Data.Map as M
import           Text.Pandoc.Options
import           Data.Maybe (fromMaybe, isJust)
import           Control.Monad (filterM)
```  

We start with rules that match different file types and then route to a destination path and compile with various templates and contexts. 

For static files, this is easy, as the route is ```idRoute``` to copy the file into ```_site/``` and there aren't templates or contexts applied. For my files, including Bootstrap and fonts, it looks like this, with ```.||.``` meaning "or" and ```/**``` meaning any children:

```haskell
-- Compress CSS
match ("css/*" 
        .||. "bootstrap/css/*" 
        .||. "highlight/styles/*"
        .||. "fonts/Serif/cmun-serif.css"
        .||. "fonts/Serif Slanted/cmun-serif-slanted.css") $ do
    route   idRoute
    compile compressCssCompiler

-- Static files
match ("js/*" 
        .||. "favicon.ico"
        .||. "bootstrap/js/*" 
        .||. "bootstrap/fonts/*" 
        .||. "images/*"
        .||. "images/highlight/*" 
        .||. "highlight/highlight.pack.js"
        .||. "fonts/Serif/*"
        .||. "fonts/Serif-Slanted/*"
        .||. "comments/*"
        .||. "js/MathBox.js/**"
        .||. "posts/**" .&&. (complement "posts/*/*.md")) $ do
    route idRoute
    compile copyFileCompiler
```

And compiling the templates is an easy one-liner baked into Hakyll:

```haskell
match "templates/*" $ compile templateCompiler
```

You can get a sense of how the project is organized from this. Other types of files have more complicated rules. Let's look at the rule for the about and contact pages, which is similar to that in the example Hakyll site:

```haskell
-- about and contact are in the pages directory
match "pages/*.md" $ do
    -- don't want the URL to include "/pages"
    route   $ gsubRoute "pages/" (const "") `composeRoutes`
              setExtension "html"
    -- compile
    compile $ pandocCompiler
                -- apply the default template with contexts
                >>= loadAndApplyTemplate "templates/default.html" 
                    (mathCtx <> defaultContext)
                >>= relativizeUrls
```

The rule for posts is rather similar. For every post, we use a submodule within the directory ```posts/``` and name the file ```index.md``` so that we don't need to specify a name within the post in the URL. Before applying the default template, we apply the post template with its context.

```haskell
match "posts/*/index.md"
    -- route only needs to set the HTML extension
    route $ setExtension ".html"
    compile $ pandocCompiler
            >>= saveSnapshot "content"
            -- post template and context
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> postCtx)
            >>= relativizeUrls
```

### Archive and Homepage: Using Partials

A good blog doesn't have its posts floating around; there should be an archive. We could make an archive page by hand, but that's unnecessarily annoying and error-prone. Instead, we have Hakyll automatically generate an archive page based on the posts in our ```/posts``` directory. 

We save a compiled list of posts that we've specified should be published in chronological order as the field ```$posts$```, which we can use in our templates. 

```haskell
create ["archive.html"] $ do
    route idRoute
    compile $ do
        -- load the posts that are published by date into "posts"
        posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*/index.md"
```

The post-list template takes ```$posts$```, which has our list of ordered published posts, and loops over it. Each field (```$url$```, ```$title$```, ```$date$```) refers to the field of the specific post at that iteration of the loop:

```html
<ul>
    $for(posts)$
        <li class="post_item">
            <a href="$url$">$title$</a>
                - <span class="soft">$date$</span>
        </li>
    $endfor$
</ul>
```

Then, the archive template uses this post-list template with partials -- by having the post-list separate, we can reuse it:

```javascript
Here are all my previous posts: 
$partial("templates/post-list.html")$
```

Going back to the rule for making the archive page, we take the empty string and apply the archive and default templates with a new context called ```archiveCtx``` to create the page. This context applies the post context to the posts before they're passed into the post list, and it gives the archive page the title of "Archives" and applies the math and default contexts to it.


```haskell
-- define the archive context
let archiveCtx =
        listField "posts" (postCtx) (return posts)
        <> constField "title" "Archives"
        <> mathCtx
        <> defaultContext

--- make the actual archive page
makeItem ""
    >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    >>= relativizeUrls
```

Finally, we want to create a home page in a similar manner. The index context is almost identical to the archive context because it has the same purpose of running ```$posts$``` through the post context, adding the ```$title$``` field, and applying the math and default contexts to the page. 

```haskell
match "index.html" $ do
    route idRoute
    compile $ do
        -- load published posts in chronological order
        posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*/index.md"
        
        -- define the index context
        let indexCtx =
                listField "posts" (postCtx) (return posts)
                <> constField "title" "Home"
                <> mathCtx
                <> defaultContext

        getResourceBody
            >>= applyAsTemplate indexCtx
            >>= loadAndApplyTemplate "templates/index_template.html" indexCtx
            >>= relativizeUrls
```

Instead of using the archive template, we used the index template, but we also don't use the default template. The index template is based on the default templates but has some structural differences (due to Bootstrap) that make us unable to simply apply the default template to it. 

### Contexts

Contexts give or affect fields that we can use in templates, such as ```$title$``` as we saw in the archive and index contexts. We can compose contexts: defaultContext, a composed context included in the Hakyll library, gives us ```$body$``` and ```$url$```.  

Our posts are each saved as ```index.md``` in their own git repo that's a submodule within the posts submodule in the project. That way, we don't have to specify a further layer to the path within the URL; the default page is the index page. However, we get an ugly URL this way: "http://blog/posts/date-postname/index.html." We can make a context to affect the ```$url$``` field:

```haskell
-- Gets rid of "/index.html" from posts
urlstripCtx :: Context a
urlstripCtx = field "url" $ \item -> do
    route <- getRoute (itemIdentifier item)
    return $ fromMaybe "/" $ 
        -- get rid of the last 10 characters, namely "index.html"
        fmap (reverse . drop 10 . reverse) route
```

And then we can make a post context that composes this URL-stripping context with a ```$date$``` field, math context and default context:

```haskell
-- Adds date and composes a few other contexts
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y"
    <> mathCtx
    <> urlstripCtx
    <> defaultContext
```

### Support for Math

#### MathJax

One of the main features my blog needed was the ability to easily support writing and displaying math. Formatting elsewhere and creating an image that's then embedded in the post is totally unacceptable. Supporting formatting within the post but displaying as an image is ugly.

Luckily, that's what [MathJax] was built for. We can write math as if we're writing in TeX, and it's rendered using JavaScript to work in all browsers. We can even copy and paste math (right click $\rightarrow$ "Show Math As"):

[MathJax]:http://www.mathjax.org/

$$ J_\alpha(x) = \sum\limits_{m=0}^\infty \frac{(-1)^m}{m! \, \Gamma(m + \alpha + 1)}{\left({\frac{x}{2}}\right)}^{2 m + \alpha} $$

We add the field ```$mathjax$``` to our default template, and the math context adds this field containing a script link to MathJax:

```haskell
-- MathJax
mathCtx :: Context String
mathCtx = field "mathjax" $ \item -> do
    metadata <- getMetadata $ itemIdentifier item
    return $ "<script type=\"text/javascript\" src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\"></script>"
```

#### MathBox

Additionally, I found an exciting library called [MathBox](https://github.com/unconed/MathBox.js/) that allows us to embed 3D math diagrams. Here's one of the demos, of a surface and plane intersecting. You can rotate it.
<iframe class="mathbox" src="/js/MathBox.js/examples/Intersections.html" width=100% height=500px seamless="seamless"></iframe>

This doesn't require any special additions to our Haskell file. We simply clone it and save as ```js/MathBox.js/```. Then we can insert diagrams like the one above with the following code:

```html
<iframe class="mathbox" src="/js/MathBox.js/examples/Intersections.html"></iframe>
```

And we can add to our CSS file:

```css
.mathbox{
    width: 100%; 
    height: 500px;
    seamless: "seamless";
}
```

MathBox uses WebGL, so it won't work for all browsers, but it's so cool it's totally worth it. I've started playing around with making my own diagrams, and I plan to write a post about [Mobius Transformations] with a MathBox animated diagram.

[Mobius Transformations]: http://en.wikipedia.org/wiki/M%C3%B6bius_transformation

### Publishing

Finally, it would be great to have a feature such that we can preview posts online without adding a link to them in the archive. Of course, we could always preview locally just with ```./site watch```, but if we want to send a post to someone else to proofread, we can't just point to a URL without adding it to the archive.

The example Hakyll site loads the ```$posts$``` field for the archive and index pages as such:

```haskell
posts <- recentFirst =<< loadAll "posts/*"
```

We edited this line to first filter only the "published" posts:

```haskell
posts <- recentFirst =<< onlyPublished =<< loadAll "posts/*/index.md"
```

```onlyPublished``` returns the posts that have "published" in their metadata:

```haskell
-- For filtering lists of items to only be published items
onlyPublished :: MonadMetadata m => [Item a] -> m [Item a]
onlyPublished = filterM isPublished where
    isPublished item = do
        pubfield <- getMetadataField (itemIdentifier item) "published"
        return (isJust pubfield)
``` 

If we don't include "published" in the metadata of a post, it will not be included in the archive. But the post still exists and we can find it if we manually go to the URL of the post.

And that's the entirety of the Haskell file.

[*More to come later...*]