## About

This is a blog post with live code about concurrency programming in Haskell.

## Project structure

- `post.org` is the blog post, in Emacs Org-Mode and Babel live code
-  `index.html` is the post in HTML format, served from http://massimo-zaniboni.github.io/threads-post
- `threads-post.cabal` is the entry point for the Haskell code
- `Makefile` specifies the benchmarks and tests parts inserted in the document

## How to generate the `post.html` document

First generate all the benchmarks parts with

    make clean
    make all

Open `post.org` inside Emacs + Org-Mode + Babel. Tell Emacs to export the document to HTML (in Spacemacs is the sequence `SPC m e e h h`). Probably you must first configure Babel for recognizing the type of scripts inside the document.

Copy manually `post.hmtl` to `index.html`

Commit and push to GitHub. 

Check http://massimo-zaniboni.github.io/threads-post

## License

Source code and post content is released under BSD2 license. See `LICENSE` file for more details.

