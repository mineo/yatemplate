[![MELPA](http://melpa.org/packages/yatemplate-badge.svg)](http://melpa.org/#/yatemplate)
[![MELPA Stable](http://stable.melpa.org/packages/yatemplate-badge.svg)](http://stable.melpa.org/#/yatemplate)

# YATemplate #

## Installation ##

Install this package from [MELPA][] with <kbd>M-x package-install RET
yatemplate RET</kbd>.

## Description ##

This package bridges the gap between [YASnippet][yasnippet_homepage] and
[auto-insert-mode][]. By populating auto-insert-alist with filenames and
automatically expanding their content after insertion by auto-insert-mode, it's
an easy way to create dynamic file templates. Simply call
`yatemplate-fill-alist` somewhere in your Emacs initialization file to populate
`auto-insert-alist` with filenames from `yatemplate-dir`.

Each filename will be turned into a new element to `push` onto
`auto-insert-alist`. To guarantee a particular order, filenames must contain one
colon (":"). After collecting all the filenames in `yatemplate-dir`, their names
will be sorted with `string<`, then split on the colon. The first substring will
be discarded, which means it can be used to establish an ordering. The second
substring will be used as a regexp as the CONDITION of the element to push onto
`auto-insert-alist`. The ACTION will be a vector of actions that first insert
the content of the template file and then expand the content of the buffer with
`yatemplate-expand-yas-buffer`, which simply calls `yas-expand-snippet`, so you
can use everything [YASnippet][yasnippet_writing] offers in the template.

Note that a dollar sign `$` will be added to the end of the regular expression
automatically because most of the template filenames will very likely be of the
form `filename.extension`. If you want to specify a template filename where the
last letters are not the extension, add `.*` at the end.

This means that if `yatemplate-dir` looks like this:

    .emacs.d/templates
    ├── 00:test_.*.py
    └── 01:.*.py

`yatemplate-fill-alist` will first `push` `(".*.py$" . ACTION)` onto
`auto-insert-alist` and then `("test_.*.py$" . ACTION)`.

Of course, you will need to enable [auto-insert-mode][] to have the snippet
inserted and expanded into new files.

[MELPA]: http://melpa.org "MELPA"

[auto-insert-mode]: https://www.gnu.org/software/emacs/manual/html_node/autotype/Autoinserting.html "auto-insert-mode"

[yasnippet_writing]: http://capitaomorte.github.io/yasnippet/snippet-development.html

[yasnippet_homepage]: https://joaotavora.github.io/yasnippet/
