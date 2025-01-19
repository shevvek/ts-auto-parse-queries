# TS-Auto-Parse-Queries

`ts-auto-parse-queries.el` transforms Tree-Sitter native queries into Emacs `treesit` sexp format font-lock rules.

Most Treesitter grammars include `.scm` files that non-Emacs editors use as-is for syntax highlighting. Emacs `treesit` unfortunately can't use these directly. This script can help bootstrap development of new Treesitter major modes by providing a working starting point for font-lock rules.

## Usage

1. Set `ts-auto-query-files` to a list of paths to `.scm` query files included with a Treesitter grammar, relative to the grammar root directory.
2. Set `ts-auto-query-lang` to the name of the language.
3. Call `(ts-auto-parse-queries <dir>)` replacing `<dir>` with the path to the grammar root directory. The automatically generated font-lock rules will be created in `ts-auto-query-dir`, by default `./auto-queries/`.

## Implementation

The automatic conversion is performed as follows:

1. Prepend to each query:
   ```
   :language lilypond
   :feature <name of query file>
   ```
2. Transform anchors and quantifiers as follows:
   ```
   * -> :*
   ? -> :?
   + -> :+
   . -> :anchor
   ```
3. Use `pcre2el` to transform `#match?` regex clauses into Emacs readable format.
4. UNTESTED: adjust format of `#equal?` and `#pred?` clauses. `#equal?` clauses *should* work. `#pred?` clauses will require manual editing.
5. Any other predicate clauses are unsupported currently in Emacs and are deleted.
6. Define a new Emacs font face for each capture name used in a query, to inherit from standard font lock faces based on keywords included in the capture names. The mapping is defined by `ts-auto-query-font-face-alist`.

Emacs balanced-expression navigation and [pcre2el](https://github.com/joddie/pcre2el) do all the hard work.
