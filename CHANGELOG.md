# 2024.04.03

- Make the major mode a customizable option

  You're not limited to just `markdown-mode`, you can any of the currently
  supported file formats (Org, Markdown, or plain and unencumbered text) you
  like and the front matter will follow suit.

- Remove dependency on `markdown-mode`

# 2024.02.19

- Add a function, `giornata-scaffold`, to scaffold a predefined configuration
  `giornata-dir-locals`
- Prefer `find-file` over `find-file-other-window` when creating an entry

# 2024.01.08

- Change the `title` attribute to `date` in giornata-front-matter
- Add function `giornata-consult` for searching the diary
- Tweak the declaration of dependencies; `markdown-mode` is explicited
  required and `consult` functions are only declared as it is an
  inessential/optional dependency.
- Rename the to-dos file to `TODOS`

# 2023.12.21

This marks the first release which includes basic journaling capabilities in
addition to calendar integration.
