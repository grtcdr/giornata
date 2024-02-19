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
