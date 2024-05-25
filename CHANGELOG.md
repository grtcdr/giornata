# 2024.05.25

- `giornata--create-entry` now uses `pop-to-buffer` providing users the freedom
  to customize how the buffer is displayed
- Add a variable named `giornata-hide-calendar` that controls whether the
  calendar is closed after invoking `giornata-from-calendar`

# 2024.05.05

- Add support for reStructuredText aka rst-mode
- Add customizable option for showing a menu item
- Make giornata--date-from-filename more robust
- Remove references to olivetti from giornata-dir-locals as it may cause issues
  for users who do not have it installed

# 2024.04.20

- Improve the documentation
- Write a test suite spanning most of the code base
- Put a pipeline into place to catch test errors
- Mark private functions which were previously public
- Switch to advice-add to advise calendar-generate-month
- Set the type property of the customizable variable `giornata-dir-locals` to
  from `list` to the more correct `alist`
- Remove the interactive function `giornata-consult`

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
