Giornata is a foolishly simple journaling system for Emacs. It's hierarchical in
nature and that's rooted in how the journal's files are organized.

```
~/journal
├── 2024     → directory
│   ├── 01      → directory
│   │   ├── 01     → file
│   │   ├── 02
│   │   ├── 03
│   │   ├── …
│   │   ├── 30
│   │   └── 31
│   └── 02
│       ├── 01
│       ├── 02
│       ├── 03
│       └── …
└── …
```

The right-most digits are your journal entries, their file extension is
purposefully omitted because Giornata is not tied to any particular file format,
you can choose whatever you feel most comfortable with.

Giornata is responsible for creating these files and directories so you can
focus on writing.

# Introduction

Let's go over the foundations of this system:
1. Writing something in the journal should be very quick; you shouldn't have to
   think about it.
2. Reviewing the journal should be intuitive; your calendar is the perfect tool
   for that (for more information, see the "Customization" section).

Writing something with Giornata can be done in one of two ways: the first
method, the one you'll be using most often, is to call `giornata-today`, an
interactive function that opens today's journal entry and places the cursor at
the end of the buffer.

Uh oh, did you forget to journal yesterday? Well, that shouldn't be a problem.
If the `calendar` is open, `giornata-from-calendar` can help create (or visit) a
journal entry for the date at point, consider binding it to the return key or
the left mouse button, you may then click on yesterday and begin to write
whatever it is you did that day.

That's everything you need to know to get started, happy journaling!

# Installation

Giornata is not yet available in any package archive so you'll have to obtain it
from source by evaluating the following:

``` emacs-lisp
(package-vc-install "https://git.sr.ht/~grtcdr/giornata" "2024.04.20")
```

Start journaling right away by typing `M-x giornata-today RET` or read through
the next section to get an idea of the different customization options at your
disposal.

# Customization

Your journal is located in `giornata-directory`.

A front matter, which as suggested by the name, is automatically inserted at the
beginning of every journal entry, the contents of the front matter can be
controlled through `giornata-front-matter`.

Customization is mostly delegated to directory local variables for which an
interactive scaffolding function, `giornata-scaffold`, exists *not only* as a
good starting point for users new to Giornata but also to make it more
convenient to reproduce the same "look and feel" that across different machines
via the `giornata-dir-locals` variable.

Remember that thing about reviewing your journal from the calendar? Giornata can
highlight days with a corresponding journal entry if you let it:

```
(add-hook 'calendar-mode-hook #'giornata-calendar-mode)
```

# Support

If you wish to contribute a patch, inquire about something or share your
feedback, you are welcome to send an email to
[~grtcdr/pub@lists.sr.ht][mailing-list]. If you encounter issues of any kind,
please file them in the project's [ticket tracker][ticket-tracker].

![builds.sr.ht status](https://builds.sr.ht/~grtcdr/giornata/commits.svg)

[mailing-list]: mailto:~grtcdr/pub@lists.sr.ht
[ticket-tracker]: https://todo.sr.ht/~grtcdr/giornata
[calendar-preview]: media/calendar.webp
