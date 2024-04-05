Giornata is a foolishly simple journaling system for Emacs. It's hierarchical in
nature and that's rooted in how the journal's files are organized.

    ~/diary
    └── 2024
        ├── 01
        │   ├── 01
        │   ├── 02
        │   ├── 03
        │   ├── ..
        │   ├── 30
        │   └── 31
        └── 02
            ├── 01
            ├── 02
            ├── 03
            └── and so on...

The right-most digits are journal entries, their file extension is purposefully
omitted because Giornata is not tied to any particular file format, you can
choose whatever you feel most comfortable with. The year and month are just
directories.

Giornata is responsible for creating these files and directories, so you can
focus on writing.

# Introduction

Let's go over the foundations of this system:
1. Writing something in the journal should be very quick; you shouldn't have to
   think about it.
2. Reviewing the journal should be intuitive; your calendar is the perfect tool
   for that.

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

# Configuration

Your journal is located in `giornata-directory`.

A front matter, which as suggested by the name, is automatically inserted at the
beginning of every journal entry, the contents of the front matter can be
controlled through `giornata-front-matter`.

If you'd like to use `giornata` on more than one device, you might be interested
in customizing `giornata-dir-locals` and using `giornata-scaffold` to reproduce
the same configuration across all your devices.

Remember that thing about reviewing your journal from the calendar? Giornata can
highlight days with a corresponding journal entry if you let it:

```
(add-hook 'calendar-mode-hook #'giornata-calendar-mode)
```

# Installation

Giornata is not yet available in any package archive so you'll have to obtain it
from source by evaluating the following:

``` emacs-lisp
(package-vc-install "https://git.sr.ht/~grtcdr/giornata" "2024.04.03")
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
