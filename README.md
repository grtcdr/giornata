`giornata` is a foolishly simple journaling system, it looks like this:

    2024        <---  year
    ├── 01      <----  month
    │   ├── 01  <-----  day: markdown, org, text, whatever floats your boat.
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

# Introduction

It's quite opinionated, but if the design choices click with you, you will find
joy in how simple it is.

Let's go over the foundations of this system:
1. Writing something in the journal should be very quick; you shouldn't have to
   think about it.
2. Reviewing the journal should be intuitive; and that might as well be an
   interactive calendar.

Writing something in `giornata` can be done in one of two ways: the first
method, the one you'll be using most often, is to call `giornata-today`, an
interactive function that opens today's journal entry and places the cursor at
the end of the buffer.

Uh oh, did you forget to journal yesterday? Well, that shouldn't be a problem,
you can open the `calendar`, click[^1] on yesterday and begin to write whatever
it is you did that day.

That's everything you need to know to get started.

# Configuration

Your journal is located in `giornata-directory`.

A front matter, which as suggested by the name, is automatically inserted at the
beginning of every journal entry, the contents of the front matter can be
controlled through `giornata-front-matter`.

If you'd like to use `giornata` on more than one device, you might be interested
in using `giornata-scaffold` (and customizing it to your liking) which will help
you reproduce the same configuration across your devices through directory local
variables.

# Installation

This package is not yet available on any archives so you'll have to obtain it
from source by evaluating the following:

    (package-vc-install "https://git.sr.ht/~grtcdr/giornata")

# Footnotes

[^1]: `giornata-from-calendar` will create a journal entry for the date at
    point, consider binding it to the return key or the left mouse button.
