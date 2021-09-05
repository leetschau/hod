# hod: donno in Haskell

A full-featured note-taking application in terminal.

## Features

* Easy note management: add, update, list, remove note;
* Organize notes in hierarchical notebooks;
* Full featured formating support: you can add rich texts (markdown),
  mathematical formula (mathjax, LaTeX), diagrams, charts (mermaid),
  images, and attachments. All will be rendered and displayed lively in your browser;
* Powerful and fast full-text search. Get all information at your fingers;
* Safe and secure: all notes are saved in plain texts (markdown). You own your data.
  You can view and update your notes without any specific applications
  but a text editor when necessary. All notes store in private git repository;
* Live in console: git style APIs. No need to learn a new GUI app.
  Get your notes anytime, anywhere: Linux, macOS, Windows (through WSL),
  Android (by Termux), SSH, ...

## Install

Copy executable `hod` to a folder in your $PATH.

### Prerequisites

* [git](https://git-scm.com/) for note synchronization;
* [pandoc](https://pandoc.org/) and [mermaid](https://github.com/mermaid-js/mermaid)
  for preview with browser.

On Debian-based systems, install them with:
```
apt install git pandoc
npm install -g @mermaid-js/mermaid-cli mermaid-filter
```

## Usage

List all commands with `hod`.

## Add attachments in a note

You can add attachments in your notes as in any other markdown file:

1. Copy the attachment file into folder <note-repo>/resources;
1. In your note, add a reference to your attachment.

For example, to add a image named "myimg.png",
copy it to folder ~/.donno/repo/resources and add a reference
`![my image](resources/myimg.png)` in your note.
To add other attachment (instead of an image), say "myatt.pdf",
copy it to folder ~/.donno/repo/resources and add a reference
`[my doc](resources/myatt.pdf)`.

To make it more convenient, you can define some helper command in your editor.
For example, with [my helper function for vim](https://github.com/leetschau/vimrcs/blob/master/donno/nvim/init.vim),
you can simply add image or other file in one step:
```
:Atti myimg.png    " use filename as caption
:Atti myimg.png MyImage  " specify the caption as *MyImage*
:Atti myatt.pdf    " use filename as caption
:Atti myatt.pdf MyDoc  " specify the caption as *MyDoc*
```

## Configuration

File path: ~/.config/hod/config.json

### Configuration Items

#### General

* app_home: root folder of donno data files. Default: ~/.donno
* repo: folder to save all note files and resource files. Default: $app_home/repo
* editor: which application to use to create/update note. Default: `nvim`
* viewer: which application to use to view notes in console. Default: `nvim -R`
* default_notebook: default notebook name for a new note. Default: `/Diary`
* logging level: debug or info(default)
* editor_envs: environment variables of the editor. For example,
  env `XDG_CONFIG_HOME` is used by neovim to load config/plugins to parse markdown files.
  Default: `~/.config`

#### Blog

* url: blog url
* publish_cmd: command to publish notes to blog

### Manage Configurations

```
hod conf get                # list all current configurations
hod conf get edtior         # get which editor to use
hod conf set editor nvim    # set the editor, make sure you've installed it
hod conf set default_notebook /Diary/2020

# set nested attribute:
hod conf set editor_envs.XDG_CONFIG_HOME $HOME/.config/vimrcs/text

# restore default values of configurations:
hod conf restore
```

## Advanced search

You can search notes by their title, tags, notebook, created date and updated date
by adding `-a` option after search command and one or more *search terms*:

hod s -a <term1> [<term2> ...]

Each *term* is a 3-tuple: 

    range:word[:style]

*range* can be one of ti/ta/nb/cr/up/co, meaning
title, tag, notebook, created, updated or all contents, respectively.

*word* is the word you want to search.

*style* is optional, defaulting to `Wi` (not-whole-word-match and ignore-case).
Its candidates include:

* w: whole word match
* W: not whole word match
* i: ignore case
* I: strict case
* b: before the date specified in *word* section when *range* is `cr` or `up`
* B: not earlier than the date specified in *word* section when *range* is `cr` or `up`

For example, the following command searches all notes that contains a tag *ltr*
(whole-word-match and ignore case), updated after 2021.6.1 and in notebook
whose name contains string *tech*, case insensitive:

    hod s -a ta:ltr:wi up:2021-06-01:B nb:tech

Other examples:
```
hod s -a ti:python  # search all notes with "python" in their titles
hod s -a co:python:w  # whole word match for "python" anywhere in the note
```

## Synchronization between Multiple Devices

### Main method: VCS

To sync notes between multiple devices,
you need a *remote* VCS (version control system) repository.
The simplest way is creating it on a source-code-hosting platform,
such as github.com, gitlab.com, gitee.com, etc.

Then push your *local note folder* (get its path with `hod conf get repo`,
if you didn't set it explicitly) to remote repository.

On a new device, clone the remote repository to the local note folder.

When you update notes on device A, and want to sync the changes to device B,
run `git pull` in the local note folder of device B.

With the powerful merge function of git,
you can create and/or update notes on multiple devices simultaneously,
then sync them with *rebase* or other methods.

### Complement method: patch file

When the changes of notes are too small to form a meaningful commit,
you can patch them to a file, then restore it on another device.

## Publish to Blog

If you want to publish notes in a specified notebook to blog,
see [blog doc](./blog/README.md) for details.

## Roadmap

### In developing

### On schedule

1. Basic publishing module: publish to blog, such as github.io

1. Advanced publishing function: publish specific note, or notes in specific notebook

1. Search notes with the whole words;

1. Better appearance: beautify output table with libraries like [colored][col], [tabulate][tab], etc

1. Search notes with keywords (which is extracted with AI algorithms, such as NLP, TF-IDF, etc);

1. Similar notes recommendations of a specific note with recommendation algorithms;

1. Note syntax check with NLP;

1. Knowledge graph construction and visualization (in TUI) from a specific root (note);

1. Note translation, English to Chinese and vice versa.

1. Suggest when typing. For example: press TAB after input `hod s py`, some candidates displayed
   below: python, pyhive, pynvim, ...

### Completed

1. Synchronize notes with patch file as the complement to the main sync
   mechanism based on git;

1. Basic note-taking functions: add, delete, list, search, view, update notes

1. Configuration module: see [Configuration](#configuration);

1. Preview: render markdown notes to HTML and previewed in browser

1. Support adding attachments into notes, especially images

1. Add logging system, distinguish application (for end user) and debugging (for developer) logs

1. Notebook management: list notebooks, list notes in specified notebook

1. Synchronize notes between hosts (based on VCS, such as git)

1. Import/Export from/to other open source note-taking apps,
   such as [Joplin](https://joplinapp.org/)

1. Advanced search function: search by title, tag, notebook and content

1. Search with regular expression

[col]: https://pypi.org/project/colored/
[tab]: https://pypi.org/project/tabulate/

