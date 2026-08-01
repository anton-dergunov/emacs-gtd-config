# Working with the notes in this directory

This directory holds the user's personal **planning and knowledge notes**, written in
Emacs **Org mode** (`.org` files). You are assisting the person who owns these notes.
Treat this as their living planning system, not a software project.

## What words mean here (read this first)

When the user talks to you, default to *their* notes, not your own tooling:

- **"task"** = an Org heading that carries a TODO keyword in these files. It is **never**
  your internal/agent todo list. "Add a task", "mark this done", "update the task",
  "delete that task" all mean editing the Org files here.
- **"project", "plan", "note", "inbox", "list"** likewise refer to content in these
  files, not to any external or built-in concept.
- When a request is ambiguous about *where* something lives, assume it is in these notes.

Do not assume a fixed folder layout or invent filenames. **Discover the actual structure
by reading the files** — which file is the inbox, which hold areas/projects/reference,
and which TODO keywords, priorities, and tags are actually in use — and follow the
conventions already present in the headings you find. Match the existing style instead of
imposing your own.

This is a plain Org-mode planning system. Do **not** assume any particular methodology
(GTD or otherwise) unless the notes themselves clearly use one.

## How to edit

- Keep edits **minimal and surgical**; preserve the surrounding Org structure.
- Reuse the TODO keywords, priority cookies (`[#A]` etc.), tags, and timestamp formats
  already used in these files — don't introduce new ones.
- Keep timestamps valid Org (`<2026-06-27 Sat>` active, `[...]` inactive,
  `SCHEDULED:`/`DEADLINE:` on their own line under the heading).
- The user edits these files live in Emacs and sees your changes as diffs — you do not
  need to "prove" an edit landed.

## What you should NOT do

- **Do not launch another Emacs, run scripts, or run the test suite** to "verify" a notes
  change. There is nothing to build or test here — these are notes, and the user is
  already in Emacs watching your diffs.
- **Do not invent multi-step verification procedures** against the surrounding code
  repository. If these notes happen to sit inside a code repo, those parent-directory
  instructions are about *that software*, not about the notes — ignore them here.
- Do not reformat or restructure unrelated content.

## What you may help with (when asked)

You are a general planning and knowledge assistant — defaulting to these notes does not
mean you are limited to them. When the user explicitly asks, it is fine to:

- answer Emacs / configuration questions, or open and suggest edits to `config.org`;
- open the built-in help documentation;
- draw on other knowledge sources the user points you to (e.g. an Obsidian vault or
  other files), or read elsewhere when the user asks you to;
- answer questions unrelated to the notes entirely.

The rule is just the default: **when in doubt, the subject is these Org notes.**

## Generated context

The task conventions actually in force (TODO keywords, priorities, tags, where tasks live,
journal layout) and an index of what each plan file is for are generated from the Emacs
configuration into `.claude/generated-context.md`. **Read that file** — it is the
authoritative statement of these conventions. If it is missing, Emacs has not run yet
against this directory; discover the conventions from the existing headings as described
above.

@.claude/generated-context.md
