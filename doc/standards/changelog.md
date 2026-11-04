Changelog rules
===============

User visible changes are recorded in [`changelog.md`](../../changelog.md) in
the root of the repository.  The primary audience are the developers of
Pytis applications, namely the wx applications.

**When to record**

- Each commit with a user visible change adds its entry to the topmost
  section of the changelog within the same commit.
- User visible means visible to application developers or application users:
  new features, changed behavior, changed or removed API, configuration
  options, specification arguments, dependencies, database migrations and
  fixes of bugs which could be noticed by the users.
- Internal changes are not recorded: type annotations, tests, CI, build and
  release tooling, refactoring without visible effect, translation updates,
  fixes of bugs introduced within the same development cycle.

**Versions**

- Each version has its own section `## <version>`, the newest first.
- The section of the version under development is marked `(unreleased)`.
  Changes committed after a release, before the commit starting a new
  development version, go to the section `## Unreleased`.  The commit
  starting a new development version (`Start 1.3.0.devN`) turns this mark
  into a new section above it.
- Development versions have no date.  Release versions have the release date:
  `## 1.3.0 (2026-12-01)`.
- On a release, the sections of all development versions of the cycle are
  merged into the single section of the release.  Entries fixing or extending
  something introduced within the same cycle are merged with the original
  entry or dropped, so that the release section describes the changes against
  the previous release.

**Structure of a version section**

- Subsections by area, always in this order, empty ones omitted:
  - `### Wx application` -- wx forms, Pytis API, remote client access
    (Pytis2Go), everything the wx applications use exclusively,
  - `### Common` -- the data layer, database specifications and tools,
    presentation specifications, utilities, dependencies,
  - `### REST API` -- `pytis.rest`,
  - `### Web` -- web forms, `pytis.web`, `pytis.cms`, JavaScript and CSS.
- Within a subsection: new features first, then changes, then fixes.

**Entries**

- One brief bullet per change, no lengthy explanations.  Details belong to
  the documentation in the code, not to the changelog.
- Start with what changed: "New ...", "Fixed ...", "... now ...".
- Describe user interface changes from the user's point of view, not by the
  implementation.
- Link the relevant code identifier to the file where its documentation can
  be found, using a relative link without line numbers:
  ``[`Data.rows()`](pytis/data/data.py)``.
- When the change relates to an issue in the issue tracking system, link it
  at the end of the entry:
  `([#521](https://itsng.oui.technology/its/issues/521))`.
- Prefix incompatible changes, which may require changes in applications or
  their configuration, by `**Incompatible:**` and say what to change.
- Wrap the text at 80 characters.

**Changes implemented in a dependency**

- Dependencies developed together with this project (such as LCG) describe
  their changes in detail in their own changelogs.
- A change implemented in such a dependency which is visible to the users of
  this project is recorded also here, described from the point of view of
  the users of this project and marked by the name of the dependency at the
  end, such as "(LCG)".
- API changes of a dependency are not repeated, only the required versions
  are recorded (see below).

**Dependency versions**

- When the project starts to rely on a change in a dependency developed
  together with it, the commit raises the minimal required version of the
  dependency to the version containing the change.  When the change is not
  released yet, the dependency must have a development version containing it
  (start a new one if necessary) and the project requires that development
  version (such as `lcg-framework>=0.9.0.dev1`, which also allows pip to
  install it).
- Raised minimal versions are recorded as "Requires LCG 0.9.0." (in
  `### Common` here).  Mark it **Incompatible** only when the applications
  need to be adjusted.
- The dependencies are released first, a released version only requires
  released versions of its dependencies.
