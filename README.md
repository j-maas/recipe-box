# Recipe Box

## Getting started
1. `npm install` to install dependencies.
2. `npm run watch` to start a server that makes the app available at http://localhost:8000.

Tests can be run with `npm run test`.

To build everything, run `npm run build` and deploy the `dist` folder.

## TODOs
- [x] Allow nested parentheses in quantities.
- [x] Experiment with having quantities first.
- [x] Parse paragraphs.
- [x] Recipe titles.
- [x] Store recipe persistently.
- [x] Store multiple recipes.
- Rework syntax
  - [x] Move list name outside of parens.
  - [x] Allow parens in description.
  - [x] `<onion: 1>`, `<onion: 1; large onion>`
- Shopping list
  - [x] Generate shopping list from multiple recipes.
  - [x] Allow ticking off items.
  - [x] Persist selected items
  - [x] Persist shopping list.
  - [x] Add button to clear all checks.
  - [ ] Add undo for clearing. Undo should be available until new checks are made.
  - [ ] Add items to shopping list manually.
  - [x] Indicate number of selected and unselected recipes.
  - [ ] Copy button to export shopping list into other apps.
- Sharing
  - [ ] Add copy button.
  - [ ] Import and export recipes from and to files.
  - [ ] Add indicator to top of files that explains how to view the recipes for people who do not know what to do with the files.
  - [ ] Add WebDav integration (for NextCloud).
  - [ ] Add DropBox integration.
- Overview
  - [ ] Filter recipes.
  - [ ] Search by ingredient.
- Recipe
  - [x] Indicate how many ingredients there are in ingredient heading.
  - [x] Show quantities in method. This is helpful when assembling ingredients while cooking.
  - [ ] Highlight occurrences of ingredients on hover.
  - [ ] Prevent display from turning off while cooking. (Wake Lock API (in draft) is only reasonable implementation.)
  - [ ] Allow setting portions.
  - [ ] Embed timers.
  - [ ] Instead of brute deletion, have a bin to protect against accidental deletion.
  - [ ] Add print style removing UI elements.
  - [ ] Delete checks when deleting recipe.
- Editing
  - [ ] Add instructions to edit page, explaining the syntax.
  - [ ] Detect when recipe already exists and offer renaming.
  - [ ] Renaming a recipe should not leave the old one.
- Syntax
  - [x] Improve error messages.
  - [ ] Allow no space between numbers and units.
  - [ ] Prevent multiple repetitions of context explanations. (E.g., when a quantity is empty, there is a problem for an expected float and an expected text that are listed separately.)
  - [ ] Highlight errors in textarea. (See https://codersblock.com/blog/highlight-text-inside-a-textarea/)
  - [ ] Allow named sections representing steps that have individual ingredient lists for preparation.
- [x] Increase font size slightly.
- [ ] Fix highlight on real button differing from link button.
- [ ] Host fonts, instead of fetching them from Google.