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
  - [ ] Persist shopping list.
  - [ ] Add items to shopping list manually.
  - [ ] Indicate number of selected and unselected recipes.
- Sharing
  - [ ] Add copy button.
  - [ ] Import and export recipes from and to files.
  - [ ] Add indicator to top of files that explains how to view the recipes for people who do not know what to do with the files.
- Overview
  - [ ] Filter recipes.
- Recipe
  - [ ] Indicate how many ingredients there are in ingredient heading (before opening the list).
  - [ ] Switch for showing/hiding quantities in description. This is helpful when assembling ingredients while cooking.
  - [ ] Highlight occurrences of ingredients on hover.
  - [ ] Allow setting portions.
  - [ ] Embed timers.
  - [ ] Instead of brute deletion, have a bin to protect against accidental deletion.
- Editing
  - [ ] Add instructions to edit page, explaining the syntax.
  - [ ] Detect when recipe already exists and offer renaming.
- Syntax
  - [ ] Improve error messages.
  - [ ] Allow named sections representing steps that have individual ingredient lists for preparation.
- [ ] Fix highlight on real button differing from link button.