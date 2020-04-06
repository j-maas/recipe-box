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
- [x] Rework syntax
  - [x] Move list name outside of parens.
  - [x] Allow parens in description.
  - [x] `<onion: 1>`, `<onion: 1; large onion>`
- [ ] Generate shopping list from multiple recipes.
- [ ] Import and export recipes from and to files.
- [ ] Filter recipes.
- [ ] Indicate how many ingredients there are in ingredient heading (before opening the list).
- [ ] Switch for showing/hiding quantities in description. This is helpful when assembling ingredients while cooking.
- [ ] Highlight occurrences of ingredients on hover.
- [ ] Allow setting portions.
- [ ] Embed timers.
- [ ] Instead of brute deletion, have a bin to protect against accidental deletion.