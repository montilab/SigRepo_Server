# SigRepo React UI Mockup

A visual-only React translation of the existing R Shiny frontend (`shiny/`),
built to evaluate what a React-based SigRepo UI could look like.

**This is a mockup, not a working app.** All data is static (`src/data/mock.ts`) —
there is no connection to the Plumber API, MCP server, or database. Login accepts
any input and always succeeds.

## Pages

Mirrors the tabs in the current Shiny navbar (`shiny/app_src/app_ui.R`):
Home, Signatures, Collections, Annotate, Compare (stub, matching the real app),
Browsing, Feedback.

## Run it

```
cd web
npm install
npm run dev
```

## Notes

- Visual design (colors, gradients, card/table layout) was reverse-engineered
  from the real Shiny module CSS (`shiny/modules/*.R`, `shiny/www/assets/css/`)
  to look like a faithful translation rather than a generic redesign.
- If this direction is worth pursuing for real, the next step is wiring pages
  up to the existing REST API (`api/`) instead of `src/data/mock.ts`.
