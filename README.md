# RSS View

A clean RSS feed reader that runs entirely in the browser, built with [Elm](https://elm-lang.org/) and hosted on GitHub Pages.

## Features

- Add and remove RSS/Atom feed URLs
- Articles sorted newest-first across all your feeds
- Unread/read tracking (persisted in `localStorage`)
- Filter articles by individual feed
- Refresh all feeds or retry failed ones
- Light/dark theme following system preference

## How it works

Feed data is fetched through the [rss2json.com](https://api.rss2json.com) API, which converts RSS/Atom XML to JSON and handles CORS so the browser can fetch feeds directly. No backend required.

## Development

```bash
npm install -g elm
elm make src/Main.elm --output=elm.js
# Open index.html in a browser
```

## Deployment

Pushing to `main` triggers the GitHub Actions workflow which compiles the Elm app and deploys it to GitHub Pages automatically.
