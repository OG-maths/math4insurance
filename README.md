# Mathematics for Insurance — Quarto site

This repository contains the source of the course website and the rendered site in `_site/`.

## Rendering locally

Render the complete website with:

```bash
quarto render
```

The rendered files are written to `_site/`.

## Publishing with GitHub Pages

The workflow in `.github/workflows/pages.yml` deploys the contents of `_site/` whenever changes are pushed to the `main` branch.

In the GitHub repository, go to **Settings → Pages → Build and deployment → Source** and select **GitHub Actions**.

Then commit both the source files and the rendered `_site/` directory:

```bash
quarto render
git add .
git commit -m "Update website"
git push
```

The deployment status is available under the repository's **Actions** tab.
