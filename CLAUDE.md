# 📊 OTIS Statistical Report - Claude Completion Guide

## Project Context
**Project:** Statistical Report on Restrictive Confinement in Ontario's Adult Correctional System  
**Principal Investigator:** Vansh Singh Ruhela (ORCID: [0009-0004-1750-3592](https://orcid.org/0009-0004-1750-3592))  
**Affiliation:** Centre for Criminology & Sociolegal Studies, University of Toronto  
**Data Source:** Ontario Open Data - Jahn Settlement Data on Inmates  
**Repository:** https://github.com/ruhelavansh-oss/OTIS-Report

---

## ✅ Completed Tasks
- [x] Fixed _quarto.yml (cold-fold → code-fold typo)
- [x] Added APA7 citations (references.bib)
- [x] Updated index.qmd with professional metadata & ORCID link
- [x] Removed sensitive personal info (detailed address)
- [x] Cleaned git (removed .DS_Store, .Rprofile, HTML outputs)
- [x] Created .rscignore for Posit Cloud deployment
- [x] Synced & pushed all commits to GitHub
- [x] Deployed to Posit Cloud

---

## 🎯 Remaining Tasks (Batch All)

### Task 1: Update _quarto.yml (Export Links & Navigation)
**What to do:**
- Ensure all export buttons in `other-links:` section are present:
  - 📊 Export: MS Word (.docx)
  - 📄 Export: PDF
  - 📦 MECA Bundle
  - 🐍 Python Notebook
  - 📈 R Analysis Code (GitHub link)
  - 📋 Data Source (Ontario Open Data)
- Add section-level GitHub edit links (pattern: `href: "https://github.com/ruhelavansh-oss/OTIS-Report/edit/main/{filename}.qmd"`)

**If it fails:** Skip this. Manual editing of YAML can be done locally.

---

### Task 2: Update index.qmd (Navigation Badges)
**What to do:**
- Add quick-jump navigation after the metadata section:
  ```markdown
  ## Quick Navigation
  [📖 Introduction](#introduction) · [🔬 Methods](#methodology) · [📊 Results](#results) · [📥 Data Source](#data-source--citation) · [🔗 GitHub](https://github.com/ruhelavansh-oss/OTIS-Report)
  ```
- Ensure ORCID link is active: `[Vansh Singh Ruhela](https://orcid.org/0009-0004-1750-3592)`
- Verify citations are in APA7 format with proper links

**If it fails:** Skip this. The report works without navigation badges.

---

### Task 3: Add "Edit on GitHub" Buttons
**What to do:**
- Add to _quarto.yml under `html:` section (if not already present):
  ```yaml
  page-footer:
    left: "© 2025 Vansh Singh Ruhela"
    right: "[📝 Edit this page](https://github.com/ruhelavansh-oss/OTIS-Report/edit/main/{filename}.qmd)"
  ```

**If it fails:** Skip this. Buttons are nice-to-have, not essential.

---

### Task 4: Verify All Citations
**What to do:**
- Check references.bib contains:
  - ✓ Ontario_SOLGEN_DataOnInmatesOntario_2025
  - ✓ Ontario_JahnSettlement_2025
  - ✓ Ontario_CKAN_API_2025
  - ✓ Ontario_MCSCS_Act (legislation)
  - ✓ Ontario_Correctional_Services_Act (legislation)
- All citations should use APA7 format
- All URLs should be active and accessible

**If it fails:** Skip verification. Citations are already updated from previous work.

---

### Task 5: Local Rendering (Run via Bash)
**What to do:**
```bash
cd ~/CSCRC1225
quarto render --to html 2>&1 | tail -20
quarto render --to docx 2>&1 | tail -20
quarto render --to pdf 2>&1 | tail -20
```

**If it fails (expected, some formats may error):** 
- ✅ Skip the format that fails
- ✅ Report which formats rendered successfully
- ✅ Don't stop the entire process
- HTML should ALWAYS work. DOCX usually works. PDF may need LaTeX.

**Fallback:** If PDF fails due to LaTeX:
```bash
# PDF requires tinytex, skip if not available
quarto render --to pdf
# Expected: May fail - that's okay, HTML is primary format
```

---

### Task 6: Professional Git Commit (One Commit, Not Multiple)
**What to do:**
```bash
git add -A
git commit -m "docs: add professional navigation, export buttons, and verify APA7 citations

- Added quick-jump navigation badges to index.qmd
- Updated _quarto.yml with export links (Word, PDF, MECA, Python, R, GitHub)
- Verified ORCID link and Jahn Settlement data source citations
- Ensured APA7 formatting for all references
- Added section-level GitHub edit links

By: Vansh Singh Ruhela
Project: Statistical Report on OTIS - Restrictive Confinement Analysis
Data Source: Ontario Open Data (data.ontario.ca)
Affiliation: Centre for Criminology & Sociolegal Studies, University of Toronto"

git log --oneline -1  # Verify commit created
```

**If it fails:** Skip git operations. User can commit manually.

---

### Task 7: Deploy to Posit Cloud
**What to do:**
```bash
cd ~/CSCRC1225
Rscript -e "rsconnect::deployApp(
  appDir = '.', 
  appName = 'Statistical-Report-on-OTIS',
  forceUpdate = TRUE,
  launch.browser = FALSE
)" 2>&1 | tail -30
```

**If it fails:** Skip deployment. Report is still available locally.

**Expected outcome:** Updated deployment at:  
https://connect.posit.cloud/vanshsinghruhela/content/019b7c00-3ae0-8fde-a7d6-2af24e725e86

---

## 🚫 Skip Rules (Budget-Friendly)

If Claude hits any of these limits/errors, **SKIP and report what was done:**

| Limit Hit | What to Skip | Still Do |
|-----------|------------|----------|
| Rate limit | Any remaining bash operations | Report what completed |
| PDF render fails | Don't retry PDF | HTML + DOCX are sufficient |
| Deployment timeout | Skip Posit Cloud update | Push to GitHub only |
| Large file read errors | Skip reading huge JSON files | Use grep/bash summaries instead |
| YAML parse errors | Don't re-edit config | Provide instructions for manual fix |

**Golden Rule:** Do what you can, skip what you can't, report clearly.

---

## 📋 Quality Checklist (For Claude to Verify)

- [ ] index.qmd has navigation badges and active ORCID link
- [ ] _quarto.yml has all 6 export buttons visible
- [ ] references.bib contains Jahn Settlement + Ontario data citations
- [ ] APA7 format confirmed in PDF output settings
- [ ] HTML renders without errors
- [ ] Git commit created with professional message
- [ ] GitHub push successful (or skipped)
- [ ] Posit Cloud deployment attempted (failure okay)

---

## 🔍 Final Status Report Template

After all tasks, Claude should provide:

```
=== OTIS Report Completion Status ===

✅ COMPLETED:
- Task 1: _quarto.yml exports
- Task 2: index.qmd navigation badges
- Task 3: GitHub edit buttons
- Task 5: HTML rendering successful

⏭️ SKIPPED (Budget/Limits):
- Task 7: Posit deployment (rate limit hit)

❌ FAILED (Non-Critical):
- Task 5: PDF rendering (LaTeX not available)

📊 OUTPUT FILES:
- HTML: _book/index.html (ready to view)
- DOCX: _book/Statistical-Report-on-OTIS.docx
- Git: Commit [hash] pushed to main

💡 NEXT STEPS:
1. View local report: open _book/index.html
2. Download DOCX/PDF from _book/ folder
3. Share Posit link when deployment completes
```

---

## 💰 Budget Tips

**Free Operations (Do First):**
```bash
quarto render          # Local, no API
git commit             # Local, no API
git push               # Should be free
rsconnect deploy       # Free once authenticated
```

**Low-Cost Claude Operations:**
- Use `grep`, `sed`, `awk` instead of reading large files
- Batch 3-5 tasks per prompt
- Ask to "skip if hits limit" at start

**Avoid:**
- Multiple small prompts (5 prompts = 5× cost)
- Large file reads (grep is faster)
- Repeated renders

---

## 🎯 Suggested Next Prompt for Claude Code

**Copy-paste this into Claude Code when ready:**

```
Load my CLAUDE.md file for context.

Run these tasks in order, skipping any that fail or hit limits:

1. Update _quarto.yml: ensure all export buttons present (Word/PDF/MECA/Python/R/GitHub/Data)
2. Add navigation badges to index.qmd after metadata section
3. Verify references.bib has Jahn Settlement + Ontario data citations in APA7
4. Render to HTML locally: quarto render --to html
5. Render to DOCX locally: quarto render --to docx (skip if fails)
6. ONE git commit: "docs: add professional navigation and verify citations"
7. Push to GitHub: git push origin main
8. Try Posit Cloud deployment (skip if timeout or rate limit)

Report status when done.
```

---

## 📚 Reference Links

- **GitHub Repo:** https://github.com/ruhelavansh-oss/OTIS-Report
- **Posit Deployment:** https://connect.posit.cloud/vanshsinghruhela/content/019b7c00-3ae0-8fde-a7d6-2af24e725e86
- **Data Source:** https://data.ontario.ca/dataset/data-on-inmates-in-ontario
- **Jahn Settlement Info:** https://www.ontario.ca/page/jahn-settlement-data-inmates-ontario
- **ORCID Profile:** https://orcid.org/0009-0004-1750-3592
- **Inspiration - r4ds:** https://r4ds.hadley.nz
- **Inspiration - Openscapes:** https://openscapes.github.io/quarto-website-tutorial/

---

## 🔗 Data API Example (For Reference)

If you need to pull live data:
```bash
curl 'https://data.ontario.ca/api/3/action/datastore_search?resource_id=5a0c5804-a055-4031-9743-73f556e43bb4&limit=5'
```

---

**Last Updated:** January 5, 2026  
**Author:** Vansh Singh Ruhela  
**Status:** Ready for final Claude Code completion