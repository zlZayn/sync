# sync
> **Personal Cloud Storage & Version Control**  
> Centralized repo for syncing configs, docs, scripts across devices.

## Purpose
- Cross-device file sync
- Git version control for critical files
- GitHub remote backup

## Quick Start

### Pull latest (new device)
```bash
git pull origin main
```

### Sync local edits
```bash
git add .
git commit -m "feat: upload files"
git push origin main
```

## Best Practices
- No sensitive data (passwords/keys) — use .env + .gitignore
- Commit frequently with small changes
- Pull before push to avoid conflicts

---
*Maintainer: Zayn*