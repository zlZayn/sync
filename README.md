# Sync
> **Personal Cloud Storage & Version Control**  
> A centralized repository for synchronizing configuration files, important documents, and scripts across multiple devices.

## 🎯 Purpose
- **Cross-Device Sync**: Seamlessly keep files updated between work and personal machines.
- **Version History**: Track changes in configuration files and critical documents using Git.
- **Secure Backup**: Maintain a remote backup on GitHub for disaster recovery.

## 🚀 Quick Start

### Pull latest changes (on a new device)
```bash
git pull origin main
```

### Sync local changes (after editing files)
```bash
git add .
git commit -m "feat: upload files"
git push origin main
```

## ⚠️ Best Practices
- **Do not store sensitive data**: Avoid committing passwords, API keys, or private credentials. Use `.env` files and add them to `.gitignore`.
- **Commit often**: Small, frequent commits make it easier to resolve conflicts.
- **Pull before push**: Always run `git pull` before starting work to minimize merge conflicts.

---
*Last Updated: 2026-03-05 | Maintainer: Zayn*
