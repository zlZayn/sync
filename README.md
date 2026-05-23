# sync

[English](README.md) | [简体中文](README_zh.md)

Personal cloud storage and version control system for syncing files across devices.

## Overview

This repository serves as a centralized hub for keeping important files in sync across multiple machines. It uses Git as the underlying version control mechanism, ensuring that every change is tracked and can be reverted if needed.

## Features

- Cross-device file synchronization
- Git-based version control for all synced files
- GitHub remote backup for redundancy

## Quick Start

### Clone on a new device

```bash
git clone https://github.com/zlZayn/sync.git
cd sync
```

### Sync local changes

```bash
git add .
git commit -m "description of changes"
git push origin main
```

### Pull latest changes

```bash
git pull origin main
```

## Best Practices

- **No sensitive data**: Do not commit passwords, API keys, or credentials. Use `.env` files with `.gitignore` instead.
- **Frequent commits**: Make small, focused commits with clear messages rather than large batch changes.
- **Pull before push**: Always run `git pull origin main` before pushing to minimize merge conflicts.
- **Descriptive messages**: Write commit messages that explain what changed and why.

---

Maintained by Zayn
