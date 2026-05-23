# sync

[English](README.md) | [简体中文](README_zh.md)

个人云存储与版本控制系统，用于跨设备同步文件。

## 概述

本仓库作为集中化枢纽，保持重要文件在多台设备间同步。底层使用 Git 作为版本控制机制，确保每次变更都有记录且可回退。

## 功能特性

- 跨设备文件同步
- 基于 Git 的版本控制，管理所有同步文件
- GitHub 远程备份，提供冗余保障

## 快速开始

### 在新设备上克隆

```bash
git clone https://github.com/zlZayn/sync.git
cd sync
```

### 同步本地变更

```bash
git add .
git commit -m "变更描述"
git push origin main
```

### 拉取最新变更

```bash
git pull origin main
```

## 最佳实践

- **不要存放敏感数据**：不要提交密码、API Key 或凭据。请使用 `.env` 文件并加入 `.gitignore`。
- **频繁提交**：做小而聚焦的提交，附带清晰的提交信息，而非大批量变更。
- **先拉后推**：推送前始终先运行 `git pull origin main`，以减少合并冲突。
- **描述性信息**：提交信息应说明改了什么以及为什么改。

---

由 Zayn 维护
