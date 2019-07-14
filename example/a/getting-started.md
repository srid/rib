---
title: "Getting Started"
description: Start using Rib to generate your own static website
---

```bash
mkdir -p mysite/content/static && cd mysite

wget https://.... -O Main.hs

cat > content/first-post.md  <<EOF
# Hello world

_This_ can be written in *Markdown*.
EOF

git clone https://github.com/srid/rib rib
./rib/ghcid
```
