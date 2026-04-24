default: install

build:
    cargo build --release

install: build
    cargo install --path .

check:
    cargo fmt && cargo clippy -p turbo-m -p turbo-m-core -- -D warnings && cargo test -p turbo-m -p turbo-m-core

# ── Worker (Cloudflare) ──────────────────────────────────────────────────

# Deploy the worker to Cloudflare
worker-deploy:
    cd worker && npx wrangler deploy

# Run the worker locally for development
worker-dev:
    cd worker && npx wrangler dev

# Create the D1 database (run once)
worker-db-create:
    cd worker && npx wrangler d1 create turbo-m

# Initialize the D1 schema
worker-db-init:
    cd worker && npx wrangler d1 execute turbo-m --command "$(cat ../worker/schema.sql)"

# Populate D1 from an existing local SQLite database file.
# Usage: just worker-db-import path/to/.turbo-m.db
worker-db-import db_path:
    sqlite3 {{db_path}} .dump > /tmp/turbo-m-dump.sql
    cd worker && npx wrangler d1 execute turbo-m --file /tmp/turbo-m-dump.sql
    rm /tmp/turbo-m-dump.sql
    @echo "Database imported successfully."

# Set the API_TOKEN secret for the worker
worker-set-token:
    cd worker && npx wrangler secret put API_TOKEN
