# SigRepo skill-runner

A small service that runs our **Claude Code skills** (`.claude/skills/*/SKILL.md`,
e.g. `create-omic-signature`) on the DigitalOcean box, next to R, OmicSignature,
the difexp files, and the SigRepo MCP server.

## Why this exists / why the Agent SDK

Our skills are Claude Code skills (a `SKILL.md` with `name` + `description`
frontmatter). The thing that natively *loads and runs* those is the
[**Claude Agent SDK**](https://code.claude.com/docs/en/agent-sdk) —
`claude-agent-sdk`, which is Claude Code packaged as a library. It discovers
`.claude/skills`, ships the Read/Write/Edit/Bash tools the skills need, runs the
agent loop, and can talk to MCP servers.

> This is **not** the plain `anthropic` API SDK — that one doesn't run skills.
> The Agent SDK drives a local `claude` CLI subprocess, which is why the image
> installs Node + `@anthropic-ai/claude-code` alongside Python.

The R client can't host a Python/Node SDK itself, so the design is:

```
  R client            React UI / Plumber API
  SigRepo::runSkill()  (future: proxy route)
        \                    /
         \                  /   HTTP  (internal db-net only)
          v                v
     ┌──────────────────────────────┐
     │  sigrepo-agent  (this service)│  FastAPI + claude-agent-sdk
     │  POST /run  → Claude Agent SDK│  + claude CLI (Node)
     │  loads /SigRepo/.claude/skills│  built FROM montilab/sigrepo:latest
     └──────────────────────────────┘  → so R / OmicSignature are right here
```

## Endpoints

| Method | Path      | Purpose                                                        |
| ------ | --------- | -------------------------------------------------------------- |
| GET    | `/health` | Liveness + whether the SDK/CLI are importable, key set, etc.   |
| GET    | `/skills` | Skills discoverable under `AGENT_SKILLS_CWD/.claude/skills`.    |
| POST   | `/run`    | Run a prompt with **skills** enabled (Bash/filesystem). Streams SSE. |
| POST   | `/chat`   | Website Q&A **assistant** — admin-gated, MCP-grounded, no Bash. Streams SSE. |

There are two agent profiles here, deliberately different:

- **`/run`** is the skill-runner — full Claude Code tools (Read/Write/Edit/Bash) so
  a skill like `create-omic-signature` can actually run R. Powerful; internal/admin.
- **`/chat`** is the website assistant — **only** the SigRepo MCP query tools
  (`search_signatures`, `compare_signatures`, `run_enrichment`, …), **no**
  Bash/filesystem (enforced by a deny-by-default permission callback), and gated to
  admin accounts. This is what the React "Assistant" page talks to.

`POST /run` body:

```json
{ "prompt": "Use the create-omic-signature skill to build a signature from ...",
  "skills": "all" }
```

The response is `text/event-stream`; each `data:` line is a JSON event
(assistant `text`, `tool_use`/`tool_result`, a final `result` with turns/cost,
plus `start`/`error`/`done` envelopes). SSE is used because a skill run can take
minutes.

## Configuration (env)

| Var                     | Default                    | Notes                                             |
| ----------------------- | -------------------------- | ------------------------------------------------- |
| `ANTHROPIC_API_KEY`     | —                          | **Required.** The key the agent bills against.    |
| `AGENT_TOKEN`           | —                          | Shared secret; required in `X-Agent-Token` if set.|
| `AGENT_SKILLS_CWD`      | `/SigRepo`                 | Dir whose `.claude/skills` are loaded (`/run`).    |
| `AGENT_SKILLS`          | `all`                      | `all` or comma-separated exact skill names.        |
| `AGENT_PERMISSION_MODE` | `bypassPermissions`        | Headless → no human to approve tool calls (`/run`).|
| `AGENT_MCP_URL`         | `http://sigrepo-mcp:8021/` | The SigRepo MCP server `/chat` grounds against.    |
| `SIGREPO_API_URL`       | `http://sigrepo-api:3838`  | Plumber API; `/chat` calls `/whoami` here to admin-gate. |

## The website assistant (`/chat`)

Flow for the chatbot on the site:

```
React "Assistant" page ──POST {prompt, api_key, session_id}──► /agent/chat
   (VITE_AGENT_URL, default "/agent", same origin)                │
                                                                  ├─► POST SIGREPO_API_URL/whoami  → must be role=admin
                                                                  ├─► Claude Agent SDK query()
                                                                  │      tools = mcp__sigrepo__*  (deny-all else)
                                                                  │      MCP  = AGENT_MCP_URL
                                                                  └─◄ SSE: assistant text + tool steps + result(session_id, cost)
```

- **Auth is per-user.** The browser sends the signed-in user's `api_key`; the
  service re-validates it via the API's `/whoami` and requires `admin` (it never
  trusts the browser's role claim). The same `api_key` is injected into the system
  prompt so every MCP call is scoped to what that user can see. The `api_key` is
  **redacted** from tool-call events before they're streamed back.
- **Multi-turn.** Each `result` event carries a `session_id`; the client sends it
  on the next turn and the SDK resumes that session.

### Exposing `/agent/` to the browser

The container has no host port, so the site's TLS nginx (the same one that fronts
`/mcp/`, on the host, not in this repo) proxies a same-origin `/agent/` path to it
and **injects the shared token** so the browser never holds it:

```nginx
location /agent/ {
    proxy_pass http://127.0.0.1:8808/;   # or the sigrepo-agent container on db-net
    proxy_set_header X-Agent-Token "<AGENT_TOKEN>";
    proxy_http_version 1.1;
    proxy_set_header Connection "";
    proxy_buffering off;                  # SSE must not be buffered
    proxy_read_timeout 300s;              # agent turns can take a while
}
```

In dev, point the React app straight at the service with
`VITE_AGENT_URL=http://localhost:8808` (and set `AGENT_TOKEN=` empty, or send it
yourself).

## Deploy on the DO box

1. Create the secrets file next to `docker-compose-vm.yml`:

   ```bash
   cp agent/.env.example .agent_env
   # edit .agent_env: set ANTHROPIC_API_KEY, and AGENT_TOKEN=$(openssl rand -hex 24)
   ```

2. Build + start just this service:

   ```bash
   docker compose -f docker-compose-vm.yml build sigrepo-agent
   docker compose -f docker-compose-vm.yml up -d sigrepo-agent
   ```

3. Verify from another container on `db-net` (it has **no host port** by design):

   ```bash
   docker exec sigrepo-api curl -s http://sigrepo-agent:8808/health
   docker exec sigrepo-api curl -s http://sigrepo-agent:8808/skills
   ```

## Use from R

```r
library(SigRepo)
Sys.setenv(SIGREPO_AGENT_URL = "http://sigrepo-agent:8808",  # inside db-net
           SIGREPO_AGENT_TOKEN = "<AGENT_TOKEN>")
listSkills()
runSkill("Use the create-omic-signature skill to build a signature from the DEGs in /path/to/table.csv")
```

## Local dev

```bash
cd agent
uv venv --python 3.11 && uv pip install -r requirements.txt
export ANTHROPIC_API_KEY=sk-ant-...  AGENT_SKILLS_CWD=../../SigRepo
uv run uvicorn runner:app --port 8808
```

`/health` and `/skills` work without an API key; `/run` needs the key + the
`claude` CLI on PATH.

## Security

- **Internal only.** No `ports:` mapping — reachable solely by other containers
  as `http://sigrepo-agent:8808`. Do not publish it; it runs an agent with
  Bash + filesystem access.
- **Token-gated.** Set `AGENT_TOKEN`; every `/run` must send it.
- **Cost.** Each run bills against `ANTHROPIC_API_KEY`; the final `result` event
  reports `total_cost_usd`.

## Not done yet (follow-ups)

- Wire the host nginx `/agent/` location above on the live box (it's outside this
  repo) so the deployed React "Assistant" page can reach `/chat`.
- Surface the skill-runner (`/run`, e.g. create-omic-signature) in the UI too —
  today only the read-only `/chat` assistant has a page; `/run` is R/CLI-only.
- Rate-limiting / usage caps on `/chat` (each turn bills `ANTHROPIC_API_KEY`).
