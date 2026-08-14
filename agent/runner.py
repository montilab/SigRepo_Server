"""
SigRepo skill-runner
====================

A thin HTTP wrapper around the **Claude Agent SDK** so the rest of the SigRepo
stack (R client, Plumber API, React UI) can run our own
``.claude/skills/*/SKILL.md`` skills -- e.g. ``create-omic-signature`` -- on the
DigitalOcean box, right next to R / OmicSignature / the difexp files / the
SigRepo MCP server.

Why the Agent SDK (and not the plain ``anthropic`` API SDK): our skills are
Claude Code SKILL.md skills. The Claude Agent SDK (``claude-agent-sdk``) is
Claude Code packaged as a library -- it *discovers* ``.claude/skills``, ships the
Read/Write/Edit/Bash tools those skills need, runs the agent loop, and can talk
to the SigRepo MCP server. It drives a local ``claude`` CLI subprocess, so the
container must have Node + ``@anthropic-ai/claude-code`` installed (see Dockerfile).

Endpoints
---------
GET  /            -> service info
GET  /health      -> liveness + whether the SDK/CLI are importable
GET  /skills      -> skills discoverable under AGENT_SKILLS_CWD/.claude/skills
POST /run         -> run a prompt with skills enabled; streams SSE events

Security
--------
This service runs an agent with Bash + filesystem access. It is meant to run
ONLY on the internal docker network (no published host port), and additionally
checks an optional shared token (``AGENT_TOKEN``) on every mutating request.
**Never expose it to the public internet.**
"""

from __future__ import annotations

import json
import os
import shutil
from pathlib import Path
from typing import List, Optional, Union

import httpx
import yaml
from fastapi import Depends, FastAPI, Header, HTTPException
from fastapi.responses import StreamingResponse
from pydantic import BaseModel

# --------------------------------------------------------------------------- #
# Configuration (all overridable via environment / compose)
# --------------------------------------------------------------------------- #

# Working directory whose .claude/skills the agent should load. On the DO box
# this is the bind-mounted /SigRepo, which already carries our SKILL.md skills.
SKILLS_CWD = os.environ.get("AGENT_SKILLS_CWD", "/SigRepo")

# Which skills to enable: "all" (default) or a comma-separated list of exact
# skill names. The SDK validates names against discovered skills and rejects
# anything malformed, so keep these in sync with .claude/skills.
_raw_skills = os.environ.get("AGENT_SKILLS", "all").strip()
DEFAULT_SKILLS: Union[List[str], str] = (
    "all" if _raw_skills in ("", "all") else [s.strip() for s in _raw_skills.split(",") if s.strip()]
)

# Headless -> no human to click "approve", so the agent can't stop on a prompt.
# "bypassPermissions" lets the skill's own tool calls run unattended. This is
# acceptable ONLY because the service is internal-only + token-gated; tighten
# with AGENT_PERMISSION_MODE=acceptEdits if you add a permission callback.
PERMISSION_MODE = os.environ.get("AGENT_PERMISSION_MODE", "bypassPermissions")

# Optional shared secret. If set, every /run and /chat call must send a matching
# X-Agent-Token header. Leave unset only when the network is fully trusted.
AGENT_TOKEN = os.environ.get("AGENT_TOKEN")

# --- Website assistant (/chat) config ---------------------------------------
# The read-only Q&A assistant is a *different* agent profile from the
# skill-runner: it has no Bash/filesystem, only the SigRepo MCP query tools, and
# is gated to admin accounts. These point it at the internal MCP server and the
# Plumber API (both on db-net) -- override in compose if names/ports differ.
AGENT_MCP_URL = os.environ.get("AGENT_MCP_URL", "http://sigrepo-mcp:8021/")
SIGREPO_API_URL = os.environ.get("SIGREPO_API_URL", "http://sigrepo-api:3838").rstrip("/")

# The 9 tools sigrepo-mcp exposes. The SDK namespaces them as
# mcp__<server>__<tool>; chat mode allows ONLY these.
MCP_SERVER_NAME = "sigrepo"
SIGREPO_TOOLS = [
    "list_vocabulary",
    "search_signatures",
    "get_signature_context",
    "compare_signatures",
    "search_collections",
    "search_geneset_resources",
    "search_geneset_entries",
    "search_features",
    "run_enrichment",
    "rummagene_enrich",
]
ALLOWED_CHAT_TOOLS = [f"mcp__{MCP_SERVER_NAME}__{t}" for t in SIGREPO_TOOLS]

app = FastAPI(title="SigRepo skill-runner", version="0.1.0")


# --------------------------------------------------------------------------- #
# Skill discovery (filesystem only -- works without the SDK installed)
# --------------------------------------------------------------------------- #

def _read_frontmatter(path: Path) -> dict:
    """Parse the YAML frontmatter block at the top of a SKILL.md file."""
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return {}
    if not text.startswith("---"):
        return {}
    parts = text.split("---", 2)
    if len(parts) < 3:
        return {}
    try:
        data = yaml.safe_load(parts[1]) or {}
    except yaml.YAMLError:
        return {}
    return data if isinstance(data, dict) else {}


def discover_skills(cwd: str) -> list[dict]:
    """List skills under <cwd>/.claude/skills, newest SKILL.md convention."""
    base = Path(cwd) / ".claude" / "skills"
    out: list[dict] = []
    if not base.is_dir():
        return out
    for skill_md in sorted(base.glob("*/SKILL.md")):
        fm = _read_frontmatter(skill_md)
        out.append(
            {
                "name": fm.get("name") or skill_md.parent.name,
                "description": fm.get("description"),
                "dir": str(skill_md.parent),
            }
        )
    return out


# --------------------------------------------------------------------------- #
# Auth
# --------------------------------------------------------------------------- #

def require_token(x_agent_token: Optional[str] = Header(default=None)) -> None:
    if AGENT_TOKEN and x_agent_token != AGENT_TOKEN:
        raise HTTPException(status_code=401, detail="Invalid or missing X-Agent-Token.")


# --------------------------------------------------------------------------- #
# Message serialization (SDK message objects -> small JSON dicts for SSE)
# --------------------------------------------------------------------------- #

def _redact(value):
    """Mask secret-ish keys (api_key, token) before streaming to the browser."""
    if isinstance(value, dict):
        return {
            k: ("***" if k in ("api_key", "apikey", "token") else _redact(v))
            for k, v in value.items()
        }
    if isinstance(value, list):
        return [_redact(v) for v in value]
    return value


def _block_to_dict(block) -> dict:
    name = type(block).__name__
    if hasattr(block, "text"):
        return {"type": "text", "text": block.text}
    if hasattr(block, "name") and hasattr(block, "input"):  # ToolUseBlock
        return {
            "type": "tool_use",
            "id": getattr(block, "id", None),
            "name": getattr(block, "name", None),
            "input": _redact(getattr(block, "input", None)),
        }
    if name == "ToolResultBlock" or hasattr(block, "tool_use_id"):
        return {
            "type": "tool_result",
            "tool_use_id": getattr(block, "tool_use_id", None),
            "is_error": getattr(block, "is_error", None),
            "content": getattr(block, "content", None),
        }
    return {"type": name}


def message_to_dict(msg) -> dict:
    name = type(msg).__name__
    content = getattr(msg, "content", None)
    if isinstance(content, list):
        return {"type": name, "content": [_block_to_dict(b) for b in content]}
    if name == "ResultMessage":
        return {
            "type": "result",
            "result": getattr(msg, "result", None),
            "is_error": getattr(msg, "is_error", None),
            "num_turns": getattr(msg, "num_turns", None),
            "duration_ms": getattr(msg, "duration_ms", None),
            "total_cost_usd": getattr(msg, "total_cost_usd", None),
            "session_id": getattr(msg, "session_id", None),
        }
    return {"type": name, "raw": str(msg)}


def _sse(obj: dict) -> str:
    return f"data: {json.dumps(obj, default=str)}\n\n"


# --------------------------------------------------------------------------- #
# Routes
# --------------------------------------------------------------------------- #

@app.get("/")
def root() -> dict:
    return {
        "service": "sigrepo-skill-runner",
        "version": app.version,
        "skills_cwd": SKILLS_CWD,
        "endpoints": ["/health", "/skills", "/run"],
    }


@app.get("/health")
def health() -> dict:
    try:
        import claude_agent_sdk  # noqa: F401

        sdk_available = True
    except Exception:
        sdk_available = False
    return {
        "status": "ok",
        "sdk_available": sdk_available,
        "cli_available": shutil.which("claude") is not None,
        "api_key_set": bool(os.environ.get("ANTHROPIC_API_KEY")),
        "skills_cwd": SKILLS_CWD,
        "skills_found": len(discover_skills(SKILLS_CWD)),
        "permission_mode": PERMISSION_MODE,
        "token_required": bool(AGENT_TOKEN),
        "mcp_url": AGENT_MCP_URL,
        "api_url": SIGREPO_API_URL,
    }


@app.get("/skills")
def skills(cwd: Optional[str] = None) -> dict:
    target = cwd or SKILLS_CWD
    return {"cwd": target, "skills": discover_skills(target)}


class RunRequest(BaseModel):
    prompt: str
    # "all", a list of exact skill names, or null to fall back to AGENT_SKILLS.
    skills: Optional[Union[List[str], str]] = None
    cwd: Optional[str] = None
    system_prompt: Optional[str] = None
    model: Optional[str] = None
    permission_mode: Optional[str] = None


@app.post("/run")
async def run(req: RunRequest, _: None = Depends(require_token)) -> StreamingResponse:
    """Run one prompt with skills enabled, streaming the agent's messages as SSE.

    The response is Server-Sent Events (``text/event-stream``); each ``data:``
    line is a JSON object -- assistant text, tool_use/tool_result, a final
    ``result``, plus our own ``start`` / ``error`` / ``done`` envelopes. SSE is
    used because a skill run (read a paper, run R, build an OmicSignature) can
    take minutes and would blow a plain request timeout.
    """
    try:
        from claude_agent_sdk import ClaudeAgentOptions, query
    except Exception as exc:  # noqa: BLE001 -- report a clean 500 to the caller
        raise HTTPException(status_code=500, detail=f"claude-agent-sdk unavailable: {exc}")

    cwd = req.cwd or SKILLS_CWD
    skills_opt = req.skills if req.skills is not None else DEFAULT_SKILLS

    option_kwargs = dict(
        cwd=cwd,
        # Load skills (and settings) from <cwd>/.claude only -- deterministic,
        # ignores unrelated user-level config on the box.
        setting_sources=["project", "local"],
        skills=skills_opt,
        permission_mode=req.permission_mode or PERMISSION_MODE,
        system_prompt=req.system_prompt or {"type": "preset", "preset": "claude_code"},
    )
    if req.model:
        option_kwargs["model"] = req.model
    options = ClaudeAgentOptions(**option_kwargs)

    async def event_stream():
        yield _sse({"type": "start", "cwd": cwd, "skills": skills_opt})
        try:
            async for message in query(prompt=req.prompt, options=options):
                yield _sse(message_to_dict(message))
        except Exception as exc:  # noqa: BLE001 -- surface to the SSE client
            yield _sse({"type": "error", "error": str(exc)})
        yield _sse({"type": "done"})

    return StreamingResponse(
        event_stream(),
        media_type="text/event-stream",
        headers={"Cache-Control": "no-cache", "X-Accel-Buffering": "no"},
    )


# --------------------------------------------------------------------------- #
# /chat -- the website Q&A assistant (read-only, MCP-grounded, admin-gated)
# --------------------------------------------------------------------------- #

async def _require_admin(api_key: str) -> dict:
    """Re-validate the api_key server-side via the SigRepo API and require the
    admin role. We never trust the browser's own role claim -- a user could
    forge it. Returns the {user_name, user_role} on success."""
    if not api_key:
        raise HTTPException(status_code=401, detail="Missing api_key.")
    try:
        async with httpx.AsyncClient(timeout=15) as client:
            resp = await client.post(f"{SIGREPO_API_URL}/whoami", json={"api_key": api_key})
    except Exception as exc:  # noqa: BLE001 -- API unreachable
        raise HTTPException(status_code=502, detail=f"Could not reach SigRepo API to validate api_key: {exc}")
    if resp.status_code != 200:
        raise HTTPException(status_code=401, detail="Invalid api_key.")
    data = resp.json()
    if str(data.get("user_role", "")).lower() != "admin":
        raise HTTPException(status_code=403, detail="The assistant is currently limited to admin accounts.")
    return data


def _chat_system_prompt(api_key: str) -> str:
    tool_list = ", ".join(SIGREPO_TOOLS)
    return (
        "You are the SigRepo assistant, embedded in the SigRepo website. SigRepo is a "
        "repository of omics signatures (differential-expression gene/feature sets with "
        "metadata) and collections. Answer the user's questions by calling the SigRepo "
        f"MCP tools ({tool_list}). "
        f'Every SigRepo tool call MUST include api_key="{api_key}". '
        "Ground every answer in what those tools return -- if the tools don't cover a "
        "question, say so plainly rather than guessing. A good default when the user is "
        "vague is to call list_vocabulary first to see what data exists, then "
        "search_signatures. You cannot read or write files or run shell commands; do not "
        "try. Keep answers concise and cite signature names / hashkeys you used."
    )


class ChatRequest(BaseModel):
    prompt: str
    api_key: str
    # Pass back the session_id from the previous turn's `result` event to keep
    # the conversation going (the SDK resumes that session).
    session_id: Optional[str] = None
    model: Optional[str] = None


@app.post("/chat")
async def chat(req: ChatRequest, _: None = Depends(require_token)) -> StreamingResponse:
    """Website Q&A assistant. Admin-gated, grounded in the SigRepo MCP tools, with
    no filesystem/Bash access. Streams SSE like /run."""
    await _require_admin(req.api_key)

    try:
        from claude_agent_sdk import (  # noqa: PLC0415
            ClaudeAgentOptions,
            PermissionResultAllow,
            PermissionResultDeny,
            query,
        )
    except Exception as exc:  # noqa: BLE001
        raise HTTPException(status_code=500, detail=f"claude-agent-sdk unavailable: {exc}")

    allowed_prefix = f"mcp__{MCP_SERVER_NAME}__"

    async def _permit(tool_name, input_data, context):  # noqa: ANN001
        # Deny-by-default: only the SigRepo MCP query tools are ever allowed in
        # chat mode. This is the hard stop that keeps a website prompt from
        # reaching Bash/Write/Edit even if the model tries.
        if tool_name.startswith(allowed_prefix):
            return PermissionResultAllow(updated_input=input_data)
        return PermissionResultDeny(message="Chat mode only permits SigRepo query tools.")

    option_kwargs = dict(
        system_prompt=_chat_system_prompt(req.api_key),
        mcp_servers={MCP_SERVER_NAME: {"type": "http", "url": AGENT_MCP_URL}},
        allowed_tools=ALLOWED_CHAT_TOOLS,
        disallowed_tools=["Bash", "Write", "Edit", "NotebookEdit"],
        can_use_tool=_permit,
        permission_mode="default",
        setting_sources=[],        # no skills/project settings needed for chat
        strict_mcp_config=True,    # only our MCP server; ignore any .mcp.json
    )
    if req.session_id:
        option_kwargs["resume"] = req.session_id
    if req.model:
        option_kwargs["model"] = req.model
    options = ClaudeAgentOptions(**option_kwargs)

    async def event_stream():
        yield _sse({"type": "start", "mode": "chat", "session_id": req.session_id})
        try:
            async for message in query(prompt=req.prompt, options=options):
                yield _sse(message_to_dict(message))
        except Exception as exc:  # noqa: BLE001
            yield _sse({"type": "error", "error": str(exc)})
        yield _sse({"type": "done"})

    return StreamingResponse(
        event_stream(),
        media_type="text/event-stream",
        headers={"Cache-Control": "no-cache", "X-Accel-Buffering": "no"},
    )
