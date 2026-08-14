# SigRepo skill-runner image.
#
# Built FROM the existing SigRepo image so the agent inherits R + OmicSignature +
# every package a skill like create-omic-signature needs to actually run. On top
# of that we add:
#   * Node + the Claude Code CLI  -- the Claude Agent SDK drives a `claude`
#     subprocess, so the CLI must be on PATH.
#   * A pinned Python 3.11 (via uv) + the runner's deps. We use uv rather than the
#     base image's apt python because claude-agent-sdk requires Python >= 3.10 and
#     we can't assume the base distro ships a new enough interpreter. uv fetches a
#     standalone Python, so the build is independent of the base image's version.
FROM montilab/sigrepo:latest

ENV DEBIAN_FRONTEND=noninteractive

# Node 20 for the Claude Code CLI the Agent SDK drives.
RUN apt-get update \
 && apt-get install -y --no-install-recommends curl ca-certificates gnupg \
 && curl -fsSL https://deb.nodesource.com/setup_20.x | bash - \
 && apt-get install -y --no-install-recommends nodejs \
 && npm install -g @anthropic-ai/claude-code \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# uv: standalone Python + fast installer, independent of the base image's python.
RUN curl -LsSf https://astral.sh/uv/install.sh | sh
ENV PATH="/root/.local/bin:${PATH}"

WORKDIR /app

COPY requirements.txt /app/requirements.txt
RUN uv venv --python 3.11 /app/.venv \
 && uv pip install --python /app/.venv/bin/python -r requirements.txt
# Put the venv (and thus uvicorn / the runner's python) first on PATH.
ENV PATH="/app/.venv/bin:${PATH}"

COPY runner.py /app/runner.py

# Defaults; override in compose. Skills come from the bind-mounted /SigRepo.
ENV AGENT_SKILLS_CWD=/SigRepo \
    AGENT_SKILLS=all \
    AGENT_PERMISSION_MODE=bypassPermissions \
    AGENT_HOST=0.0.0.0 \
    AGENT_PORT=8808

EXPOSE 8808

CMD ["sh", "-c", "uvicorn runner:app --host ${AGENT_HOST} --port ${AGENT_PORT}"]
