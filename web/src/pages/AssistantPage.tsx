import { useRef, useState } from "react";
import { Bot, Send, Wrench, Loader2, User } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import { getAuth, chatStream, ApiError, type ChatEvent } from "../api/client";

interface ChatTurn {
  role: "user" | "assistant";
  text: string;
  tools: string[];
}

const SUGGESTIONS = [
  "What organisms and assay types have data?",
  "Find human breast cancer signatures.",
  "Compare the two most recent signatures.",
  "Run enrichment on a signature you find.",
];

// Strip the mcp__sigrepo__ prefix for a readable tool chip.
function toolLabel(name: string): string {
  return name.replace(/^mcp__[^_]+__/, "");
}

export default function AssistantPage() {
  const auth = getAuth();
  const isAdmin = (auth?.user_role ?? "").toLowerCase() === "admin";

  const [messages, setMessages] = useState<ChatTurn[]>([]);
  const [input, setInput] = useState("");
  const [sending, setSending] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const sessionId = useRef<string | null>(null);
  const scrollRef = useRef<HTMLDivElement>(null);

  // Mutate the in-flight assistant turn (always the last message) as events stream.
  function updateLast(fn: (turn: ChatTurn) => ChatTurn) {
    setMessages((prev) => {
      if (prev.length === 0) return prev;
      const copy = prev.slice();
      copy[copy.length - 1] = fn(copy[copy.length - 1]);
      return copy;
    });
  }

  function onEvent(e: ChatEvent) {
    if (e.type === "result") {
      if (e.session_id) sessionId.current = e.session_id;
      return;
    }
    if (e.type === "error") {
      updateLast((t) => ({ ...t, text: t.text + (t.text ? "\n\n" : "") + `⚠️ ${e.error}` }));
      return;
    }
    for (const block of e.content ?? []) {
      if (block.type === "text" && block.text) {
        updateLast((t) => ({ ...t, text: t.text + block.text }));
      } else if (block.type === "tool_use" && block.name) {
        updateLast((t) => ({ ...t, tools: [...t.tools, toolLabel(block.name!)] }));
      }
    }
    requestAnimationFrame(() => scrollRef.current?.scrollTo({ top: scrollRef.current.scrollHeight }));
  }

  async function send(prompt: string) {
    const trimmed = prompt.trim();
    if (!trimmed || sending || !isAdmin) return;
    setError(null);
    setInput("");
    setMessages((prev) => [
      ...prev,
      { role: "user", text: trimmed, tools: [] },
      { role: "assistant", text: "", tools: [] },
    ]);
    setSending(true);
    try {
      await chatStream(trimmed, { sessionId: sessionId.current, onEvent });
    } catch (err) {
      setError(err instanceof ApiError ? err.message : "The assistant is unavailable.");
      updateLast((t) => ({ ...t, text: t.text || "—" }));
    } finally {
      setSending(false);
    }
  }

  function onKeyDown(e: React.KeyboardEvent<HTMLTextAreaElement>) {
    if (e.key === "Enter" && !e.shiftKey) {
      e.preventDefault();
      void send(input);
    }
  }

  return (
    <div className="page page-assistant">
      <PageHeader
        variant="bar"
        title="Assistant"
        subtitle="Ask about signatures, collections, and enrichment — grounded in SigRepo data."
        icon={<Bot size={18} />}
      />

      {!isAdmin ? (
        <Card>
          <div className="empty-state">
            The assistant is currently limited to admin accounts.
          </div>
        </Card>
      ) : (
        <div className="assistant-wrap">
          <div className="assistant-scroll" ref={scrollRef}>
            {messages.length === 0 ? (
              <div className="assistant-empty">
                <Bot size={30} />
                <p>Ask a question to get started.</p>
                <div className="assistant-suggestions">
                  {SUGGESTIONS.map((s) => (
                    <button key={s} className="assistant-chip" onClick={() => void send(s)}>
                      {s}
                    </button>
                  ))}
                </div>
              </div>
            ) : (
              messages.map((m, i) => (
                <div key={i} className={"chat-row chat-row-" + m.role}>
                  <div className="chat-avatar">
                    {m.role === "user" ? <User size={15} /> : <Bot size={15} />}
                  </div>
                  <div className="chat-bubble">
                    {m.tools.length > 0 && (
                      <div className="chat-tools">
                        {m.tools.map((t, j) => (
                          <span className="chat-tool" key={j}>
                            <Wrench size={11} /> {t}
                          </span>
                        ))}
                      </div>
                    )}
                    {m.text ? (
                      <div className="chat-text">{m.text}</div>
                    ) : (
                      sending &&
                      i === messages.length - 1 && (
                        <div className="chat-thinking">
                          <Loader2 size={14} className="spin" /> thinking…
                        </div>
                      )
                    )}
                  </div>
                </div>
              ))
            )}
          </div>

          {error && <div className="assistant-error">{error}</div>}

          <div className="assistant-input">
            <textarea
              value={input}
              onChange={(e) => setInput(e.target.value)}
              onKeyDown={onKeyDown}
              placeholder="Ask the SigRepo assistant…"
              rows={1}
              disabled={sending}
            />
            <button
              className="btn btn-primary"
              onClick={() => void send(input)}
              disabled={sending || !input.trim()}
            >
              {sending ? <Loader2 size={16} className="spin" /> : <Send size={16} />}
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
