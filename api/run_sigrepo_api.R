
# Load R packages
library(plumber)

# Get package path
api_path <- base::file.path(base::Sys.getenv("SIGREPO_SERVER_DIR"), "api")

# /annotate/run hands its multi-second hypeR work to a worker through
# promises::future_promise(), so the main process keeps answering other
# requests while an enrichment runs. This is where that worker pool is set up.
# Without a plan, future falls back to running inline -- correct, but blocking,
# which is exactly the behaviour this replaces.
#
# multicore (fork) is preferred where the platform allows it: the child shares
# the parent's loaded packages and the gene set tables by copy-on-write, so it
# costs only the enrichment's own working memory and ships nothing over a
# socket. multisession is the fallback (Windows, RStudio): a fresh R process
# per worker that must reload every package. SIGREPO_ASYNC_PLAN forces one.
#
# SIGREPO_ASYNC_WORKERS bounds how many enrichments run at once, and defaults
# to 1 on purpose: a single worker already gives the guarantee that matters
# (browsing never waits behind an enrichment) while keeping peak memory the
# same as running them inline. Raise it only with the RAM to match -- each
# concurrent C5 run is on the order of 1 GB.
async_workers <- base::max(1L, base::as.integer(base::Sys.getenv("SIGREPO_ASYNC_WORKERS", "1")))
async_plan <- base::Sys.getenv("SIGREPO_ASYNC_PLAN", "")
if (!async_plan %in% base::c("multicore", "multisession")) {
  async_plan <- if (future::supportsMulticore()) "multicore" else "multisession"
}
# plan() substitutes its first argument rather than evaluating it, so it has to
# be handed a plain value -- the strategy's name -- not an expression that
# would produce one. A computed call here fails inside tweak() at startup.
# I() matters: future treats a plain `workers = 1` as "no parallelism" and
# quietly runs every future inline in this process -- which is precisely the
# blocking behaviour being replaced, with nothing in the logs to say so. I()
# is future's documented way of insisting on one real worker.
future::plan(async_plan, workers = base::I(async_workers))

# Prove it, rather than trust it: run a trivial future and check it came back
# from a different process. The failure mode here is silent (the API starts,
# answers, and blocks on every enrichment exactly as before), so make it loud.
async_worker_pid <- future::value(future::future(base::Sys.getpid()))
async_dispatches <- !base::identical(async_worker_pid, base::Sys.getpid())
base::message(base::sprintf("async enrichment: plan=%s workers=%d dispatches_to_worker=%s",
                            async_plan, async_workers, async_dispatches))
if (!async_dispatches) {
  base::warning("async enrichment is running INLINE: enrichments will block the API for every caller",
                call. = FALSE, immediate. = TRUE)
}

# Start a Plumber API instance
api <- plumber::plumb(file = base::file.path(api_path, "api.R"))

# Deploy Plumber API on localhost at port 3838
api$run(host = "0.0.0.0", port = 3838)


