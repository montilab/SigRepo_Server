<br>

# SigRepo_Server

**SigRepo_Server** is the server half of
<a href="https://github.com/montilab/SigRepo" target="_blank"><strong>SigRepo</strong></a>,
a platform for storing, sharing, and comparing omic signatures. It is a
reproducible framework for deploying a SigRepo instance — locally or on
a Linux-based cloud host — and for interacting with it through a REST
API, a web interface, and an MCP server for AI agents.

The client half is the
<a href="https://github.com/montilab/SigRepo" target="_blank"><strong>SigRepo</strong></a>
R package; signatures themselves are represented as R6 objects defined
by
<a href="https://github.com/montilab/OmicSignature" target="_blank"><strong>OmicSignature</strong></a>,
our in-house package.

## Architecture

The server is a set of containerized services:

-   **MySQL database** — initializes and manages the SigRepo schema,
    storing signatures, collections, users, controlled vocabularies
    (organisms, platforms, sample types, assay types), and gene set
    resources. Per-signature differential-expression (*difexp*) tables
    are held in a companion object store.
-   **Plumber REST API** — the single access path to the data. It
    defines the schema, loads reference tables, stores and retrieves
    difexp objects, authenticates every call by `api_key`, and exposes
    the analysis endpoints (signature comparison, enrichment) so that
    analysis runs server-side rather than requiring users to download
    whole signatures.
-   **Web interface** — a React single-page app, built to static assets
    and served by nginx, which also proxies `/api` to the API container so
    the browser talks to a single origin. It replaced the R Shiny
    dashboard as the served UI; the Shiny app is preserved under
    `legacy_app/` and is no longer started by the compose stack.
-   **MCP server** — a
    <a href="https://modelcontextprotocol.io" target="_blank">Model
    Context Protocol</a> endpoint that lets AI agents search signatures,
    retrieve signature context, compare signatures, browse gene sets,
    and run enrichment, grounded in the stored data. It sits behind a
    rate-limiting reverse proxy and is reachable at
    <https://sigrepo.org/mcp/>.

Because analyses are exposed as discrete API endpoints over a common
signature representation, additional analysis engines and external
gene-set resources can be added as new endpoints without changing the
database schema.

## Our deployed instance

Our team runs a SigRepo server on
<a href="https://www.digitalocean.com/" target="_blank"><strong>DigitalOcean</strong></a>.

-   To access the signatures and collections stored in our database,
    <a target="_blank" href="https://sigrepo.org">VISIT OUR WEBSITE</a>
    to create an account or <a href="mailto:sigrepo@bu.edu">CONTACT
    US</a> to be added.
-   <a href="https://montilab.github.io/SigRepo/index.html"
    target="_blank">To connect and interact with our database using the
    "SigRepo" R package</a>
-   <a href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html"
    target="_blank">To install a "SigRepo" database instance on your
    local machine</a>

## Analysis endpoints

Beyond storage and retrieval, the API supports analysis over stored
signatures:

-   **Signature comparison** — any set of signatures can be compared by
    feature overlap (Jaccard index with Fisher exact tests), by
    rank-based Kolmogorov–Smirnov statistics, or by GSEA, returning
    similarity matrices and, for GSEA, per-pair leading-edge data.
    Bi-directional signatures are compared per matched group label.
-   **Gene set enrichment** — over-representation and rank-based
    enrichment against MSigDB via
    <a href="https://github.com/montilab/hypeR" target="_blank">hypeR</a>,
    with MSigDB collections cached server-side.

## In development

Active work, not yet part of the deployed stack:

-   **AI-assisted signature authoring** — an agent service that reads a
    study's differential-expression output and description, proposes
    metadata from SigRepo's controlled vocabularies, and emits a
    validated OmicSignature for the depositor to review, so contributing
    a signature becomes a review step rather than a curation task.
-   **Additional external gene-set resources** for matching stored
    signatures against published gene sets.

## Deployment

See the
<a href="https://montilab.github.io/SigRepo_Server/articles/install_sigrepo.html"
target="_blank">installation guide</a> for standing up your own
instance. The stack is defined with `docker compose`; the MySQL service,
the API, the web app, and the MCP server each run as their own container
on a shared internal network, with only the intended ports published.
