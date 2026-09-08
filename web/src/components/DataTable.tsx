import { useEffect, useMemo, useRef, useState, type ReactNode } from "react";
import { ChevronDown, ChevronUp, ChevronsUpDown, Filter, Search } from "lucide-react";

const PAGE_SIZE_OPTIONS = [10, 25, 50, 100];

export interface Column<T> {
  key: keyof T;
  label: string;
  align?: "left" | "right";
  render?: (row: T) => ReactNode;
  // Opt-in per column: adds a filter icon next to the header that opens a
  // checkbox dropdown of every distinct value present in `rows` for that
  // column. Best for low-cardinality categorical fields (assay type,
  // organism, visibility) -- not enforced, just a UX suggestion.
  filterable?: boolean;
}

export default function DataTable<T extends object>({
  columns,
  rows,
  rowKey,
  selectedKey,
  onSelectRow,
  pageSize = 10,
  emptyLabel = "No rows to display",
  scrollable = false,
  maxHeight = 420,
  selectable = false,
  selectedKeys,
  onToggleRow,
  onToggleAll,
  searchable = true,
  serverPagination,
  serverSort,
}: {
  columns: Column<T>[];
  rows: T[];
  rowKey: keyof T;
  selectedKey?: string | null;
  onSelectRow?: (row: T) => void;
  pageSize?: number;
  emptyLabel?: string;
  // Renders every sorted/filtered row inside a fixed-height, vertically
  // scrolling body (sticky header) instead of paginating -- for tables that
  // may hold hundreds of rows (e.g. picking from every signature).
  scrollable?: boolean;
  maxHeight?: number;
  // Adds a checkbox column for choosing more than one row at once,
  // independent of (and combinable with) the single-row selectedKey/
  // onSelectRow highlight.
  selectable?: boolean;
  selectedKeys?: Set<string>;
  onToggleRow?: (row: T) => void;
  onToggleAll?: (rows: T[], checked: boolean) => void;
  // Global "Search:" box (DT-style) that matches against every column's
  // rendered value. Works in both paginated and scrollable modes; pass
  // searchable={false} on pages that already provide their own search box.
  searchable?: boolean;
  // Server-side pagination (DT `server = TRUE`): the parent fetches one page
  // at a time and passes it in as `rows`, plus the total match count. The
  // table then renders pager controls from `total` and calls onPageChange
  // instead of slicing rows itself. Client-side sort/column-filters are
  // suppressed in this mode since they can't span pages the table hasn't
  // loaded; drive those through the parent's query instead.
  serverPagination?: {
    page: number;
    pageSize: number;
    total: number;
    onPageChange: (page: number) => void;
  };
  // Supply alongside serverPagination to restore sortable headers. Without it,
  // headers stay inert under server pagination -- sorting one loaded page would
  // look like sorting the whole result set, which is worse than not offering it.
  serverSort?: {
    sortBy: string | null;
    sortDir: "asc" | "desc";
    onSortChange: (key: string, dir: "asc" | "desc") => void;
  };
}) {
  const [sortKey, setSortKey] = useState<keyof T | null>(null);
  const [sortAsc, setSortAsc] = useState(true);
  const [page, setPage] = useState(0);
  const [pageSizeState, setPageSizeState] = useState(pageSize);
  const [search, setSearch] = useState("");
  const [filters, setFilters] = useState<Record<string, Set<string>>>({});
  const [openFilterKey, setOpenFilterKey] = useState<string | null>(null);
  const popoverContainerRef = useRef<HTMLSpanElement | null>(null);
  const showSearch = searchable;

  useEffect(() => {
    if (!openFilterKey) return;
    function handleOutsideClick(e: MouseEvent) {
      if (popoverContainerRef.current && !popoverContainerRef.current.contains(e.target as Node)) {
        setOpenFilterKey(null);
      }
    }
    document.addEventListener("mousedown", handleOutsideClick);
    return () => document.removeEventListener("mousedown", handleOutsideClick);
  }, [openFilterKey]);

  // Distinct values available per filterable column, drawn from the full
  // (unfiltered) row set -- so a column's own filter always offers every
  // option, independent of what other columns are currently filtering out.
  const filterOptions = useMemo(() => {
    const map: Record<string, string[]> = {};
    for (const col of columns) {
      if (!col.filterable) continue;
      const key = String(col.key);
      const values = new Set<string>();
      for (const row of rows) {
        const v = row[col.key];
        if (v !== null && v !== undefined && v !== "") values.add(String(v));
      }
      map[key] = Array.from(values).sort((a, b) => a.localeCompare(b));
    }
    return map;
  }, [columns, rows]);

  const filtered = useMemo(() => {
    const activeKeys = Object.keys(filters).filter((k) => filters[k].size > 0);
    let result = rows;
    if (activeKeys.length > 0) {
      result = result.filter((row) =>
        activeKeys.every((key) => {
          const col = columns.find((c) => String(c.key) === key);
          if (!col) return true;
          return filters[key].has(String(row[col.key] ?? ""));
        })
      );
    }
    const needle = search.trim().toLowerCase();
    if (showSearch && needle) {
      result = result.filter((row) =>
        columns.some((col) => String(row[col.key] ?? "").toLowerCase().includes(needle))
      );
    }
    return result;
  }, [rows, columns, filters, search, showSearch]);

  const sorted = [...filtered].sort((a, b) => {
    if (!sortKey) return 0;
    const av = a[sortKey];
    const bv = b[sortKey];
    if (typeof av === "number" && typeof bv === "number") return sortAsc ? av - bv : bv - av;
    return sortAsc ? String(av ?? "").localeCompare(String(bv ?? "")) : String(bv ?? "").localeCompare(String(av ?? ""));
  });

  // Server mode: `rows` is already the current page from the parent; the table
  // must not sort/filter/slice it (those can't span pages it hasn't loaded).
  const server = !!serverPagination;

  const effectivePageSize = server ? serverPagination!.pageSize : scrollable ? sorted.length || 1 : pageSizeState;
  const pageCount = server
    ? Math.max(1, Math.ceil(serverPagination!.total / serverPagination!.pageSize))
    : Math.max(1, Math.ceil(sorted.length / effectivePageSize));
  const safePage = server ? serverPagination!.page : Math.min(page, pageCount - 1);
  const paged = server ? rows : scrollable ? sorted : sorted.slice(safePage * effectivePageSize, safePage * effectivePageSize + effectivePageSize);
  const goToPage = server ? (p: number) => serverPagination!.onPageChange(p) : (p: number) => setPage(p);

  const allVisibleSelected = selectable && paged.length > 0 && paged.every((row) => selectedKeys?.has(String(row[rowKey])));

  // Windowed page numbers around the current page (DT-style: first, last,
  // and a small run around safePage, with "…" gaps elsewhere).
  const pageNumbers = useMemo(() => {
    const out: (number | "ellipsis")[] = [];
    const last = pageCount - 1;
    const window = 1;
    for (let i = 0; i <= last; i++) {
      if (i === 0 || i === last || Math.abs(i - safePage) <= window) {
        out.push(i);
      } else if (out[out.length - 1] !== "ellipsis") {
        out.push("ellipsis");
      }
    }
    return out;
  }, [pageCount, safePage]);

  function toggleSort(key: keyof T) {
    if (sortKey === key) setSortAsc((a) => !a);
    else {
      setSortKey(key);
      setSortAsc(true);
    }
  }

  function toggleFilterValue(key: string, value: string) {
    setFilters((prev) => {
      const next = new Set(prev[key] ?? []);
      if (next.has(value)) next.delete(value);
      else next.add(value);
      return { ...prev, [key]: next };
    });
    setPage(0);
  }

  function clearFilter(key: string) {
    setFilters((prev) => {
      const next = { ...prev };
      delete next[key];
      return next;
    });
    setPage(0);
  }

  return (
    <div className="dt">
      {showSearch && (
        <div className={"dt-toolbar" + (scrollable ? " dt-toolbar-search-only" : "")}>
          {!scrollable && (
            <label className="dt-pagesize">
              Show
              <select
                value={pageSizeState}
                onChange={(e) => {
                  setPageSizeState(Number(e.target.value));
                  setPage(0);
                }}
              >
                {PAGE_SIZE_OPTIONS.map((n) => (
                  <option key={n} value={n}>
                    {n}
                  </option>
                ))}
              </select>
              entries
            </label>
          )}
          <label className="dt-search">
            <Search size={14} className="dt-search-icon" />
            <input
              type="text"
              placeholder="Search…"
              value={search}
              onChange={(e) => {
                setSearch(e.target.value);
                setPage(0);
              }}
            />
          </label>
        </div>
      )}
      <div className={"dt-scroll" + (scrollable ? " dt-scroll-bounded" : "")} style={scrollable ? { maxHeight } : undefined}>
        <table className="dt-table">
          <thead>
            <tr>
              {selectable && (
                <th className="dt-check-col">
                  <input
                    type="checkbox"
                    checked={allVisibleSelected}
                    onChange={(e) => onToggleAll?.(paged, e.target.checked)}
                    aria-label="Select all visible rows"
                  />
                </th>
              )}
              {columns.map((col) => {
                const key = String(col.key);
                const activeCount = filters[key]?.size ?? 0;
                return (
                  <th key={key} className={(col.align === "right" ? "dt-right" : "") + (server && !serverSort ? " dt-th-nosort" : "")}>
                    <span className="dt-th">
                      {server && !serverSort ? (
                        <span className="dt-th-static">{col.label}</span>
                      ) : server && serverSort ? (
                        // Sorting delegated to the server, which is what makes it
                        // meaningful across every page rather than within one.
                        <span
                          className="dt-th-sort"
                          onClick={() =>
                            serverSort.onSortChange(
                              String(col.key),
                              serverSort.sortBy === String(col.key) && serverSort.sortDir === "asc" ? "desc" : "asc"
                            )
                          }
                        >
                          {col.label}
                          {serverSort.sortBy === String(col.key) ? (
                            serverSort.sortDir === "asc" ? <ChevronUp size={13} /> : <ChevronDown size={13} />
                          ) : (
                            <ChevronsUpDown size={13} className="dt-sort-idle" />
                          )}
                        </span>
                      ) : (
                        <span className="dt-th-sort" onClick={() => toggleSort(col.key)}>
                          {col.label}
                          {sortKey === col.key ? (
                            sortAsc ? (
                              <ChevronUp size={13} />
                            ) : (
                              <ChevronDown size={13} />
                            )
                          ) : (
                            <ChevronsUpDown size={13} className="dt-sort-idle" />
                          )}
                        </span>
                      )}
                      {col.filterable && !server && (
                        <span
                          className="dt-filter-wrap"
                          ref={openFilterKey === key ? popoverContainerRef : undefined}
                          onClick={(e) => e.stopPropagation()}
                        >
                          <button
                            type="button"
                            className={"dt-filter-btn" + (activeCount > 0 ? " dt-filter-btn-active" : "")}
                            onClick={() => setOpenFilterKey(openFilterKey === key ? null : key)}
                            title={`Filter ${col.label}`}
                          >
                            <Filter size={12} />
                          </button>
                          {openFilterKey === key && (
                            <div className="dt-filter-popover">
                              <div className="dt-filter-popover-head">
                                <span>{col.label}</span>
                                <button type="button" className="dt-filter-clear" onClick={() => clearFilter(key)}>
                                  Clear
                                </button>
                              </div>
                              <div className="dt-filter-options">
                                {(filterOptions[key] ?? []).map((opt) => (
                                  <label key={opt} className="dt-filter-option">
                                    <input
                                      type="checkbox"
                                      checked={filters[key]?.has(opt) ?? false}
                                      onChange={() => toggleFilterValue(key, opt)}
                                    />
                                    <span>{opt}</span>
                                  </label>
                                ))}
                                {(filterOptions[key] ?? []).length === 0 && (
                                  <span className="dt-filter-empty">No values</span>
                                )}
                              </div>
                            </div>
                          )}
                        </span>
                      )}
                    </span>
                  </th>
                );
              })}
            </tr>
          </thead>
          <tbody>
            {paged.map((row) => {
              const key = String(row[rowKey]);
              const checked = selectedKeys?.has(key) ?? false;
              return (
                <tr
                  key={key}
                  className={
                    (onSelectRow ? "dt-clickable" : "") + (selectedKey === key ? " dt-selected" : "") + (checked ? " dt-selected" : "")
                  }
                  onClick={() => onSelectRow?.(row)}
                >
                  {selectable && (
                    <td className="dt-check-col" onClick={(e) => e.stopPropagation()}>
                      <input type="checkbox" checked={checked} onChange={() => onToggleRow?.(row)} aria-label={`Select row ${key}`} />
                    </td>
                  )}
                  {columns.map((col) => {
                    // Cells are clipped to one line so rows stay a uniform
                    // height, so anything too long to fit needs to remain
                    // readable on hover. Taken from the raw value rather than
                    // the rendered node, which may be a badge or an element.
                    const raw = row[col.key];
                    const title =
                      raw == null || typeof raw === "object" ? undefined : String(raw);
                    return (
                      <td
                        key={String(col.key)}
                        className={col.align === "right" ? "dt-right" : ""}
                        title={title}
                      >
                        {col.render ? col.render(row) : String(raw ?? "")}
                      </td>
                    );
                  })}
                </tr>
              );
            })}
            {paged.length === 0 && (
              <tr>
                <td colSpan={columns.length + (selectable ? 1 : 0)} className="dt-empty">
                  {emptyLabel}
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {server ? (
        <div className="dt-foot">
          <span className="dt-count">
            {serverPagination!.total === 0
              ? "0 entries"
              : `Showing ${safePage * effectivePageSize + 1} to ${safePage * effectivePageSize + paged.length} of ${serverPagination!.total} entries`}
          </span>
          {pageCount > 1 && (
            <div className="dt-pager">
              <button className="dt-page-btn" disabled={safePage === 0} onClick={() => goToPage(safePage - 1)}>
                Previous
              </button>
              {pageNumbers.map((n, i) =>
                n === "ellipsis" ? (
                  <span key={`e${i}`} className="dt-page-ellipsis">
                    …
                  </span>
                ) : (
                  <button
                    key={n}
                    className={"dt-page-btn dt-page-num" + (n === safePage ? " dt-page-num-active" : "")}
                    onClick={() => goToPage(n)}
                  >
                    {n + 1}
                  </button>
                )
              )}
              <button className="dt-page-btn" disabled={safePage >= pageCount - 1} onClick={() => goToPage(safePage + 1)}>
                Next
              </button>
            </div>
          )}
        </div>
      ) : scrollable ? (
        <div className="dt-foot">
          <span className="dt-count">
            {sorted.length} {sorted.length === 1 ? "entry" : "entries"}
            {sorted.length !== rows.length ? ` (filtered from ${rows.length} total)` : ""}
          </span>
        </div>
      ) : (
        <div className="dt-foot">
          <span className="dt-count">
            {sorted.length === 0
              ? "0 entries"
              : `Showing ${safePage * effectivePageSize + 1} to ${Math.min(sorted.length, safePage * effectivePageSize + effectivePageSize)} of ${sorted.length} entries`}
            {sorted.length !== rows.length ? ` (filtered from ${rows.length} total)` : ""}
          </span>
          {pageCount > 1 && (
            <div className="dt-pager">
              <button className="dt-page-btn" disabled={safePage === 0} onClick={() => setPage(safePage - 1)}>
                Previous
              </button>
              {pageNumbers.map((n, i) =>
                n === "ellipsis" ? (
                  <span key={`e${i}`} className="dt-page-ellipsis">
                    …
                  </span>
                ) : (
                  <button
                    key={n}
                    className={"dt-page-btn dt-page-num" + (n === safePage ? " dt-page-num-active" : "")}
                    onClick={() => setPage(n)}
                  >
                    {n + 1}
                  </button>
                )
              )}
              <button className="dt-page-btn" disabled={safePage >= pageCount - 1} onClick={() => setPage(safePage + 1)}>
                Next
              </button>
            </div>
          )}
        </div>
      )}
    </div>
  );
}
