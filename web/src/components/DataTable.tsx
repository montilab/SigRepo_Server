import { useEffect, useMemo, useRef, useState, type ReactNode } from "react";
import { ChevronDown, ChevronUp, ChevronsUpDown, Filter } from "lucide-react";

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
  pageSize = 8,
  emptyLabel = "No rows to display",
  scrollable = false,
  maxHeight = 420,
  selectable = false,
  selectedKeys,
  onToggleRow,
  onToggleAll,
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
}) {
  const [sortKey, setSortKey] = useState<keyof T | null>(null);
  const [sortAsc, setSortAsc] = useState(true);
  const [page, setPage] = useState(0);
  const [filters, setFilters] = useState<Record<string, Set<string>>>({});
  const [openFilterKey, setOpenFilterKey] = useState<string | null>(null);
  const popoverContainerRef = useRef<HTMLSpanElement | null>(null);

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
    if (activeKeys.length === 0) return rows;
    return rows.filter((row) =>
      activeKeys.every((key) => {
        const col = columns.find((c) => String(c.key) === key);
        if (!col) return true;
        return filters[key].has(String(row[col.key] ?? ""));
      })
    );
  }, [rows, columns, filters]);

  const sorted = [...filtered].sort((a, b) => {
    if (!sortKey) return 0;
    const av = a[sortKey];
    const bv = b[sortKey];
    if (typeof av === "number" && typeof bv === "number") return sortAsc ? av - bv : bv - av;
    return sortAsc ? String(av ?? "").localeCompare(String(bv ?? "")) : String(bv ?? "").localeCompare(String(av ?? ""));
  });

  const pageCount = Math.max(1, Math.ceil(sorted.length / pageSize));
  const safePage = Math.min(page, pageCount - 1);
  const paged = scrollable ? sorted : sorted.slice(safePage * pageSize, safePage * pageSize + pageSize);

  const allVisibleSelected = selectable && paged.length > 0 && paged.every((row) => selectedKeys?.has(String(row[rowKey])));

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
                  <th key={key} className={col.align === "right" ? "dt-right" : ""}>
                    <span className="dt-th">
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
                      {col.filterable && (
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
                  {columns.map((col) => (
                    <td key={String(col.key)} className={col.align === "right" ? "dt-right" : ""}>
                      {col.render ? col.render(row) : String(row[col.key] ?? "")}
                    </td>
                  ))}
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
      {!scrollable && sorted.length > pageSize && (
        <div className="dt-foot">
          <span className="dt-count">
            {safePage * pageSize + 1}–{Math.min(sorted.length, safePage * pageSize + pageSize)} of {sorted.length}
          </span>
          <div className="dt-pager">
            <button className="btn btn-ghost btn-sm" disabled={safePage === 0} onClick={() => setPage(safePage - 1)}>
              Previous
            </button>
            <button
              className="btn btn-ghost btn-sm"
              disabled={safePage >= pageCount - 1}
              onClick={() => setPage(safePage + 1)}
            >
              Next
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
