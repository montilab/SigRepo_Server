// Shimmer placeholder used in loading states instead of plain "Loading…" text.
export default function Skeleton({
  width,
  height = 14,
  radius,
  style,
}: {
  width?: number | string;
  height?: number | string;
  radius?: number | string;
  style?: React.CSSProperties;
}) {
  return <span className="skeleton" style={{ width, height, borderRadius: radius, ...style }} />;
}

// A block of shimmer "table rows" for tables that are still loading.
export function SkeletonRows({ rows = 6, cols = 4 }: { rows?: number; cols?: number }) {
  return (
    <div>
      {Array.from({ length: rows }).map((_, r) => (
        <div className="skeleton-row" key={r}>
          {Array.from({ length: cols }).map((_, c) => (
            <Skeleton key={c} height={12} width={c === 0 ? "40%" : "16%"} />
          ))}
        </div>
      ))}
    </div>
  );
}
