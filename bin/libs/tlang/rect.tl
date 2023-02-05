import "std.tl"

Rect :: struct {
	min: V2s // inclusive
	max: V2s // exclusive
}

rect :: (minx: S32, miny: S32, maxx: S32, maxy: S32) => Rect(V2s(minx, miny), V2s(maxx, maxy))
rect :: (min: V2s, max: V2s) => Rect(min, max)
rect :: (min: V2s, size: V2s) => Rect(min, min+size)

size :: (using this: Rect) => max - min

extend :: (using this: *Rect, extent: V2s) => Rect(min - extent, max + extent)

in_bounds :: (using this: V2s, rect: Rect) =>
    rect.min.x <= x && x < rect.max.x &&
    rect.min.y <= y && y < rect.max.y;
