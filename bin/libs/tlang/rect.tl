import "std.tl"

/*
Rect :: struct (T: Type) {
	min: T // inclusive
	max: T // exclusive
}

rect :: (minx, miny, maxx, maxy: $T) => Rect(Vector2(T))(Vector2(T)(minx, miny), Vector2(T)(maxx, maxy))
rect :: (min, max: Vector($T)) => Rect(Vector2(T))(min, max)
rect :: (min, size: Vector($T)) => Rect(Vector2(T))(min, min+size)

size :: (using this: Rect) => max - min

extend :: (using this: *Rect, extent: this.T) => Rect(min - extent, max + extent)

in_bounds :: (using this: $T, rect: Rect(T)) =>
    rect.min.x <= x && x < rect.max.x &&
    rect.min.y <= y && y < rect.max.y;
*/
