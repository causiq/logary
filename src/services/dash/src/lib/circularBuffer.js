import { OrderedSet } from "immutable";

/**
 * A circular buffer that overwrites the oldest item when its capacity is reached.
 */
export default class CircularBuffer {
  constructor(capacity) {
    if (capacity < 1) {
      throw new Error("Invalid argument: capacity; must ≥1")
    }
    this.len = 0 // tracks current size
    this.pos = 0 // points to value to be written
    this.capacity = capacity
    this.buffer = new Array(capacity)
    this.snap = OrderedSet()
  }

  get length() {
    return this.len
  }

  isFull() {
    return this.length === this.capacity
  }

  push = item => {
    this.len = Math.max(this.len, Math.min(this.capacity, this.pos + 1))
    this.buffer[this.pos] = item
    this.pos = (this.pos + 1) % this.capacity
    //console.log('after push len=', this.len, 'pos=', this.pos)
    return this;
  }

  forEach = fn => {
    // cap = 3, written 1 items; len = 1, pos = 1, k = 0
    // cap = 3, written 2 items; len = 2, pos = 2, k = 0
    // cap = 3, written 3 items; len = 3, pos = 0, k = 0
    // cap = 3, written 4 items; len = 3, pos = 1, k = pos
    // cap = 3, written 5 items; len = 3, pos = 2, k = pos
    const kI = this.len === this.capacity ? this.pos : 0
    for (let i = 0, k = kI; i < this.len; i++, k = (k + 1) % this.capacity) {
      fn(this.buffer[k], i, k)
    }
    return kI
  }

  /**
   * Returns a snapshot of the values in this buffer. If nothing has changed, returns the same reference
   * as the last call.
   */
  snapshot = (sortBy, filter = _ => true) => {
    return this.snap = OrderedSet().withMutations(xs => {
      this.forEach((x, _, k) => {
        xs = filter(x) ? xs.add({ ...x, key: k }) : xs
      });
    }).sort(sortBy)
  }
}