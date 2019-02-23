import { List } from "immutable";

/**
 * A circular buffer that overwrites the oldest item when its capacity is reached.
 */
export default class CircularBuffer {
  constructor(capacity) {
    if (capacity < 1) {
      throw new Error("Invalid argument: capacity; must ≥1")
    }
    this.len = 0 // tracks current size
    this.pos = 0 // points to most recent value
    this.capacity = capacity
    this.buffer = new Array(capacity)
    this.snap = List()
  }

  get length() {
    return this.len
  }

  isFull() {
    return this.length === this.capacity
  }

  push = item => {
    const write = (this.pos + 1) % this.capacity
    this.len = Math.max(this.len, Math.min(this.capacity, this.pos + 1))
    this.buffer[write] = item
    this.pos = write
    return this;
  }

  forEach = fn => {
  	let k = Math.abs((this.pos - this.len + 1) % this.capacity)
    for (let i = 0; i < this.len; i++) {
      fn(this.buffer[k], i)
      k = (k + 1) % this.capacity
    }
  }

  /**
   * Returns a snapshot of the values in this buffer. If nothing has changed, returns the same reference
   * as the last call.
   */
  snapshot = () => {
    this.snap = this.snap.withMutations(xs => {
      this.forEach((x, i) => { xs = xs.set(i, x) });
    })
    return this.snap;
  }
}