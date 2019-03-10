import CircularBuffer from "./circularBuffer";
import { OrderedSet } from "immutable";

describe('circular buffer', () => {
  let subject
  beforeEach(() => {
    subject = new CircularBuffer(2);
  })

  it('has push', () => {
    expect(subject).toHaveProperty("push")
  })

  it('has forEach', () => {
    expect(subject).toHaveProperty("forEach")
  })

  it("has snapshot", () => {
    expect(subject).toHaveProperty("snapshot")
  })

  it("has isFull", () => {
    expect(subject).toHaveProperty("isFull")
  })

  describe("snapshot empty", () => {
    it("equals empty", () => {
      const empty = OrderedSet(),
            sn1 = subject.snapshot()
      expect(sn1).toEqual(empty)
    })

    it("immutable OrderedSet equality", () => {
      const one = OrderedSet([ 1 ]), two = OrderedSet([ 1 ]);
      expect(one).toEqual(two)
      const three = two.add(2);
      expect(one).not.toEqual(three)
    })
  })

  describe("protocol", () => {
    it("invariants", () => {
      expect(subject.snapshot().snapshot === subject.snapshot().snapshot).toBeTruthy()

      subject.push(1);
      expect(subject.length).toEqual(1)
      expect(subject.isFull()).toBeFalsy()

      subject.push(2)
      expect(subject.length).toEqual(2)
      expect(subject.isFull()).toBeTruthy()

      subject.push(3)
      expect(subject.length).toEqual(2)
      expect(subject.isFull()).toBeTruthy()
    })

    describe("forEach", () => {
      it("on empty", () => {
        let calls = 0
        const kI = subject.forEach(() => { calls++ })
        expect(calls).toEqual(0)
        expect(kI).toEqual(0)
      })

      it("one item", () => {
        let calls = 0
        subject.push("apa")
        subject.forEach(() => { calls++ })
        expect(calls).toEqual(1)
      })

      it("two items", () => {
        let calls = 0
        subject.push("apa")
        subject.push("pap")
        subject.forEach(() => { calls++ })
        expect(calls).toEqual(2)
      })

      it("wraparound at three items", () => {
        let calls = 0
        subject.push("apa")
        subject.push("pap")
        subject.push("aap")
        subject.forEach(() => { calls++ })
        expect(calls).toEqual(2)
      })
    })

  })
})