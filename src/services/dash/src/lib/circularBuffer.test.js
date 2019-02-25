import CircularBuffer from "./circularBuffer";
import { List } from "immutable";

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
      const empty = List(),
            sn1 = subject.snapshot()
      expect(sn1.snapshot).toEqual(empty)
      expect(sn1.kI).toEqual(0)
    })

    it("immutable List equality", () => {
      const one = List([ 1 ]), two = List([ 1 ]);
      expect(one).toEqual(two)
      const three = two.push(2);
      expect(one).not.toEqual(three)
    })
  })

  describe("protocol", () => {
    it("invariants", () => {
      expect(subject.snapshot().snapshot === subject.snapshot().snapshot).toBeTruthy()

      subject.push(1);
      expect(subject.length).toEqual(1)
      expect(subject.isFull()).toBeFalsy()
      const sn1 = subject.snapshot()
      expect(sn1.snapshot === subject.snapshot().snapshot).toBeTruthy()
      expect(sn1.snapshot.get(0)).toEqual(1)
      expect(sn1.snapshot.get(1)).toBeUndefined()
      expect(sn1.kI).toEqual(0)

      subject.push(2)
      expect(subject.length).toEqual(2)
      expect(subject.isFull()).toBeTruthy()
      const sn2 = subject.snapshot()
      expect(sn2.snapshot === subject.snapshot().snapshot).toBeTruthy()
      expect(sn2.snapshot.get(0)).toEqual(1)
      expect(sn2.snapshot.get(1)).toEqual(2)
      expect(sn2.kI).toEqual(0)

      subject.push(3)
      expect(subject.length).toEqual(2)
      expect(subject.isFull()).toBeTruthy()
      const sn3 = subject.snapshot()
      expect(sn3.snapshot === subject.snapshot().snapshot).toBeTruthy()
      expect(sn3.snapshot.get(0)).toEqual(2)
      expect(sn3.snapshot.get(1)).toEqual(3)
      expect(sn3.kI).toEqual(1)
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