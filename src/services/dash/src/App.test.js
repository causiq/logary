import React from 'react';
import ReactDOM from 'react-dom';
import App, { sortByTS, sortByName, invertSorter, buildSortBy, Sorter } from './App';
import CircularBuffer from './lib/circularBuffer'

it('renders without crashing', () => {
  const div = document.createElement('div');
  ReactDOM.render(<App />, div);
  ReactDOM.unmountComponentAtNode(div);
});

describe('sorting', () => {
  it('can sort by ts', () => {
    expect(sortByTS({ timestamp: 1 }, { timestamp: 2 })).toEqual(-1)
    expect(sortByTS({ timestamp: 2 }, { timestamp: 2 })).toEqual(0)
    expect(sortByTS({ timestamp: 2 }, { timestamp: 1 })).toEqual(1)
  })

  it('can invert sorter', () => {
    const sorter = invertSorter(sortByTS)
    expect(sorter({ timestamp: 1 }, { timestamp: 2 })).toEqual(1)
    expect(sorter({ timestamp: 2 }, { timestamp: 2 })).toEqual(0)
    expect(sorter({ timestamp: 2 }, { timestamp: 1 })).toEqual(-1)
  })

  it('can invert sorter object', () => {
    const s = new Sorter('timestamp', sortByTS, true)
    expect(s.sort({ timestamp: 1 }, { timestamp: 2 })).toEqual(1)
    expect(s.sort({ timestamp: 2 }, { timestamp: 2 })).toEqual(0)
    expect(s.sort({ timestamp: 2 }, { timestamp: 1 })).toEqual(-1)
  })

  describe('builder', () => {
    it('delegates', () => {
      const sorter = buildSortBy([ new Sorter('timestamp', sortByTS) ])
      expect(sorter({ timestamp: 1 }, { timestamp: 2 })).toEqual(-1)
      expect(sorter({ timestamp: 2 }, { timestamp: 2 })).toEqual(0)
      expect(sorter({ timestamp: 2 }, { timestamp: 1 })).toEqual(1)
    })

    it('handles two sorters', () => {
      const sorter = buildSortBy([
        new Sorter('timestamp', sortByTS),
        /* then by */ new Sorter('name', sortByName)
      ])
      expect(sorter({ timestamp: 1 }, { timestamp: 2 })).toEqual(-1)
      expect(sorter({ timestamp: 2, name: 'a' }, { timestamp: 2, name: 'a' })).toEqual(0)
      expect(sorter({ timestamp: 2, name: 'a' }, { timestamp: 2, name: 'b' })).toEqual(-1)
      expect(sorter({ timestamp: 2, name: 'b' }, { timestamp: 2, name: 'a' })).toEqual(1)
      expect(sorter({ timestamp: 2 }, { timestamp: 1 })).toEqual(1)
    })
  })

  describe('calling snapshot with sorter', () => {
    const cb = new CircularBuffer(10)
    cb.push({ timestamp: 1, name: 'a' })
    cb.push({ timestamp: 2, name: 'b' })

    const s1 = cb.snapshot(sortByTS).toArray()
    expect(s1).toEqual([
      { timestamp: 1, name: 'a', key: 0 },
      { timestamp: 2, name: 'b', key: 1 }
    ])

    const s2 = cb.snapshot(invertSorter(sortByTS)).toArray()
    expect(s2).toEqual([
      { timestamp: 2, name: 'b', key: 1 },
      { timestamp: 1, name: 'a', key: 0 }
    ])

  })
})