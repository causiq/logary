import { useState, RefObject } from 'react'
import { useScrollPosition, ScrollPosition } from './useScrollPosition'

interface HasRef {
  id: string;
  ref: RefObject<HTMLElement>
}

const closestAbove = <T extends HasRef>(items: T[], _: ScrollPosition): T | null => {
  let found: T | null = null

  for (let i = 0; i < items.length; i++) {
    const item = items[i]
    const rect = item.ref.current?.getBoundingClientRect()

    if (rect == null) continue // is not rendered right now
    if (rect.top >= 10) continue // this ToC item hasn't scrolled past yet

    // @ts-ignore ?????
    const prevRect: DOMRect | undefined = found?.ref.current?.getBoundingClientRect()

    found = prevRect != null && Math.abs(prevRect.top) <= Math.abs(rect.top)
      ? found
      : item
  }

  return found
}

export default function useSectionTracking<TSection extends HasRef>(
  items: TSection[],
  currentChanged?: (section: TSection | null) => void,
  deps?: any[]
) {
  const [bestMatch, setBestMatch] = useState<HasRef | null>(null)

  useScrollPosition(({ current }) => {
    const found = closestAbove(items, current)
    if (found?.id === bestMatch?.id) return
    if (currentChanged != null) currentChanged(found)
    setBestMatch(found)
  }, null, deps != null ? [...deps, items, currentChanged] : [items, currentChanged])

  return bestMatch
}