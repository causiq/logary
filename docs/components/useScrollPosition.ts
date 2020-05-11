import React, { useRef, RefObject } from 'react'

export type ScrollPosition = Omit<DOMRect, "toJSON"> & Readonly<{
  window: Readonly<{
    scrollX: number;
    scrollY: number;
  }>
}>

function getScrollPosition(element?: RefObject<HTMLElement> | null): ScrollPosition {
  if (typeof window === 'undefined') {
    return {
      bottom: 0,
      height: 0,
      left: 0,
      right: 0,
      top: 0,
      width: 0,
      x: 0,
      y: 0,
      window: {
        scrollX: 0,
        scrollY: 0
      }
    }
  }

  const target = element != null && element.current != null
    ? element.current
    : window.document.body

  const position = target.getBoundingClientRect()

  return {
    ...position,
    // https://stackoverflow.com/a/38515267
    window: {
      scrollX: window.scrollX || window.pageXOffset,
      scrollY: window.scrollY || window.pageYOffset
    }
  }
}

export type PositionInfo = Readonly<{
  previous: ScrollPosition | null;
  current: ScrollPosition;
}>

const useLayoutEffect = typeof window === 'undefined' ? React.useEffect : React.useLayoutEffect

/**
 *
 * @param effect Callback, called when the passed element (or window.body otherwise) has its scroll position updated
 * @param element Optional element ref
 * @param deps Dependencies to recalculate the scroll effect
 */
export function useScrollPosition(
  effect: (position: PositionInfo) => void,
  element: RefObject<HTMLElement> | null | undefined,
  deps?: any[],
) {
  const position = useRef(getScrollPosition(element))

  useLayoutEffect(() => {
    const handleScroll = () => {
      const current = getScrollPosition(element)
      effect({ previous: position.current, current })
      position.current = current
    }
    window.addEventListener('scroll', handleScroll)
    return () => window.removeEventListener('scroll', handleScroll)
  }, deps != null ? [ ...deps, position, element ] : [ position, element ])
}