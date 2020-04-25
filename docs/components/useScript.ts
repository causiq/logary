// https://usehooks.com/useScript/
import { useState, useEffect } from 'react'

const cachedScripts: string[] = []

/**
 * error === false iff loaded === false,
 * error || !error otherwise
 */
export type ScriptHookState = {
  loaded: boolean;
  error: boolean;
}

export function loadScript(src: string, setState: (state: ScriptHookState) => void) {
  if (typeof window === 'undefined') return;

  // If cachedScripts array already includes src that means another instance ...
  // ... of this hook already loaded this script, so no need to load again.
  if (cachedScripts.includes(src)) {
    setState({
      loaded: true,
      error: false
    });
  } else {
    cachedScripts.push(src);

    // Create script
    let script = document.createElement('script')
    script.src = src
    script.async = true
    script.crossOrigin = 'anonymous'

    // Script event listener callbacks for load and error
    const onScriptLoad = () => {
      setState({
        loaded: true,
        error: false
      });
    };

    const onScriptError = () => {
      // Remove from cachedScripts we can try loading again
      const index = cachedScripts.indexOf(src)

      if (index >= 0) cachedScripts.splice(index, 1)
      script.remove();

      setState({
        loaded: true,
        error: true
      });
    }

    script.addEventListener('load', onScriptLoad)
    script.addEventListener('error', onScriptError);

    document.body.appendChild(script)

    return () => {
      script.removeEventListener('load', onScriptLoad)
      script.removeEventListener('error', onScriptError)
    }
  }
}

export default function useScript<T extends {}>(src: string, skip: boolean = false, onLoad?: () => T) {
  // Keeping track of script loaded and error state
  const [state, setState] = useState<ScriptHookState & { current: T | null }>({
    loaded: false,
    error: false,
    current: null
  })

  useEffect(() => {
    function setStateInner(state: ScriptHookState) {
      if (state.loaded && !state.error && onLoad) {
        setState({
          ...state,
          current: onLoad()
        })
      } else {
        setState({
          ...state,
          current: null
        })
      }
    }
    return skip ? () => {} : loadScript(src, setStateInner)
  // Because we only ever want to load it once, but onLoad
  // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [skip, src])

  return state
}
