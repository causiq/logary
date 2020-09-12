import { ConsoleTarget, RuttaTarget, getLogary, LogLevel } from 'logary'
import browser from '@logary/plugin-browser'
import nextjs from '@logary/plugin-nextjs'
import react from '@logary/plugin-react'

const instance = getLogary({
  minLevel: LogLevel.debug,
  serviceName: 'logary-docs',
  targets: [
    new ConsoleTarget(),
    new RuttaTarget({
      endpoint: 'https://i.logary.tech',
      disabled: typeof window === 'undefined'
    })
  ]
})

browser(instance, { debugHandler: false })
nextjs(instance)
react(instance)

export default instance