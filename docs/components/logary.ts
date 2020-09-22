import { ConsoleTarget, RuttaTarget, getLogary, LogLevel } from 'logary'
import browser from '@logary/plugin-browser'
import nextjs from '@logary/plugin-nextjs'
import react from '@logary/plugin-react'

const instance = getLogary({
  minLevel: LogLevel.debug,
  serviceName: 'logary-docs',
  targets: [
    new ConsoleTarget(),
    new RuttaTarget({ endpoint: 'https://i.logary.tech', })
  ],
  appId: 'LA-14396174'
})

browser(instance)
nextjs(instance)
react(instance)

export default instance