// eslint-disable-next-line
import React from 'react';
import moment from 'moment'
import CircularBuffer from './lib/circularBuffer'
import subscribe from './lib/subscribe'
import sse from './lib/sse'
import { Scheduler, BehaviorSubject, interval, of, from } from 'rxjs'
import { scan, map, auditTime, combineLatest, startWith, retryWhen, delay, concatMap } from 'rxjs/operators'

/** @jsx jsx */
import { Global, jsx } from '@emotion/core'
import styled, { css } from '@emotion/primitives'
import { ThemeProvider } from 'emotion-theming'

////////////////////////// DATA //////////////////////

// Nice: https://usehooks.com/

function template(value, fields = {}, context = {}) {
  return value.replace(/\{(\w+)\}/g, (match, key, y, z) => {
    // console.log('match=', match,'key=',key,'y=',y,'z=',z)
    return fields[key] || context[key] || match
  })
}

const interpret = input => {
  return Array.isArray(input)
    ? from(input.map(x => ({ ...x, message: template(x.value, x.fields, x.context) })))
    : of({ ...input, message: template(input.value, input.fields, input.context) })
}

const allLogs$ = sse("http://localhost:8080/logs").pipe(
  retryWhen(e => e.pipe(delay(1000))),
  map(JSON.parse),
  concatMap(interpret),
  scan((acc, x) => acc.push(x), new CircularBuffer(2048)),
  auditTime(0, Scheduler.animationFrameScheduler), // https://rxjs-dev.firebaseapp.com/api/operators/auditTime
  map(xs => xs.snapshot().snapshot)
)

const filter$ = new BehaviorSubject('')
const age = interval(1000).pipe(startWith(0))

const messages = allLogs$.pipe(
  combineLatest(filter$),
  map(([ xs, filter ]) =>
    xs.filter(x => x.level === filter || x.message.indexOf(filter) !== -1 || x.name.indexOf(filter) !== -1)
  )
)

////////////////////////// VIEWS //////////////////////

const theme = {
  bg: '#302F34',  // dark black blue
  bg2: '#242424', // dark grey
  text: '#D5E9FA', // white blue
  highlight: '#306DAC', // matte dark blue
  highlightText: '#D9EDFF', // whiter blue
  info: '#1B5A75',
  infoFocus: '#2E9AC9', // carrebean ocean blue
  warning: '#DB8C1F', // organge
  warningFocus: '#DB8C1F', // brighter orange
  error: '#CC5A54', // red
  errorFocus: '#DE625B' // brighter red
}

const FilterInput = ({ filter, setFilter }) =>
  <input
    id="filter"
    css={css({
      padding: '0.6em 0',
      flex: 1
    })}
    value={filter}
    onChange={evt => setFilter(evt.currentTarget.value)}
    placeholder="Filter logs..."
    />

const Filter = subscribe({ filter$ }, { setFilter: x => filter$.next(x) })(FilterInput)

/**
 * Wrappers around cells and other wrappers: flex, row, wrap
 */
const wrapper = css({
  display: 'flex',
  flexDirection: 'row',
  flexGrow: 0,
  flexWrap: 'wrap'
})

/**
 * Text based cells: truncate and show ellipsis when there is not enough space
 */
const text = css({
  overflow: 'hidden',
  whiteSpace: 'nowrap',
  paddingRight: '20px',
})

const longLine = css({
  width: '180px',
  flexGrow: 20
})

const shortLine = css({
  width: '100px',
  flexGrow: 10,
  textOverflow: 'ellipsis',
  maxWidth: '160px'
})

const tinyLine = css({
  width: '50px',
  flexGrow: 1
})

const header = css({
  textAlign: 'left',
  textTransform: 'uppercase',
  backgroundColor: theme.bg2,
  fontWeight: 'bold'
})

const Cell = styled.Text({
  ...text,
  padding: '0.8em',
})

/**
 * Main container: initialize the flex, direction is row
 */
const Row = styled.View`
  display: flex;
  flex-direction: row;
  flex-grow: 0;
  width: 100%;
`
// &:hover {
//   color: black;
// }

const Table = styled.View({})

// https://codepen.io/Orangetronic/pen/pJgpXw
// https://scotch.io/tutorials/a-visual-guide-to-css3-flexbox-properties#toc-flex-grow
// https://hashnode.com/post/really-responsive-tables-using-css3-flexbox-cijzbxd8n00pwvm53sl4l42cx
// https://codepen.io/vasansr/pen/VerOPy?editors=1100
const MessageTableInner = ({
  messages,
  age,
  ...rest
}) => {
  if (messages.isLoading) {
    return "Loading..."
  }

  const rows = messages.value.map((message, i) => {
    const colourised = css({
      backgroundColor: theme[message.level] || 'inherit'
    })
    return <Row key={`message-${i}`}>
      <Cell css={[ tinyLine, colourised ]} data-name='level'>{message.level}</Cell>
      <Cell css={[ longLine ]} data-name='message' title={message.value}>{message.message}</Cell>
      <Cell css={[ shortLine ]} data-name='name'>{message.name}</Cell>
      <Cell css={[ shortLine ]} data-name='timestamp' title={message.timestamp}>{moment(message.timestamp).fromNow()}</Cell>
    </Row>})

  return <Table data-age={age.value} css={css({
    '> *:nth-child(2n+1)': {
      backgroundColor: theme.bg2
    },
  })} {...rest}>
    <Row css={[ wrapper, header ]}>
      <Cell css={[ tinyLine ]} className='level'>Level</Cell>
      <Cell css={[ longLine ]} className='message'>Message</Cell>
      <Cell css={[ shortLine ]} className='name'>Name</Cell>
      <Cell css={[ shortLine ]} className='timestamp'>TS</Cell>
    </Row>
    {rows}
  </Table>
}

const MessageTable = subscribe({ messages, age })(MessageTableInner)

const globalStyles = css({
  body: {
    backgroundColor: theme.bg,
    minHeight: '100vh',
    fontSize: '10px',
    color: theme.text,
    minWidth: '350px',
    margin: 0,
    padding: 0,
    fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", "Roboto", "Oxygen", "Ubuntu", "Cantarell", "Fira Sans", "Droid Sans", "Helvetica Neue", sans-serif',
    WebkitFontSmoothing: 'antialiased',
    MozOsxFontSmoothing: 'grayscale'
  },
  '#root': {
    fontSize: '130%',
    paddingTop: '50px'
  },
  '::selection': {
    backgroundColor: theme.warning,
    color: theme.highlightText,
  },
  'div[title]': {
    textDecoration: '1px dotted white'
  },

})

const H1 = styled.Text`
  margin: 0 14px;
  padding: 0;
  font-size: 200%;
  font-weight: bold
`

const Header = styled.View({
  display: 'flex',
  flexDirection: 'row',
  alignItems: 'center',
  padding: '1vmin',
  height: '50px',
  width: '100%',
  position: 'fixed',
  top: 0,
  left: 0,
  backgroundColor: theme.bg,
  zIndex: 200
})

const App = () => {
  return <ThemeProvider theme={theme}>
    <Global styles={globalStyles} />
    <Header id="header">
      <H1>Logary Dash</H1>
      <Filter />
    </Header>
    <MessageTable css={css({ width: '100%' })} />
  </ThemeProvider>
}

export default App;

// https://medium.com/@jonnykalambay/your-first-hybrid-app-in-15-minutes-react-native-on-the-web-2cc2646051e
// https://github.com/jonnyk20/hybrid-app-pokedex

//import Gun from 'gun/gun'
//var gun = Gun(['http://localhost:8765/gun']);
//gun.get('logs').set({ "title": "Hello gun world"})