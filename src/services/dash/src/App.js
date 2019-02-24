import React from 'react';
import moment from 'moment'
import classNames from 'classnames'
import CircularBuffer from './lib/circularBuffer'
import subscribe from './lib/subscribe'
import sse from './lib/sse'
import { Scheduler, BehaviorSubject, interval, of, from } from 'rxjs'
import { scan, map, auditTime, combineLatest, startWith, retryWhen, delay, concatMap } from 'rxjs/operators'
import "./App.css"

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
  scan((acc, x, i) => acc.push(x), new CircularBuffer(2048)),
  auditTime(0, Scheduler.animationFrameScheduler), // https://rxjs-dev.firebaseapp.com/api/operators/auditTime
  map(xs => xs.snapshot())
)

const MessageTableInner = (props) => {
  if (props.messages.isLoading) {
    return "Loading..."
  }

  const rows = props.messages.value.map((message, i) =>
    <tr className={classNames(`level-${message.level}`)} key={`message-${i}`}>
      <td className='level'>{message.level}</td>
      <td className='message' title={message.value}>{message.message}</td>
      <td className='name'>{message.name}</td>
      <td className='timestamp' title={message.timestamp}>{moment(message.timestamp).fromNow()}</td>
    </tr>)

  return <table data-age={props.age.value}>
    <thead>
      <tr>
        <th className='level'>Level</th>
        <th className='message'>Message</th>
        <th className='name'>Name</th>
        <th className='timestamp'>Timestamp</th>
      </tr>
    </thead>
    <tbody>
      {rows}
    </tbody>
  </table>
}

const FilterInput = ({ filter, setFilter }) =>
  <input
    id="filter"
    value={filter}
    onChange={evt => setFilter(evt.currentTarget.value)}
    placeholder="Filter logs..."
    />

const filter$ = new BehaviorSubject('')
const Filter = subscribe({ filter$ }, { setFilter: x => filter$.next(x) })(FilterInput)
const age = interval(1000).pipe(startWith(0))

const messages = allLogs$.pipe(
  combineLatest(filter$),
  map(([ xs, filter ]) =>
    xs.filter(x => x.level === filter || x.message.indexOf(filter) !== -1 || x.name.indexOf(filter) !== -1)
  )
)
const MessageTable = subscribe({ messages, age })(MessageTableInner)

const App = () => {
  return <React.Fragment>
    <div id="header">
      <h1>Logary Dash</h1>
      <Filter />
    </div>
    <MessageTable />
  </React.Fragment>
}

export default App;

//import Gun from 'gun/gun'
//var gun = Gun(['http://localhost:8765/gun']);
//gun.get('logs').set({ "title": "Hello gun world"})