import React, { useState, useEffect } from 'react';
import classNames from 'classnames'
import CircularBuffer from './lib/circularBuffer'
import sse from './lib/sse'
import { Scheduler, BehaviorSubject } from 'rxjs';
import { scan, map, auditTime, combineLatest } from 'rxjs/operators'
import "./App.css"

/**
 * Subscribe to all of an object's observables { [name: string]: Observable } structure.
 */
const subscribe = (observables, props) => {
  const initialState =
    Object.keys(observables)
          .reduce((acc, k) => ({ ...acc, [`${k}`]: { isLoading: true } }), {})
  return Component => outerProps => {
    const [ state, setState ] = useState(initialState)
    useEffect(() => {
      const subscriptions = []
      Object.keys(observables).forEach(k => {
        subscriptions.push(observables[k].subscribe(value => {
          setState({
            ...state,
            [`${k}`]: {
              value,
              isLoading: false
            }
          })
        }))
      })
      return () => {
        subscriptions.forEach(sub => {
          sub.unsubscribe();
        })
      }
    }, [ observables ])
    return <Component {...props} {...state} {...outerProps} />
  }
}

function template(value, fields = {}, context = {}) {
  return value.replace(/\{(\w+)\}/g, (match, key, y, z) => {
    // console.log('match=', match,'key=',key,'y=',y,'z=',z)
    return fields[key] || context[key] || match
  })
}

const allLogs$ = sse("http://localhost:8765/logs").pipe(
  map(JSON.parse),
  map(m => ({ ...m, message: template(m.value, m.fields, m.context) })),
  scan((acc, x, i) => acc.push(x), new CircularBuffer(2048)),
  auditTime(0, Scheduler.animationFrameScheduler), // https://rxjs-dev.firebaseapp.com/api/operators/auditTime
  map(xs => xs.snapshot())
)

const MessageTableInner = (props) => {
  if (props.message$.isLoading) {
    return "Loading..."
  }

  const rows = props.message$.value.map((message, i) =>
    <tr className={classNames(`level-${message.level}`)} key={`message-${i}`}>
      <td className='level'>{message.level}</td>
      <td className='message' title={message.value}>{message.message}</td>
      <td className='name'>{message.name}</td>
    </tr>)

  return <table>
    <thead>
      <tr>
        <th className='level'>Level</th>
        <th className='message'>Message</th>
        <th className='name'>Name</th>
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

const message$ = allLogs$.pipe(
  combineLatest(filter$),
  map(([ xs, filter ]) => xs.filter(x => x.message.indexOf(filter) !== -1))
)
const MessageTable = subscribe({ message$ })(MessageTableInner)

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