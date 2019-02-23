import { Observable } from 'rxjs'

const sse = uri => Observable.create(o => {
  const handleMessage = event => o.next(event.data)
  const handleError = error => o.error(error)
  const sse = new EventSource(uri)
  sse.addEventListener('message', handleMessage)
  sse.addEventListener('error', handleError)
  return () => {
    sse.removeEventListener('message', handleMessage);
    sse.removeEventListener('error', handleError)
    sse.close()
  }
})

export default sse