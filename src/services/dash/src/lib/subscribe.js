import React from 'react';

/**
 * Subscribe to all of an object's observables { [name: string]: Observable } structure.
 */
const subscribe = (observables, innerProps) => Component => {
  return class extends React.Component {
    constructor(props) {
      super(props)
      this.state =
        Object.keys(observables)
          .reduce((acc, k) => ({ ...acc, [`${k}`]: { isLoading: true } }), {})
    }
    componentDidMount() {
      this.subscriptions = []
      Object.keys(observables).forEach(k => {
        this.subscriptions.push(observables[k].subscribe(value => {
          this.setState({
            ...this.state,
            [`${k}`]: {
              value,
              isLoading: false
            }
          })
        }))
      })
    }
    componentWillUnmount() {
      this.subscriptions.forEach(sub => {
        sub.unsubscribe();
      })
    }
    render() {
      return <Component {...this.props} {...innerProps} {...this.state} />
    }
  }
}

export default subscribe;