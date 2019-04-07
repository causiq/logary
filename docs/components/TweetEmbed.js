import React from 'react';
import PropTypes from 'prop-types'
import addScript from './addScript'

class TweetEmbed extends React.Component {
  componentDidMount () {
    const options = this.props.options || {};

    const renderTweet = () => {
      window.twttr.widgets.createTweetEmbed(this.props.id, this._div, options)
    }
    if (!window.twttr) {
      addScript('//platform.twitter.com/widgets.js', renderTweet)
    } else {
      renderTweet()
    }
  }
  render () {
    return <div ref={(c) => this._div = c} />
  }
}

TweetEmbed.propTypes = {
  id: PropTypes.string,
  options: PropTypes.object,
};

export default TweetEmbed;