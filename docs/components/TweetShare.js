import React from 'react';
import PropTypes from 'prop-types'
import addScript from './addScript'

class TweetShare extends React.Component {
  componentDidMount () {
    const options = {
      text: this.props.text,
      url: this.props.url,
      hashtags: this.props.hashtags,
      via: this.props.via,
      related: this.props.related,
      size: this.props.size,
      lang: this.props.lang,
      dnt: this.props.dnt || true
    }

    const renderTweet = () => {
      window.twttr.widgets.createShareButton(this.props.url, this._div, options)
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

// https://developer.twitter.com/en/docs/twitter-for-websites/tweet-button/guides/parameter-reference1
// https://developer.twitter.com/en/docs/twitter-for-websites/tweet-button/guides/javascript-factory-function
TweetShare.propTypes = {
  text: PropTypes.string,
  url: PropTypes.string,
  via: PropTypes.string,
  related: PropTypes.string,
  hashtags: PropTypes.string,
  options: PropTypes.object,
  size: PropTypes.string,
  lang: PropTypes.string,
  dnt: PropTypes.string
};

export default TweetShare;