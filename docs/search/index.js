import lunr from 'lunr';
import pages from './pages.json'

// https://lunrjs.com/docs/index.html
// https://lunrjs.com/docs/lunr.Builder.html
// see ./buildIndex.js
// https://github.com/angeloashmore/react-lunr
export const index = lunr(function () {
  this.field('title', { boost: 3 })
  this.field('subtitle', { boost: 2 })
  this.field('description', { boost: 2 })
  this.field('body')
  // this.field('href')
  Object.keys(pages).map(k => pages[k]).forEach(this.add.bind(this));
})

export { pages as store }
export default index