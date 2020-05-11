import { Parser } from 'htmlparser2';

export const processPage = (iid, href, input) => {
  const document = {
    id: iid,
    title: '',
    subtitle: [],
    description: '',
    body: '',
    href
  }
  const bodies = [];
  const element = [];
  let current = null;
  const getCurrent = () => element[element.length - 1]

  // https://astexplorer.net/#/2AmVrGuGVJ
  const parser = new Parser({
    onopentag: function (name, attrs) {
      //console.log('opentag', name)
      const x = { name, attrs }
      current = x
      element.push(x);
    },

    onclosetag: function (name) {
      //console.log('closetag', name)
      const expected = current.name
      if (name !== expected) throw new Error(`Unclosed element tag: ${expected} for document\n${input}`)
      element.pop();
      current = getCurrent()
    },

    ontext: function (text) {
      switch (current.name) {
        case "h1":
          if (current.attrs.class != null && current.attrs.class.indexOf("title") !== -1) {
            document.title = text
          }
          break;

        case "h2":
        case "h3":
          document.subtitle.push(text)
          break;

        case "h4":
        case "h5":
        case "h6":
          break;

        default:
          bodies.push(text)
      }
    }
  }, { decodeEntities: true });
  parser.write(input);
  parser.end();

  document.body = bodies.join(' ')
  return document;
}

import axios from "axios";
const sleep = durationMS =>
  new Promise(resolve => {
    const tag = setTimeout(() => {
      clearTimeout(tag)
      resolve(null)
    }, durationMS)
  })

axios.interceptors.response.use(null, async error => {
  if (error.code === "ECONNREFUSED") {
    await sleep(1000)
    return await axios.request(error.config)
  }
  return Promise.reject(error);
});

export const getPage = async (page, apiBase= 'http://127.0.0.1:3000') => {
  if (page == null || page.length === 0) throw new Error("Expected page to have a value")
  const url = `${apiBase}/${page.substr(1)}`
  try {
    const res = await axios(url)
    return res.data;
  } catch (e) {
    console.error(`Failed to get url='${url}'.`)
    console.error(e)
  }
}

// script portion
import fs from 'fs'
import path from 'path'

async function execute() {
  const pages = [
    '/',
    '/dotnet/docs',
    '/dotnet/prometheus',
    '/dotnet/quickstart',
    '/dotnet/targets',
    '/js/docs',
    '/js/quickstart',
    '/other/faqs',
    '/other/license',
    '/other/pricing',
    '/other/tutorial',
    '/rutta',
    '/tutorials/how',
    '/tutorials/visualise',
  ]

  let i = 0, results = {};
  for (const page of pages) {
    const raw = await getPage(page)
    const processed = await processPage(String(++i), page, raw)
    results[String(i)] = processed
  }

  const filePath = path.resolve(__dirname, 'pages.json')
  console.log('Writing results to ', filePath)
  await new Promise(resolve => fs.writeFile(filePath, JSON.stringify(results), 'utf8', resolve))
}

execute()