import preval from 'babel-plugin-preval/macro'

export const DotNet =
  preval`
    const fs = require('fs'), path = require('path'), basePath = __dirname + '/../examples/dotnet';
    module.exports = fs
      .readdirSync(basePath)
      .map(file => [ file, fs.readFileSync(path.join(basePath, file), 'utf8') ])
      .reduce((acc, item) => {
        acc[item[0]] = item[1];
        return acc;
      }, {})
  `

export const Targets =
  preval`
    const fs = require('fs'), path = require('path'), basePath = __dirname + '/../examples/targets';
    module.exports = fs
      .readdirSync(basePath)
      .map(file => [ file, fs.readFileSync(path.join(basePath, file), 'utf8') ])
      .reduce((acc, item) => {
        acc[item[0]] = item[1];
        return acc;
      }, {})
  `