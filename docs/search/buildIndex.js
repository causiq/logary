import { Parser } from 'htmlparser2';

export const process = (iid, href, input) => {
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
  // https://astexplorer.net/#/2AmVrGuGVJ
  const parser = new Parser({
    onopentag: function (name, attrs) {
      previous = current
      const x = { name, attrs }
      current = x
      element.push(x);
    },

    onclosetag: function (name) {
      const expected = current().name
      if (name !== expected) throw new Error("Unclosed element tag: " + expected)
      element.pop(); // |> ignore
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

export default process