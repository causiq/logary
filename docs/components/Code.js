const Code = ({ lang, children }) =>
  <pre>
    <code className={`language-${lang}`}>
      {children}
    </code>
  </pre>

const BashCode = ({ children }) =>
  <Code lang="bash">
    {children}
  </Code>

export { BashCode }

export default Code